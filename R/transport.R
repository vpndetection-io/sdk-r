# One request is one curl handle: no redirect followed, the SDK's User-Agent, the key as a Bearer
# header and nowhere else, and a bound on the whole attempt, body included.

backoff_base <- 0.25
# The longest wait curl's millisecond options hold, and so the longest Retry-After slept as given.
longest_wait <- 2147483.647

sdk_version <- function() {
    as.character(utils::packageVersion("vpndetection"))
}

new_request <- function(state, path, timeout, body = NULL) {
    handle <- curl::new_handle()
    curl::handle_setopt(handle,
        followlocation = 0L,
        useragent = paste0("vpndetection-r/", sdk_version()),
        timeout_ms = as.integer(ceiling(timeout * 1000)),
        nosignal = 1L
    )
    headers <- list(Accept = "application/json")
    if (!is.null(state$api_key)) {
        headers$Authorization <- paste("Bearer", state$api_key)
    }
    if (!is.null(body)) {
        headers[["Content-Type"]] <- "application/json"
        curl::handle_setopt(handle, copypostfields = body)
    }
    do.call(curl::handle_setheaders, c(list(handle), headers))
    url <- paste0(state$base_url, path)
    curl::handle_setopt(handle, url = url)
    list(url = url, handle = handle)
}

# A finished response as list(value = ...) or list(error = <vpndetection_error>). Its status is
# read first, so a proxy's HTML 401 is a 401; only a 2xx is read as the API's answer, by `read`.
read_response <- function(res, read) {
    status <- res$status_code
    text <- tryCatch({
        txt <- rawToChar(res$content)
        Encoding(txt) <- "UTF-8"
        if (!validUTF8(txt)) NA_character_ else txt
    }, error = function(e) NA_character_)
    if (status < 200 || status > 299) {
        retry_after <- curl::parse_headers_list(res$headers)[["retry-after"]]
        return(list(error = error_from_response(status, retry_after, text)))
    }
    body <- if (is.na(text)) NULL else tryCatch(
        list(json = jsonlite::parse_json(text, simplifyVector = FALSE)),
        error = function(e) NULL
    )
    if (is.null(body)) {
        return(list(error = vpnd_error("server_error",
            "the API did not answer with JSON", status)))
    }
    tryCatch(list(value = read(body$json)), vpndetection_error = function(e) {
        e$status <- status
        list(error = e)
    })
}

# A 2xx that is not the answer the call returns is the API's fault, retried as any 5xx is.
unreadable <- function(problem) {
    stop(vpnd_error("server_error", paste("the API answered with an unreadable body:", problem)))
}

read_as <- function(schema) {
    function(json) {
        problem <- schema_problem(json, schema)
        if (!is.null(problem)) {
            unreadable(problem)
        }
        json
    }
}

# A call whose answer has no schema of its own in the spec: any JSON object.
read_object <- function(json) {
    if (!is_object(json) && !(is.list(json) && length(json) == 0 && !is.null(attr(json, "names")))) {
        unreadable("the answer is not a JSON object")
    }
    json
}

transport_error <- function(message) {
    vpnd_error("network", paste("the request failed:", gsub("\\s+", " ", trimws(message))))
}

retry_delay <- function(retry_after, tries) {
    if (!is.na(retry_after) && retry_after > 0 && retry_after <= longest_wait) {
        return(retry_after)
    }
    backoff_base * 2^min(tries, 6)
}

# A blocking call with its retries. Every attempt is bounded by `timeout`, so a retried call can
# take longer than it in total.
call_api <- function(state, path, timeout, retries, read, body = NULL) {
    tries <- 0
    repeat {
        req <- new_request(state, path, timeout, body)
        outcome <- tryCatch(
            read_response(curl::curl_fetch_memory(req$url, handle = req$handle), read),
            error = function(e) list(error = transport_error(conditionMessage(e)))
        )
        if (is.null(outcome$error)) {
            return(outcome$value)
        }
        if (tries >= retries || !outcome$error$retryable) {
            stop(outcome$error)
        }
        Sys.sleep(retry_delay(outcome$error$retry_after, tries))
        tries <- tries + 1
    }
}

# Requests run together on curl's multi interface, at most `concurrency` in flight, each retried
# as call_api retries. Returns one outcome per request, in order.
call_api_many <- function(state, requests, timeout, retries, concurrency, read) {
    n <- length(requests)
    outcomes <- vector("list", n)
    tries <- integer(n)
    pending <- seq_len(n)
    while (length(pending) > 0) {
        # multiplex = FALSE: with it on, curl holds every request to one host behind the first
        # to see whether that connection can multiplex, so over HTTP/1.1 they ran one at a time.
        pool <- curl::new_pool(total_con = concurrency, host_con = concurrency, multiplex = FALSE)
        queue <- pending
        in_flight <- 0
        start_next <- function() {
            while (in_flight < concurrency && length(queue) > 0) {
                i <- queue[1]
                queue <<- queue[-1]
                in_flight <<- in_flight + 1
                req <- new_request(state, requests[[i]]$path, timeout, requests[[i]]$body)
                curl::multi_add(req$handle, pool = pool,
                    done = local({ j <- i; function(res) finish(j, read_response(res, read)) }),
                    fail = local({ j <- i; function(msg) finish(j, list(error = transport_error(msg))) })
                )
            }
        }
        finish <- function(i, outcome) {
            outcomes[[i]] <<- outcome
            in_flight <<- in_flight - 1
            start_next()
        }
        start_next()
        curl::multi_run(pool = pool)
        retry <- Filter(function(i) {
            e <- outcomes[[i]]$error
            !is.null(e) && e$retryable && tries[i] < retries
        }, pending)
        if (length(retry) > 0) {
            Sys.sleep(max(vapply(retry, function(i) {
                retry_delay(outcomes[[i]]$error$retry_after, tries[i])
            }, numeric(1))))
            tries[retry] <- tries[retry] + 1L
        }
        pending <- retry
    }
    outcomes
}
