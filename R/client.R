# The client is a list of closures over one state, so a call reads `client$lookup(ip)` as every other
# VPNDetection SDK names it. Each call checks its options with client_options() before anything
# else can answer it, then makes its request with call_api(), or call_api_many() for several at once.

vpndetection_client <- function(api_key = NULL, base_url = NULL, timeout = 30, retries = 2,
                                concurrency = 8, cache_size = 10000, cache_ttl = 3600) {
    method <- "vpndetection_client"
    check_timeout(timeout, method)
    check_count(retries, "retries", method, 0)
    check_count(concurrency, "concurrency", method, 1)
    check_count(cache_size, "cache_size", method, 0)
    if (!is.numeric(cache_ttl) || length(cache_ttl) != 1 || is.na(cache_ttl) || cache_ttl < 0) {
        refuse(paste0(method, ": cache_ttl must be a number of seconds, 0 or more"))
    }
    state <- new.env(parent = emptyenv())
    state$api_key <- clean_key(api_key)
    state$base_url <- clean_base_url(base_url)
    state$timeout <- as.numeric(timeout)
    state$retries <- as.integer(retries)
    state$concurrency <- as.integer(concurrency)
    state$cache <- new_cache(as.integer(cache_size), as.numeric(cache_ttl))

    client <- list(
        lookup = function(ip, timeout = NULL, retries = NULL) {
            opts <- client_options(state, "lookup", timeout, retries)
            check_ip(ip, "lookup")
            address <- unmap_ip(ip)
            if (is_bogon(address)) {
                return(bogon_result(address))
            }
            cached <- state$cache$get(address)
            if (!is.null(cached)) {
                return(cached)
            }
            result <- call_api(state, paste0("/", curl::curl_escape(address)), opts$timeout,
                opts$retries, read_as("LookupResponse"))
            state$cache$set(address, result)
            result
        },
        my_ip = function(timeout = NULL, retries = NULL) {
            opts <- client_options(state, "my_ip", timeout, retries)
            call_api(state, "/myip", opts$timeout, opts$retries, read_as("LookupResponse"))
        },
        my_entitlement = function(timeout = NULL, retries = NULL) {
            opts <- client_options(state, "my_entitlement", timeout, retries)
            call_api(state, "/api/v1/entitlement", opts$timeout, opts$retries,
                read_as("Entitlement"))
        },
        lookup_batch = function(ips, concurrency = NULL, timeout = NULL, retries = NULL) {
            opts <- client_options(state, "lookup_batch", timeout, retries, concurrency)
            if (!is.character(ips) || anyNA(ips)) {
                refuse("lookup_batch: ips must be a character vector with no NA")
            }
            batch(state, ips, opts)
        },
        clear_cache = function() {
            state$cache$clear()
            invisible(NULL)
        }
    )
    structure(client, class = "vpndetection_client", state = state)
}

print.vpndetection_client <- function(x, ...) {
    state <- attr(x, "state")
    cat("<vpndetection_client> ", state$base_url,
        if (is.null(state$api_key)) " (no API key)" else " (with an API key)", "\n", sep = "")
    invisible(x)
}

# A call's own timeout, retries and concurrency, checked before a bogon, a cache hit or anything else
# could answer it without a request, and the client's where the call names none.
client_options <- function(state, method, timeout, retries, concurrency = NULL) {
    if (!is.null(timeout)) check_timeout(timeout, method)
    if (!is.null(retries)) check_count(retries, "retries", method, 0)
    if (!is.null(concurrency)) check_count(concurrency, "concurrency", method, 1)
    list(
        timeout = if (is.null(timeout)) state$timeout else as.numeric(timeout),
        retries = if (is.null(retries)) state$retries else as.integer(retries),
        concurrency = if (is.null(concurrency)) state$concurrency else as.integer(concurrency)
    )
}

clean_key <- function(api_key) {
    if (is.null(api_key)) {
        return(NULL)
    }
    if (!is.character(api_key) || length(api_key) != 1 || is.na(api_key)) {
        refuse("vpndetection_client: api_key must be one string")
    }
    key <- trimws(api_key, whitespace = "[\\h\\v]")
    if (!nzchar(key)) {
        return(NULL)
    }
    if (grepl("[\\x00-\\x1f\\x7f]", key, perl = TRUE)) {
        refuse("vpndetection_client: api_key holds a control character")
    }
    key
}

clean_base_url <- function(base_url) {
    if (is.null(base_url)) {
        return("https://api.vpndetection.io")
    }
    if (!is.character(base_url) || length(base_url) != 1 || is.na(base_url) || !nzchar(base_url)) {
        refuse("vpndetection_client: base_url must be one non-empty string")
    }
    sub("/+$", "", base_url)
}

# A timeout no attempt can meet is refused where it is set: negative, NaN, infinite, not a number,
# or longer than curl's millisecond bound holds. 0 means no bound, as it does to curl.
check_timeout <- function(timeout, method) {
    if (!is.numeric(timeout) || length(timeout) != 1 || is.na(timeout) || timeout < 0 ||
        timeout > longest_wait) {
        refuse(paste0(method, ": timeout must be a number of seconds from 0 to ", longest_wait))
    }
}

check_count <- function(n, name, method, floor) {
    if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < floor || n != trunc(n) ||
        n > .Machine$integer.max) {
        refuse(sprintf("%s: %s must be a whole number, %d or more", method, name, floor))
    }
}

batch_chunk <- 1000

batch <- function(state, ips, opts) {
    keys <- unique(ips)
    results <- stats::setNames(vector("list", length(keys)), keys)
    send_for <- character(0)
    for (key in keys) {
        address <- unmap_ip(key)
        if (is_bogon(address)) {
            results[[key]] <- bogon_result(address)
            next
        }
        cached <- state$cache$get(address)
        if (!is.null(cached)) {
            results[[key]] <- cached
            next
        }
        send_for[[key]] <- address
    }
    to_send <- unique(unname(send_for))
    if (length(to_send) == 0) {
        return(results)
    }
    chunks <- split(to_send, ceiling(seq_along(to_send) / batch_chunk))
    requests <- lapply(chunks, function(chunk) list(
        path = "/batch",
        body = jsonlite::toJSON(list(ips = I(chunk)), auto_unbox = TRUE)
    ))
    outcomes <- call_api_many(state, requests, opts$timeout, opts$retries, opts$concurrency,
        read_batch)
    answers <- list()
    for (i in seq_along(chunks)) {
        outcome <- outcomes[[i]]
        for (address in chunks[[i]]) {
            answers[[address]] <- if (!is.null(outcome$error)) outcome$error
                else batch_entry(outcome$value, address, state)
        }
    }
    for (key in names(send_for)) {
        results[[key]] <- answers[[send_for[[key]]]]
    }
    results
}

# The chunk's envelope must hold both maps; an entry inside it that cannot be read is that address's
# error alone, so it is read per address by batch_entry.
read_batch <- function(json) {
    if (!is_object(json) || !is.list(json$results) || !is.list(json$errors) ||
        is_array(json$results) && length(json$results) > 0 ||
        is_array(json$errors) && length(json$errors) > 0) {
        unreadable("a batch answer holds neither results nor errors as objects")
    }
    json
}

batch_entry <- function(json, address, state) {
    if (address %in% names(json$results)) {
        entry <- json$results[[address]]
        problem <- schema_problem(entry, "LookupResponse")
        if (!is.null(problem)) {
            return(vpnd_error("server_error",
                paste("the API answered with an unreadable body:", problem), 200))
        }
        state$cache$set(address, entry)
        return(entry)
    }
    if (address %in% names(json$errors)) {
        entry <- json$errors[[address]]
        problem <- schema_problem(entry, "BatchLookupError")
        if (!is.null(problem)) {
            return(vpnd_error("server_error",
                paste("the API answered with an unreadable body:", problem), 200))
        }
        # Classified by its status alone: an entry has no headers, so its 429 is a spent quota.
        status <- as.integer(entry$status)
        kind <- if (status == 429) "quota_exceeded" else error_from_response(status, NULL, "")$kind
        return(vpnd_error(kind, entry$error, status))
    }
    vpnd_error("server_error", "the API's batch answer left this address out", 200)
}

flags <- c("is_vpn", "is_hosting", "is_relay", "is_tor", "is_cdn", "is_resproxy", "is_dcproxy",
    "is_mobproxy", "is_pubproxy")
details <- c("vpn", "hosting", "relay", "tor", "cdn", "resproxy", "dcproxy", "mobproxy", "pubproxy")

# A reserved address is answered here, never sent: the widest shape, every flag false, every detail
# empty, location and asn with every key empty, as the API answers a bogon.
bogon_result <- function(ip) {
    empty <- stats::setNames(list(), character(0))
    location_keys <- c("continent", "continent_name", "country", "country_name", "timezone",
        "currency_code", "currency_name", "currency_symbol", "flag_emoji", "flag_unicode")
    c(
        list(ip = ip),
        stats::setNames(as.list(rep(FALSE, length(flags))), flags),
        stats::setNames(rep(list(empty), length(details)), details),
        list(
            location = stats::setNames(as.list(rep("", length(location_keys))), location_keys),
            asn = list(asn = 0L, name = "", domain = "", type = "", prefix = ""),
            is_bogon = TRUE
        )
    )
}

check_ip <- function(ip, method) {
    if (!is.character(ip) || length(ip) != 1 || is.na(ip)) {
        refuse(paste0(method, ": ip must be one string"))
    }
}
