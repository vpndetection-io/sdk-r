# A local origin the tests script: POST /__script queues the answers it serves, in order, the last
# one repeated, and GET /__log reads back every request it received.
origin_app <- function() {
    app <- webfakes::new_app()
    app$locals$script <- list()
    app$locals$log <- list()
    app$use(function(req, res) {
        if (req$path == "/__script") {
            req$app$locals$script <- jsonlite::parse_json(rawToChar(req$.body), simplifyVector = FALSE)
            req$app$locals$log <- list()
            req$app$locals$now <- 0
            req$app$locals$peak <- 0
            return(res$set_status(204L)$send(""))
        }
        if (req$path == "/__peak") {
            return(res$set_status(200L)$set_type("application/json")$send(
                as.character(req$app$locals$peak)))
        }
        if (req$path == "/__log") {
            return(res$set_status(200L)$set_type("application/json")$send(
                jsonlite::toJSON(req$app$locals$log, auto_unbox = TRUE, null = "null")))
        }
        # A delayed answer comes back through this handler once its delay has passed, which
        # leaves the origin free to take other requests meanwhile.
        if (is.null(res$locals$n)) {
            req$app$locals$log[[length(req$app$locals$log) + 1]] <- list(
                method = req$method, path = req$path,
                headers = as.list(req$headers),
                body = if (length(req$.body)) rawToChar(req$.body) else ""
            )
            res$locals$n <- length(req$app$locals$log)
            req$app$locals$now <- req$app$locals$now + 1
            req$app$locals$peak <- max(req$app$locals$peak, req$app$locals$now)
        }
        script <- req$app$locals$script
        answer <- script[[min(res$locals$n, length(script))]]
        if (!is.null(answer$delay) && is.null(res$locals$waited)) {
            res$locals$waited <- TRUE
            return(res$delay(answer$delay))
        }
        req$app$locals$now <- req$app$locals$now - 1
        res$set_status(as.integer(answer$status))
        for (h in names(answer$headers)) res$set_header(h, answer$headers[[h]])
        if (is.null(answer$headers[["Content-Type"]])) res$set_type("application/json")
        if (isTRUE(answer$echo)) {
            # Answers a batch for whatever it was sent: each address in `fail` as that entry's
            # error, the rest as clean answers.
            ips <- unlist(jsonlite::parse_json(rawToChar(req$.body))$ips)
            answered <- setdiff(ips, names(answer$fail))
            answer$body <- list(
                results = stats::setNames(lapply(answered, function(ip) list(ip = ip, is_vpn = FALSE)), answered),
                errors = answer$fail[intersect(names(answer$fail), ips)]
            )
            for (m in c("results", "errors")) {
                if (length(answer$body[[m]]) == 0) answer$body[[m]] <- structure(list(), names = character(0))
            }
        }
        body <- if (!is.null(answer$raw)) answer$raw
            else if (!is.null(answer$body)) jsonlite::toJSON(answer$body, auto_unbox = TRUE,
                null = "null", digits = NA)
            else ""
        res$send(as.character(body))
    })
    app
}

origin <- webfakes::new_app_process(origin_app(), opts = webfakes::server_opts(num_threads = 12))

script <- function(...) {
    answers <- list(...)
    h <- curl::new_handle(copypostfields = jsonlite::toJSON(answers, auto_unbox = TRUE,
        null = "null", digits = NA))
    curl::handle_setheaders(h, `Content-Type` = "application/json")
    curl::curl_fetch_memory(origin$url("/__script"), handle = h)
    invisible(NULL)
}

# The most requests the origin held at once since the last script.
peak_in_flight <- function() {
    as.numeric(rawToChar(curl::curl_fetch_memory(origin$url("/__peak"))$content))
}

served <- function() {
    jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory(origin$url("/__log"))$content),
        simplifyVector = FALSE)
}

test_client <- function(...) {
    vpndetection_client(base_url = origin$url(), ...)
}

corpus <- function() {
    jsonlite::read_json(test_path("testdata", "testdata.json"), simplifyVector = FALSE)
}

caught <- function(expr) {
    tryCatch({ expr; NULL }, vpndetection_error = function(e) e)
}
