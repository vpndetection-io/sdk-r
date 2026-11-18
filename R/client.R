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
