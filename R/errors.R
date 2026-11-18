# Every failure this package raises is a condition of class `vpndetection_error`, carrying its
# kind, the HTTP status when there was a response, and the server's Retry-After in seconds.

retryable_kinds <- c("rate_limited", "server_error", "network")

vpnd_error <- function(kind, message = kind, status = NA_integer_, retry_after = NA_real_) {
    structure(
        class = c(paste0("vpndetection_", kind), "vpndetection_error", "error", "condition"),
        list(
            message = message,
            call = NULL,
            kind = kind,
            status = as.integer(status),
            retry_after = as.numeric(retry_after),
            retryable = kind %in% retryable_kinds
        )
    )
}

conditionMessage.vpndetection_error <- function(c) {
    c$message
}

print.vpndetection_error <- function(x, ...) {
    status <- if (is.na(x$status)) "" else paste0(" (HTTP ", x$status, ")")
    cat("<vpndetection_error: ", x$kind, status, "> ", x$message, "\n", sep = "")
    invisible(x)
}

# A refused argument fails before anything is sent, as R's own argument checks do.
refuse <- function(message) {
    stop(vpnd_error("bad_request", message))
}

# Classify a non-2xx response by its status alone, before anything reads its body as the API's.
error_from_response <- function(status, retry_after_header, body_text) {
    message <- error_message(body_text)
    if (is.null(message)) {
        message <- paste("request failed with status", status)
    }
    if (status == 429) {
        # A 429 carrying Retry-After is a throttle worth retrying; one without it is a spent
        # allowance, and the header is the only thing that separates the two.
        if (is.null(retry_after_header)) {
            return(vpnd_error("quota_exceeded", message, status))
        }
        return(vpnd_error("rate_limited", message, status, parse_retry_after(retry_after_header)))
    }
    kind <- if (status == 401) {
        "unauthorized"
    } else if (status == 403) {
        "forbidden"
    } else if (status < 500) {
        "bad_request"
    } else {
        "server_error"
    }
    vpnd_error(kind, message, status)
}

# The API's own explanation, `error` and then `rc`, when the body is the API's JSON object.
error_message <- function(body_text) {
    body <- tryCatch(
        jsonlite::parse_json(body_text, simplifyVector = FALSE),
        error = function(e) NULL
    )
    if (!is.list(body) || is.null(names(body))) {
        return(NULL)
    }
    for (key in c("error", "rc")) {
        value <- body[[key]]
        if (is.character(value) && length(value) == 1 && !is.na(value) && nzchar(value)) {
            return(value)
        }
    }
    NULL
}

# Retry-After is digits alone, or an HTTP-date in one of RFC 9110's three forms in GMT. Anything
# else, a sign, a fraction, hex, an exponent or a word, reads as NA and is waited out on the
# backoff rather than guessed at.
parse_retry_after <- function(value) {
    value <- trimws(value, whitespace = "[ \t]")
    if (grepl("^[0-9]+$", value)) {
        return(as.numeric(value))
    }
    when <- http_date(value)
    if (is.na(when)) {
        return(NA_real_)
    }
    max(0, as.numeric(difftime(when, Sys.time(), units = "secs")))
}

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

http_date <- function(value) {
    mon <- paste(months, collapse = "|")
    time <- "([0-9]{2}):([0-9]{2}):([0-9]{2})"
    imf <- paste0("^(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun), ([0-9]{2}) (", mon, ") ([0-9]{4}) ", time,
        " GMT\\z")
    rfc850 <- paste0("^(?:Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday), ",
        "([0-9]{2})-(", mon, ")-([0-9]{2}) ", time, " GMT\\z")
    asctime <- paste0("^(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun) (", mon, ") ( [1-9]|[0-9]{2}) ", time,
        " ([0-9]{4})\\z")
    parts <- function(pattern) {
        m <- regmatches(value, regexec(pattern, value, perl = TRUE))[[1]]
        if (length(m) == 0) NULL else m[-1]
    }
    if (!is.null(p <- parts(imf))) {
        fields <- list(day = p[1], month = p[2], year = p[3], hms = p[4:6])
    } else if (!is.null(p <- parts(rfc850))) {
        now <- as.integer(format(Sys.time(), "%Y", tz = "UTC"))
        year <- as.integer(p[3]) + now - now %% 100
        if (year > now + 50) {
            year <- year - 100
        }
        fields <- list(day = p[1], month = p[2], year = year, hms = p[4:6])
    } else if (!is.null(p <- parts(asctime))) {
        fields <- list(day = trimws(p[2]), month = p[1], year = p[6], hms = p[3:5])
    } else {
        return(as.POSIXct(NA))
    }
    stamp <- sprintf("%04d-%02d-%02d %s:%s:%s", as.integer(fields$year),
        match(fields$month, months), as.integer(fields$day),
        fields$hms[1], fields$hms[2], fields$hms[3])
    parsed <- as.POSIXct(stamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    # strptime rolls 31 November into 1 December; a date that does not round-trip is no date.
    if (is.na(parsed) || format(parsed, "%Y-%m-%d %H:%M:%S", tz = "UTC") != stamp) {
        return(as.POSIXct(NA))
    }
    parsed
}
