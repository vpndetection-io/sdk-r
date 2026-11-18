# The transport baseline every SDK of this API keeps, measured at a local origin.

ok_answer <- list(status = 200, body = list(ip = "8.8.8.8", is_vpn = FALSE))

test_that("every request names the SDK and sends the key only as a Bearer header", {
    script(ok_answer)
    probe(test_client(api_key = "  key-1\n"))
    sent <- served()[[1]]
    expect_match(sent$headers[["User-Agent"]], "^vpndetection-r/[0-9]+\\.[0-9]+\\.[0-9]+$")
    expect_identical(sent$headers[["Authorization"]], "Bearer key-1")
    expect_identical(sent$path, "/probe")
    for (key in list(NULL, "", "   ", "\t")) {
        script(ok_answer)
        probe(test_client(api_key = key))
        expect_null(served()[[1]]$headers[["Authorization"]])
    }
})

test_that("a key holding a control character is refused where it is set", {
    for (key in c("key\r\nX-Injected: 1", "key\x01", "a\177b")) {
        expect_s3_class(caught(test_client(api_key = key)), "vpndetection_bad_request")
    }
})

test_that("one to three trailing slashes on the base URL are dropped", {
    for (slashes in c("/", "//", "///")) {
        client <- vpndetection_client(base_url = paste0(origin$url(), slashes))
        script(ok_answer)
        probe(client)
        probe_many(client, 2)
        expect_identical(vapply(served(), `[[`, "", "path"), rep("/probe", 3))
    }
})

test_that("a timeout, retry count or concurrency no call can use is refused where it is set", {
    bad <- list(-1, NaN, Inf, "1", "", TRUE, list(1), NA, 3e6)
    for (value in bad) {
        expect_s3_class(caught(test_client(timeout = value)), "vpndetection_bad_request")
    }
    counts <- list(-1, NaN, 1.5, "word", "", list(1), NA, Inf)
    for (value in counts) {
        expect_s3_class(caught(test_client(retries = value)), "vpndetection_bad_request")
        expect_s3_class(caught(test_client(concurrency = value)), "vpndetection_bad_request")
    }
    expect_s3_class(caught(test_client(concurrency = 0)), "vpndetection_bad_request")
    script(ok_answer)
    client <- test_client()
    for (value in bad) {
        expect_s3_class(caught(probe(client, timeout = value)), "vpndetection_bad_request")
        expect_s3_class(caught(probe_many(client, 1, timeout = value)),
            "vpndetection_bad_request")
    }
    for (value in counts) {
        expect_s3_class(caught(probe(client, retries = value)), "vpndetection_bad_request")
        expect_s3_class(caught(probe_many(client, 1, concurrency = value)),
            "vpndetection_bad_request")
    }
    expect_s3_class(caught(probe_many(client, 1, concurrency = 0)), "vpndetection_bad_request")
    expect_length(served(), 0)
})

test_that("an attempt past its timeout is a retried network error", {
    # Timed against the clock, which a loaded check machine runs slow.
    skip_on_cran()
    # A socket that listens and never answers: the connection opens and no response comes.
    silent <- NULL
    for (port in sample(20000:40000, 20)) {
        silent <- tryCatch(serverSocket(port), error = function(e) NULL)
        if (!is.null(silent)) break
    }
    on.exit(close(silent))
    url <- paste0("http://127.0.0.1:", port)
    started <- Sys.time()
    e <- caught(probe(vpndetection_client(base_url = url, timeout = 0.2, retries = 1)))
    took <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    expect_s3_class(e, "vpndetection_network")
    expect_true(e$retryable)
    expect_gt(took, 0.6)
    expect_lt(took, 3)
    e <- caught(probe(vpndetection_client(base_url = url), timeout = 0.2, retries = 0))
    expect_s3_class(e, "vpndetection_network")
    outcome <- probe_many(vpndetection_client(base_url = url), 1, timeout = 0.2, retries = 0)
    expect_s3_class(outcome[[1]]$error, "vpndetection_network")
})

test_that("retries follow the status: 5xx and a throttle three times, every other 4xx once", {
    sends <- function(answer) {
        script(answer)
        caught(probe(test_client()))
        length(served())
    }
    expect_identical(sends(list(status = 500, body = list(error = "x"))), 3L)
    expect_identical(sends(list(status = 404, body = list(rc = "NOT_FOUND"))), 1L)
    expect_identical(sends(list(status = 422, body = list(rc = "x"))), 1L)
    expect_identical(sends(list(status = 429, body = list(error = "cap"))), 1L)
    expect_identical(sends(list(status = 429, headers = list(`Retry-After` = "0"),
        body = list(error = "slow"))), 3L)
    expect_identical(sends(list(status = 503, headers = list(`Retry-After` = "0"))), 3L)
    expect_identical(sends(list(status = 503, headers = list(`Retry-After` = "1"))), 3L)
})

test_that("a Retry-After no parser should guess at is waited out on the backoff", {
    # Timed against the clock, which a loaded check machine runs slow.
    skip_on_cran()
    for (value in c("-1", "x", "tomorrow", "0x10", "1_0", "1e3", "+1", "1.5", "2147484",
        "9223372036854775807", "Mon, 31 Nov 2026 08:49:37 GMT")) {
        script(list(status = 429, headers = list(`Retry-After` = value), body = list(error = "slow")))
        started <- Sys.time()
        e <- caught(probe(test_client()))
        took <- as.numeric(difftime(Sys.time(), started, units = "secs"))
        expect_identical(e$kind, "rate_limited", label = value)
        expect_true(is.na(e$retry_after) || e$retry_after > 2147483, label = value)
        expect_length(served(), 3)
        expect_lt(took, 5)
    }
    script(list(status = 429, headers = list(`Retry-After` = "1"), body = list(error = "slow")))
    started <- Sys.time()
    caught(probe(test_client(retries = 1)))
    expect_gte(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)
    expect_equal(pkg$parse_retry_after("Sun, 06 Nov 1994 08:49:37 GMT"), 0)
    expect_equal(pkg$parse_retry_after("Sunday, 06-Nov-94 08:49:37 GMT"), 0)
    expect_equal(pkg$parse_retry_after("Sun Nov  6 08:49:37 1994"), 0)
    expect_true(is.na(pkg$parse_retry_after("Sun, 06 Nov 1994 08:49:37 PST")))
})

test_that("a 2xx no call can read is a retried server_error carrying the status", {
    bodies <- list(
        list(raw = "<html>proxy</html>", headers = list(`Content-Type` = "text/html")),
        list(raw = "{\"ip\":\"8.8.8.8\",\"is_vp"), list(raw = ""), list(raw = "null"),
        list(raw = "[]"), list(raw = "{}"), list(body = list(ip = NULL, is_vpn = FALSE)),
        list(body = list(ip = "8.8.8.8", is_vpn = "false")),
        list(body = list(ip = 8, is_vpn = FALSE))
    )
    for (b in bodies) {
        label <- if (is.null(b$raw)) jsonlite::toJSON(b$body, auto_unbox = TRUE, null = "null") else b$raw
        script(c(list(status = 200), b))
        e <- caught(probe(test_client()))
        expect_s3_class(e, "vpndetection_server_error")
        expect_identical(e$status, 200L, label = label)
        expect_length(served(), 3)
        script(c(list(status = 200), b))
        outcome <- probe_many(test_client(), 2)
        for (o in outcome) {
            expect_s3_class(o$error, "vpndetection_server_error")
            expect_identical(o$error$status, 200L, label = label)
        }
        expect_length(served(), 6)
    }
})

test_that("a 4xx whose body is not the API's is classified by its status and sent once", {
    for (case in list(list(401, "unauthorized"), list(403, "forbidden"), list(429, "quota_exceeded"),
        list(400, "bad_request"))) {
        for (type in c("text/html", "application/json")) {
            script(list(status = case[[1]], raw = if (case[[1]] == 403) "" else "<html>no</html>",
                headers = list(`Content-Type` = type)))
            e <- caught(probe(test_client()))
            expect_identical(e$kind, case[[2]])
            expect_identical(e$status, as.integer(case[[1]]))
            expect_length(served(), 1)
        }
    }
})

test_that("a date member is held to RFC 3339 and a malformed one is refused", {
    answer <- function(seen) list(status = 200, body = list(ip = "8.8.8.8", seen = seen))
    script(answer("2026-09-02"))
    expect_identical(probe(test_client())$seen, "2026-09-02")
    for (bad in c("x", "tomorrow", "monday", "2026-02-30", "2026-09-02T10:00:00Z", "2026-09-02\n",
        "٢026-09-02", "2026-9-2", "")) {
        script(answer(bad))
        e <- caught(probe(test_client(retries = 0)))
        expect_s3_class(e, "vpndetection_server_error")
    }
    is_dt <- pkg$is_date_time
    expect_true(is_dt("2026-09-02T10:00:00Z"))
    expect_true(is_dt("2026-09-02t10:00:00z"))
    expect_true(is_dt("2026-09-02T10:00:00.5+05:30"))
    for (bad in c("2026-09-02T24:00:00Z", "2026-09-02T10:00:00", "2026-09-02", "2026-09-02T10:00Z",
        "2026-09-02T10:00:00+05", "2026-09-02T10:00:00Z\n", "2026-02-30T10:00:00Z", "x",
        "tomorrow", "monday")) {
        expect_false(is_dt(bad), label = bad)
    }
})

test_that("requests sent together keep at most `concurrency` in flight, per client and per call", {
    # Timed against the clock, which a loaded check machine runs slow.
    skip_on_cran()
    peak <- function(client, ...) {
        script(c(ok_answer, list(delay = 0.5)))
        probe_many(client, 6, ...)
        expect_length(served(), 6)
        peak_in_flight()
    }
    expect_identical(peak(test_client(retries = 0)), 6)
    expect_identical(peak(test_client(retries = 0, concurrency = 4)), 4)
    expect_identical(peak(test_client(retries = 0), concurrency = 2), 2)
    expect_identical(peak(test_client(retries = 0, concurrency = 3), concurrency = 1), 1)
})

test_that("the cache keeps the newest answers and drops one past its lifetime", {
    cache <- pkg$new_cache(2L, 3600)
    cache$set("a", 1)
    cache$set("b", 2)
    cache$get("a")
    cache$set("c", 3)
    expect_identical(cache$get("a"), 1)
    expect_null(cache$get("b"))
    expect_identical(cache$get("c"), 3)
    off <- pkg$new_cache(0L, 3600)
    off$set("a", 1)
    expect_null(off$get("a"))
    short <- pkg$new_cache(10L, 0.05)
    short$set("a", 1)
    Sys.sleep(0.1)
    expect_null(short$get("a"))
})
