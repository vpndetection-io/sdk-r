# The shared corpus every SDK of this API answers identically. Its errors section runs through the
# probe until the client has calls of its own; each later section joins with the call it needs.

test_that("the corpus is here and carries error cases", {
    expect_gt(length(corpus()$errors), 0)
})

test_that("every errors case is classified as the corpus says, and sent as often", {
    for (case in corpus()$errors) {
        script(list(status = case$status, headers = case$headers, body = case$body))
        e <- caught(probe(test_client(retries = 0, cache_size = 0)))
        expect_s3_class(e, "vpndetection_error")
        expect_identical(e$kind, case$expect$kind, label = case$name)
        expect_identical(e$retryable, case$expect$retryable, label = case$name)
        expect_identical(e$status, as.integer(case$status), label = case$name)
        if (!is.null(case$expect$message)) {
            expect_identical(conditionMessage(e), case$expect$message, label = case$name)
        }
        if (!is.null(case$expect$retryAfterSeconds)) {
            expect_equal(e$retry_after, case$expect$retryAfterSeconds, label = case$name)
        }
        expect_length(served(), 1)
    }
})

test_that("a retryable errors case is sent once more per retry, and every other one once", {
    for (case in corpus()$errors) {
        # The corpus's Retry-After would make this test sleep; a retried case answers 0 instead.
        headers <- case$headers
        for (h in names(headers)) if (tolower(h) == "retry-after") headers[[h]] <- "0"
        script(list(status = case$status, headers = headers, body = case$body))
        caught(probe(test_client(retries = 2, cache_size = 0)))
        expect_length(served(), if (isTRUE(case$expect$retryable)) 3 else 1)
    }
})
