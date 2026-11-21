# The shared corpus every SDK of this API answers identically.

test_that("every errors case is classified as the corpus says, and sent as often", {
    for (case in corpus()$errors) {
        script(list(status = case$status, headers = case$headers, body = case$body))
        e <- caught(test_client(retries = 0, cache_size = 0)$lookup("8.8.8.8"))
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

test_that("every lookup case reads present members as served and absent ones as absent", {
    for (case in corpus()$lookup) {
        script(list(status = case$status, body = case$body))
        result <- test_client(cache_size = 0)$lookup(case$body$ip)
        expect_identical(result$ip, case$expect$ip, label = case$name)
        expect_identical(isTRUE(result$is_bogon), case$expect$isBogon, label = case$name)
        for (member in names(case$expect$present)) {
            expect_identical(result[[member]], case$expect$present[[member]],
                label = paste(case$name, member))
        }
        for (member in unlist(case$expect$absent)) {
            expect_false(member %in% names(result), label = paste(case$name, member))
        }
        for (member in unlist(case$expect$emptyPresent)) {
            expect_true(member %in% names(result), label = paste(case$name, member))
            expect_length(result[[member]], 0)
        }
        for (detail in setdiff(names(case$expect),
            c("ip", "isBogon", "present", "absent", "emptyPresent"))) {
            expect_identical(result[[detail]], case$expect[[detail]],
                label = paste(case$name, detail))
        }
    }
})

test_that("is_bogon agrees with every corpus claim", {
    for (case in corpus()$isBogon) {
        expect_identical(is_bogon(case$ip), case$expect, label = case$ip)
    }
})

test_that("an IPv4-mapped address is judged, sent and cached as the address it carries", {
    for (case in corpus()$ipv4Mapped) {
        expect_identical(is_bogon(case$ip), case$expect, label = case$ip)
        if (!case$expect) {
            script(list(status = 200, body = list(ip = case$carries, is_vpn = FALSE)))
            client <- test_client()
            client$lookup(case$ip)
            client$lookup(case$carries)
            log <- served()
            expect_length(log, 1)
            expect_identical(log[[1]]$path, paste0("/", case$carries))
        }
    }
})

test_that("a bogon is answered locally in the widest shape", {
    shape <- corpus()$bogonResponse
    script(list(status = 500, body = list(error = "must not be asked")))
    result <- test_client()$lookup("10.0.0.1")
    expect_length(served(), 0)
    expect_true(result$is_bogon)
    expect_identical(result$ip, "10.0.0.1")
    for (flag in unlist(shape$flagsFalse)) expect_false(result[[flag]], label = flag)
    for (detail in unlist(shape$emptyObjects)) expect_length(result[[detail]], 0)
    members <- setdiff(names(shape), c("why", "flagsFalse", "emptyObjects"))
    for (member in members) {
        expect_identical(result[[member]], shape[[member]], label = member)
    }
    expect_setequal(names(result), c("ip", "is_bogon", unlist(shape$flagsFalse),
        unlist(shape$emptyObjects), members))
    expect_null(schema_problem(result, "LookupResponse"))
})

test_that("a batch collapses duplicates, answers bogons locally and keys by address", {
    statuses <- c(bad_request = 400L, quota_exceeded = 429L, server_error = 500L)
    for (case in corpus()$batch) {
        input <- unlist(case$input)
        fail <- stats::setNames(lapply(names(case$expect$errorKinds), function(ip) {
            list(status = statuses[[case$expect$errorKinds[[ip]]]], error = "refused")
        }), names(case$expect$errorKinds))
        if (length(fail) == 0) fail <- structure(list(), names = character(0))
        script(list(status = 200, echo = TRUE, fail = fail))
        client <- test_client(retries = 0)
        for (i in seq_len(if (is.null(case[["repeat"]])) 1 else case[["repeat"]])) {
            answer <- client$lookup_batch(input)
        }
        if (!is.null(case$expect$keys)) {
            expect_identical(names(answer), unlist(case$expect$keys), label = case$name)
        }
        if (!is.null(case$expect$keyCount)) {
            expect_length(answer, case$expect$keyCount)
        }
        if (!is.null(case$expect$httpRequests)) {
            expect_length(served(), case$expect$httpRequests)
        }
        for (key in unlist(case$expect$bogonKeys)) expect_true(answer[[key]]$is_bogon)
        for (key in names(case$expect$errorKinds)) {
            expect_s3_class(answer[[key]], "vpndetection_error")
            expect_identical(answer[[key]]$kind, case$expect$errorKinds[[key]], label = key)
        }
        for (key in setdiff(names(answer), c(names(case$expect$errorKinds),
            unlist(case$expect$bogonKeys)))) {
            expect_identical(answer[[key]]$ip, key)
        }
    }
})

test_that("the entitlement hands back the plan's name, and a tier it was not built with, as served", {
    cases <- Filter(function(case) case$call == "myEntitlement", corpus()$vocabulary$cases)
    expect_gt(length(cases), 0)
    for (case in cases) {
        script(list(status = 200, body = case$answer))
        answer <- test_client()$my_entitlement()
        for (path in names(case$expect)) {
            value <- answer
            for (step in strsplit(path, ".", fixed = TRUE)[[1]]) value <- value[[step]]
            expect_identical(value, case$expect[[path]], label = paste(case$name, path))
        }
    }
})
