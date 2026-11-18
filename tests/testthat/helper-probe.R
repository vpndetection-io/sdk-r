# Until the client has calls of its own, the errors corpus and the transport's cases run through
# these: a GET of /probe made exactly as a call makes it, its answer held to probe_schema, and the
# same request several times at once as a call that makes several sends them. Move each test onto a
# real call once there is one, and delete this file.

pkg <- asNamespace("vpndetection")

probe_schema <- list(type = "object", required = list("ip"), properties = list(
    ip = list(type = "string"),
    is_vpn = list(type = "boolean"),
    seen = list(type = "string", format = "date")
))

read_probe <- function(json) {
    problem <- pkg$check_value(json, probe_schema, "probe")
    if (!is.null(problem)) {
        pkg$unreadable(problem)
    }
    json
}

probe <- function(client, timeout = NULL, retries = NULL) {
    state <- attr(client, "state")
    opts <- pkg$client_options(state, "probe", timeout, retries)
    pkg$call_api(state, "/probe", opts$timeout, opts$retries, read_probe)
}

# n requests at once, each outcome list(value = ...) or list(error = <condition>), in order.
probe_many <- function(client, n, concurrency = NULL, timeout = NULL, retries = NULL) {
    state <- attr(client, "state")
    opts <- pkg$client_options(state, "probe_many", timeout, retries, concurrency)
    requests <- rep(list(list(path = "/probe", body = "{}")), n)
    pkg$call_api_many(state, requests, opts$timeout, opts$retries, opts$concurrency, read_probe)
}
