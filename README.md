# [<img src="https://s3.vpndetection.io/vpndetection-public/brand/mark.svg" alt="VPNDetection" height="28"/>](https://vpndetection.io/) VPNDetection R Client Library

[![CRAN](https://www.r-pkg.org/badges/version/vpndetection)](https://cran.r-project.org/package=vpndetection)
[![license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE.md)

The official R client library for the [VPNDetection](https://vpndetection.io) API.

## Getting Started

```r
install.packages("vpndetection")
```

Requires R 4.1 or newer. [curl](https://cran.r-project.org/package=curl) and [jsonlite](https://cran.r-project.org/package=jsonlite) are its only dependencies.

## Usage

```r
library(vpndetection)

client <- vpndetection_client()
```

### With an API key

Create one in the [console](https://app.vpndetection.io), then pass it in:

```r
client <- vpndetection_client(api_key = Sys.getenv("VPNDETECTION_API_KEY"))
```

The key is optional: leave it out, or pass an empty one, and no `Authorization` header is sent at all.

### Errors

Every failure is a condition of class `vpndetection_error`, carrying `kind`, `status`, `retryable` and `retry_after`:

```r
tryCatch(client$clear_cache(), vpndetection_error = function(e) {
    message(e$kind, ": ", conditionMessage(e))
})
```

`kind` is one of `bad_request`, `unauthorized`, `forbidden`, `rate_limited`, `quota_exceeded`, `server_error` or `network`, and each is also a class of its own (`vpndetection_rate_limited`).

`rate_limited` and `quota_exceeded` both arrive as HTTP 429 and are not the same thing: a rate limit passes if you wait, while a spent quota needs your allowance raised or the window to roll over. The library retries rate limits for you, and server and network failures, but never a spent quota or anything else you sent. It waits the `Retry-After` the API sent, or its own backoff from 250 ms when that is past about 24.8 days.

### Timeouts and retries

Each attempt gives up after 30 seconds, and a transient failure is retried twice. Both can be changed for the client, and for a single call:

```r
client <- vpndetection_client(timeout = 10, retries = 4)
```

The timeout is in seconds and applies to each attempt, body included, so a retried call can take longer in total, and 0 means no bound. An attempt that runs out of time fails as a retryable `network` error. A timeout or retry count no call can use is refused where you pass it, before any request.

## Documentation

The API reference is at [docs.vpndetection.io](https://docs.vpndetection.io), and `?vpndetection_client` documents every option.

## Other Libraries

There are official VPNDetection client libraries available for many languages including PHP, Python, Go, Java, and Ruby. See our GitHub at https://github.com/vpndetection-io for more.

## About VPNDetection

VPN Detection API: Accurate anonymity detection identifying VPNs, residential proxies, hosting servers, Tor nodes, CDNs, relays and more.

[<img src="https://s3.vpndetection.io/vpndetection-public/brand/mark.svg" alt="VPNDetection" height="64"/>](https://vpndetection.io/)

## License

This project is licensed under the [MIT License](LICENSE.md).
