# [<img src="https://s3.vpndetection.io/vpndetection-public/brand/mark.svg" alt="VPNDetection" height="28"/>](https://vpndetection.io/) VPNDetection R Client Library

[![CRAN](https://www.r-pkg.org/badges/version/vpndetection)](https://cran.r-project.org/package=vpndetection)
[![license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/vpndetection-io/sdk-r/blob/main/LICENSE.md)

The official R client for the [VPNDetection](https://vpndetection.io) API: whether an IP address is a VPN, a residential, datacenter, mobile or public proxy, a hosting provider, a Tor node, a CDN or a relay.

## Getting Started

```r
install.packages("vpndetection")
```

## Look up an address

```r
library(vpndetection)

client <- vpndetection_client(api_key = Sys.getenv("VPNDETECTION_API_KEY"))

answer <- client$lookup("146.70.116.130")
answer$is_vpn
answer$vpn$provider
```

A key is optional: without one, lookups run on the Unlimited plan. Get a key at [app.vpndetection.io](https://app.vpndetection.io).

The answer is the API's JSON as a list. A member your plan does not include is absent, never `FALSE`. Reserved and private addresses are answered locally, with `is_bogon` set to `TRUE`, and never sent.

## Many addresses

```r
answers <- client$lookup_batch(c("1.1.1.1", "8.8.8.8", "10.0.0.1"))
answers[["8.8.8.8"]]$is_hosting
```

The result is a named list with one entry per distinct address. An address that failed holds its error rather than failing the batch. Batches need a paid plan, and any number of addresses is sent in chunks of 1000.

## Your own address and plan

```r
client$my_ip()
client$my_entitlement()
```

## Errors

Every failure is a condition of class `vpndetection_error`:

```r
tryCatch(client$lookup("8.8.8.8"), vpndetection_error = function(e) {
    message(e$kind, ": ", conditionMessage(e))
})
```

`kind` is one of `bad_request`, `unauthorized`, `forbidden`, `rate_limited`, `quota_exceeded`, `server_error` or `network`. A 5xx, a network failure and a rate limit are retried twice by default, waiting as long as the API's `Retry-After` asks.

## Options

`vpndetection_client()` takes `timeout` (30 seconds per attempt), `retries` (2), `concurrency` (8 batch chunks at once), `cache_size` (10,000 answers) and `cache_ttl` (an hour). Each call takes its own `timeout` and `retries`, and `lookup_batch()` its own `concurrency`.

## Documentation

The API reference is at [docs.vpndetection.io](https://docs.vpndetection.io), and `?vpndetection_client` documents every option.

## Other Libraries

There are official VPNDetection client libraries available for many languages including PHP, Python, Go, Java, and Ruby. See our GitHub at https://github.com/vpndetection-io for more.

## About VPNDetection

VPN Detection API: Accurate anonymity detection identifying VPNs, residential proxies, hosting servers, Tor nodes, CDNs, relays and more.

[<img src="https://s3.vpndetection.io/vpndetection-public/brand/mark.svg" alt="VPNDetection" height="64"/>](https://vpndetection.io/)

## License

This project is licensed under the [MIT License](https://github.com/vpndetection-io/sdk-r/blob/main/LICENSE.md).
