\name{vpndetection_client}
\alias{vpndetection_client}
\title{A client for the 'VPNDetection' API}
\description{
Creates a client whose functions look up what an IP address is: a VPN, a proxy of one of four
kinds, a hosting provider, a Tor node, a content delivery network or a relay.
}
\usage{
vpndetection_client(api_key = NULL, base_url = NULL, timeout = 30, retries = 2,
  concurrency = 8, cache_size = 10000, cache_ttl = 3600)
}
\arguments{
\item{api_key}{Your API key from the console, or \code{NULL} to look up without one. The key is
trimmed, and a blank one is no key. A key holding a control character is refused.}
\item{base_url}{The API's address. Defaults to \code{https://api.vpndetection.io}; a trailing
slash is dropped.}
\item{timeout}{Seconds each attempt may take, body included. 0 means no bound. A retried call can
take longer in total.}
\item{retries}{Attempts after a retryable failure: a 5xx, a transport failure, or a 429 carrying
\code{Retry-After}. Each waits the server's \code{Retry-After}, or a backoff of 250 ms that doubles
per retry.}
\item{concurrency}{Batch chunks in flight at once.}
\item{cache_size}{Answers kept per client, keyed by address. 0 turns the cache off.}
\item{cache_ttl}{Seconds an answer stays cached. 0 turns the cache off.}
}
\value{
A \code{vpndetection_client}: a list of functions.
\describe{
\item{\code{lookup(ip, timeout = NULL, retries = NULL)}}{The answer for one address, as a list.
A reserved address is answered locally, with \code{is_bogon} \code{TRUE}.}
\item{\code{lookup_batch(ips, concurrency = NULL, timeout = NULL, retries = NULL)}}{A named
list, one entry per distinct address, in input order. An address that failed holds its
\code{vpndetection_error} rather than failing the batch. Any number of addresses is sent in chunks
of 1000.}
\item{\code{my_ip(timeout = NULL, retries = NULL)}}{The answer for the address you are calling
from. Never cached.}
\item{\code{my_entitlement(timeout = NULL, retries = NULL)}}{What your API key may spend: its
plan and usage. Never cached.}
\item{\code{clear_cache()}}{Empties the cache.}
}
Every failure is a condition of class \code{vpndetection_error} carrying \code{kind}
(\code{bad_request}, \code{unauthorized}, \code{forbidden}, \code{rate_limited},
\code{quota_exceeded}, \code{server_error} or \code{network}), \code{status}, \code{retryable}
and \code{retry_after}.
}
\examples{
client <- vpndetection_client()
client$lookup("10.0.0.1")$is_bogon
\donttest{
answer <- tryCatch(client$lookup("8.8.8.8"), vpndetection_error = function(e) e)
}
}
