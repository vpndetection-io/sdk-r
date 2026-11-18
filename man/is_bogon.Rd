\name{is_bogon}
\alias{is_bogon}
\title{Whether an address is reserved}
\description{
Whether an IP address falls in reserved or private space, which no lookup sends to the API. An
IPv4-mapped IPv6 address is judged as the IPv4 address it carries.
}
\usage{
is_bogon(ip)
}
\arguments{
\item{ip}{One address, as a string.}
}
\value{
\code{TRUE} or \code{FALSE}. A string that is no address is \code{FALSE}.
}
\examples{
is_bogon("10.0.0.1")
is_bogon("8.8.8.8")
}
