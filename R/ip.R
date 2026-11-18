# Address parsing for the bogon check: an address is an integer vector of its bits' bytes, 4 for
# IPv4 and 16 for IPv6, or NULL when the string is no address.

parse_ipv4 <- function(s) {
    if (!grepl("^[0-9]{1,3}(\\.[0-9]{1,3}){3}$", s)) {
        return(NULL)
    }
    parts <- strsplit(s, ".", fixed = TRUE)[[1]]
    # A leading zero is octal to some parsers and decimal to others, so it is no address.
    if (any(nchar(parts) > 1 & startsWith(parts, "0"))) {
        return(NULL)
    }
    octets <- as.integer(parts)
    if (any(octets > 255)) {
        return(NULL)
    }
    octets
}

parse_ipv6 <- function(s) {
    s <- sub("%.*$", "", s)
    if (!grepl("^[0-9A-Fa-f:.]+$", s) || lengths(regmatches(s, gregexpr("::", s))) > 1) {
        return(NULL)
    }
    tail_v4 <- NULL
    if (grepl(".", s, fixed = TRUE)) {
        last <- sub("^.*:", "", s)
        tail_v4 <- parse_ipv4(last)
        if (is.null(tail_v4)) {
            return(NULL)
        }
        s <- paste0(substr(s, 1, nchar(s) - nchar(last)), "0:0")
    }
    halves <- strsplit(s, "::", fixed = TRUE)[[1]]
    split_groups <- function(x) {
        if (length(x) == 0 || is.na(x) || x == "") character(0) else strsplit(x, ":", fixed = TRUE)[[1]]
    }
    if (grepl("::", s, fixed = TRUE)) {
        head <- split_groups(halves[1])
        tail <- split_groups(if (length(halves) > 1) halves[2] else "")
        missing <- 8 - length(head) - length(tail)
        if (missing < 1) {
            return(NULL)
        }
        groups <- c(head, rep("0", missing), tail)
    } else {
        groups <- split_groups(s)
    }
    if (length(groups) != 8 || any(!grepl("^[0-9A-Fa-f]{1,4}$", groups))) {
        return(NULL)
    }
    words <- strtoi(groups, 16L)
    bytes <- as.vector(rbind(words %/% 256L, words %% 256L))
    if (!is.null(tail_v4)) {
        bytes[13:16] <- tail_v4
    }
    bytes
}

parse_ip <- function(s) {
    if (!is.character(s) || length(s) != 1 || is.na(s)) {
        return(NULL)
    }
    if (grepl(":", s, fixed = TRUE)) parse_ipv6(s) else parse_ipv4(s)
}

# An IPv4-mapped IPv6 address (::ffff:a.b.c.d) is the IPv4 visitor a server listening on :: sees,
# so it is judged, sent and cached as that IPv4 address.
unmap_ip <- function(s) {
    bytes <- parse_ip(s)
    if (length(bytes) == 16 && all(bytes[1:10] == 0) && all(bytes[11:12] == 255)) {
        return(paste(bytes[13:16], collapse = "."))
    }
    s
}

parse_cidr <- function(cidr) {
    parts <- strsplit(cidr, "/", fixed = TRUE)[[1]]
    list(bytes = parse_ip(parts[1]), bits = as.integer(parts[2]))
}

in_cidr <- function(bytes, net) {
    if (length(bytes) != length(net$bytes)) {
        return(FALSE)
    }
    whole <- net$bits %/% 8
    if (whole > 0 && any(bytes[seq_len(whole)] != net$bytes[seq_len(whole)])) {
        return(FALSE)
    }
    rest <- net$bits %% 8
    if (rest == 0) {
        return(TRUE)
    }
    mask <- 256L - 2L^(8L - rest)
    bitwAnd(bytes[whole + 1], mask) == bitwAnd(net$bytes[whole + 1], mask)
}

bogon_nets <- local({
    nets <- NULL
    function() {
        if (is.null(nets)) {
            nets <<- lapply(c(bogon_v4, bogon_v6), parse_cidr)
        }
        nets
    }
})

is_bogon <- function(ip) {
    if (!is.character(ip) || length(ip) != 1 || is.na(ip)) {
        refuse("is_bogon: ip must be one string")
    }
    bytes <- parse_ip(unmap_ip(ip))
    if (is.null(bytes)) {
        return(FALSE)
    }
    for (net in bogon_nets()) {
        if (in_cidr(bytes, net)) {
            return(TRUE)
        }
    }
    FALSE
}
