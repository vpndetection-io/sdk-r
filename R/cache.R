# The per-client answer cache, keyed by address. R runs one call at a time, so two misses for one
# address never overlap and need no shared request.

new_cache <- function(size, ttl) {
    entries <- new.env(parent = emptyenv())
    stamp <- 0
    list(
        get = function(key) {
            entry <- entries[[key]]
            if (is.null(entry)) {
                return(NULL)
            }
            if (entry$expires <= Sys.time()) {
                rm(list = key, envir = entries)
                return(NULL)
            }
            stamp <<- stamp + 1
            entry$used <- stamp
            assign(key, entry, envir = entries)
            entry$value
        },
        set = function(key, value) {
            if (size == 0 || ttl == 0) {
                return(invisible(NULL))
            }
            stamp <<- stamp + 1
            assign(key, list(value = value, expires = Sys.time() + ttl, used = stamp), envir = entries)
            keys <- ls(entries, all.names = TRUE, sorted = FALSE)
            if (length(keys) > size) {
                used <- vapply(keys, function(k) entries[[k]]$used, numeric(1))
                rm(list = keys[which.min(used)], envir = entries)
            }
            invisible(NULL)
        },
        clear = function() {
            rm(list = ls(entries, all.names = TRUE), envir = entries)
        }
    )
}
