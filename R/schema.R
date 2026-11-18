# Every answer is held to the schema the pinned spec gives it before it is handed back, so a member
# of the wrong type, a null the spec does not allow or a malformed date fails as the API's fault
# (a retried server_error) rather than reaching the caller as if it were the answer. Members the
# spec does not name yet pass through untouched, so a new one needs no release of this package.

spec_schemas <- local({
    schemas <- NULL
    function() {
        if (is.null(schemas)) {
            path <- system.file("spec", "openapi.json", package = "vpndetection")
            schemas <<- jsonlite::read_json(path, simplifyVector = FALSE)$components$schemas
        }
        schemas
    }
})

# NULL when `value` fits the named component schema, otherwise where and why it does not.
schema_problem <- function(value, name) {
    check_value(value, list(`$ref` = paste0("#/components/schemas/", name)), name)
}

is_object <- function(x) is.list(x) && !is.null(names(x))
is_array <- function(x) is.list(x) && is.null(names(x))

check_value <- function(value, schema, where) {
    while (!is.null(schema$`$ref`)) {
        schema <- spec_schemas()[[sub("^#/components/schemas/", "", schema$`$ref`)]]
    }
    if (is.null(value)) {
        return(if (isTRUE(schema$nullable)) NULL else paste(where, "is null"))
    }
    for (part in schema$allOf) {
        problem <- check_value(value, part, where)
        if (!is.null(problem)) {
            return(problem)
        }
    }
    type <- schema$type
    if (is.null(type)) {
        if (!is.null(schema$properties) || !is.null(schema$additionalProperties)) type <- "object"
        else return(NULL)
    }
    scalar <- function(ok) {
        if (length(value) == 1 && !is.list(value) && !is.na(value) && ok) NULL
        else paste(where, "is not", if (type %in% c("integer", "array")) "an" else "a", type)
    }
    switch(type,
        object = check_object(value, schema, where),
        array = {
            if (!is_array(value)) return(paste(where, "is not an array"))
            for (i in seq_along(value)) {
                problem <- check_value(value[[i]], schema$items, sprintf("%s[%d]", where, i))
                if (!is.null(problem)) return(problem)
            }
            NULL
        },
        boolean = scalar(is.logical(value)),
        integer = scalar(is.numeric(value) && value == trunc(value)),
        number = scalar(is.numeric(value)),
        string = {
            problem <- scalar(is.character(value))
            if (!is.null(problem) || is.null(schema$format)) problem
            else if (schema$format == "date" && !is_date(value)) paste(where, "is not a date")
            else if (schema$format == "date-time" && !is_date_time(value))
                paste(where, "is not a date-time")
            else NULL
        },
        NULL
    )
}

check_object <- function(value, schema, where) {
    # jsonlite reads {} as a named empty list and [] as an unnamed one.
    if (!is_object(value) && !(is.list(value) && length(value) == 0 && !is.null(attr(value, "names")))) {
        return(paste(where, "is not an object"))
    }
    for (member in unlist(schema$required)) {
        if (!(member %in% names(value))) {
            return(paste0(where, " has no ", member))
        }
    }
    for (member in names(value)) {
        sub_schema <- schema$properties[[member]]
        if (is.null(sub_schema) && is.list(schema$additionalProperties)) {
            sub_schema <- schema$additionalProperties
        }
        if (!is.null(sub_schema)) {
            problem <- check_value(value[[member]], sub_schema, paste0(where, "$", member))
            if (!is.null(problem)) {
                return(problem)
            }
        }
    }
    NULL
}

# RFC 3339's full-date: ASCII digits, a real calendar day, and nothing after it.
is_date <- function(s) {
    grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}\\z", s, perl = TRUE) && real_day(s)
}

real_day <- function(s) {
    day <- as.Date(s, format = "%Y-%m-%d", optional = TRUE)
    !is.na(day) && format(day, "%Y-%m-%d") == s
}

# RFC 3339's date-time: seconds required, a zone required (Z or +hh:mm), T and Z in either case.
is_date_time <- function(s) {
    m <- regmatches(s, regexec(paste0(
        "^([0-9]{4}-[0-9]{2}-[0-9]{2})[Tt]([0-9]{2}):([0-9]{2}):([0-9]{2})(\\.[0-9]+)?",
        "([Zz]|[+-]([0-9]{2}):([0-9]{2}))\\z"), s, perl = TRUE))[[1]]
    if (length(m) == 0 || !real_day(m[2])) {
        return(FALSE)
    }
    hms <- as.integer(m[3:5])
    zone <- if (m[8] == "") c(0L, 0L) else as.integer(m[8:9])
    hms[1] <= 23 && hms[2] <= 59 && hms[3] <= 59 && zone[1] <= 23 && zone[2] <= 59
}
