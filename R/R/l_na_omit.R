l_na_omit <- function(w, ..., envir = parent.frame()) {

    n_dim_states <- l_nDimStateNames(w)

    ## NA index
    no_valid_index <- lapply(n_dim_states,
                             function(state) {

                                 value <- .get(state, envir = envir)

                                 if(is.null(value)) {
                                     # maybe in '...' ?
                                     args <- list(...)
                                     value <- args[[state]]
                                     if(is.null(value)) {
                                         # no defined
                                         NULL
                                     } else {
                                         # Not NA, NaN, Inf or -Inf
                                         which(is_not_valid(value))
                                     }
                                 } else if(is.data.frame(value)) {
                                     which(apply(value, 1, function(x) any(is_not_valid(x))))
                                 } else if(is.list(value)) {

                                     which(
                                         vapply(value,
                                                function(v) {
                                                    any(is_not_valid(v))
                                                }, logical(1))
                                     )
                                 } else {
                                     if(length(value) > 1)
                                         which(is_not_valid(value))
                                 }
                             })

    no_valid_index <- unique(unlist(no_valid_index))
    x <- .get("x", envir = envir)
    n <- if(is.null(x)) {
        # serialaxes?
        x <- .get("data", envir = envir)
        if(is.null(x)) integer(0) else dim(x)[1]
    } else {
        length(x)
    }

    # Give a warning if any missing values appear
    if(length(no_valid_index) > 0) {
        warning(
            paste0("Removed {",
                   paste0(no_valid_index, collapse = ","),
                   "} ",
                   length(no_valid_index),
                   " observations containing missing values"),
            call. = FALSE
        )
    }

    valid_index <- setdiff(seq_len(n), no_valid_index)

    # remove parent environment NA
    valid_index_len <- length(valid_index)
    if(valid_index_len == 0)
        warning("No valid input", call. = FALSE)

    lapply(n_dim_states,
           function(state) {
               value <- .get(state, envir = envir)

               if(is.null(value)) {

                   args <- list(...)
                   value <- args[[state]]
                   if(is.null(value)) {
                       # state is defined in ..., like linkingKey, tag, itemLabel, etc
                       if(!state %in% formalArgs(w)) {
                           assign(state,
                                  seq_n(n, char = state)[valid_index],
                                  envir = envir)
                       }
                   } else
                       assign(state, value[valid_index], envir = envir)

               } else if(is.data.frame(value)) {

                   assign(state, value[valid_index, ], envir = envir)

               } else if(is.list(value)) {

                   assign(state, value[valid_index], envir = envir)

               } else {
                   if(length(value) > 1)
                       assign(state, value[valid_index], envir = envir)
                   else
                       if(valid_index_len == 0) assign(state, character(0), envir = envir)
               }
           })

    return(NULL)
}

is_not_valid <- function(x) {
    if(is.numeric(x))
        !is.finite(x)
    else
        is.na(x)
}

.get <- function(x, envir = as.environment(-1), mode = "any", ifnotfound,
                 inherits = FALSE) {

    if(missing(ifnotfound))
        ifnotfound <- list(NULL)

    mget(x = x, envir = envir, mode = mode,
         ifnotfound = ifnotfound,
         inherits = inherits)[[x]]

}

seq_n <- function(n, char = "") {

    if(char == "linkingKey") char <- ""

    if(n > 0)
        paste0(char, (seq_len(n)-1))
    else character(0)
}
