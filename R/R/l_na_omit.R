l_na_omit <- function(w, args,
                      n_dim_states = NULL) {

    is_not_valid <- function(x) {
        if(is.numeric(x))
            !is.finite(x)
        else
            is.na(x)
    }

    if(is.null(n_dim_states)) n_dim_states <- l_nDimStateNames(w)

    # what is the n?
    x <- args[["x"]]
    n <- if(is.null(x)) {
        # serialaxes?
        x <- args[["data"]]
        if(is.null(x)) integer(0) else dim(x)[1]
    } else {
        length(x)
    }

    ## NA index
    no_valid_index <- lapply(n_dim_states,
                             function(state) {

                                 value <- args[[state]]

                                 if(is.null(value)) {
                                     NULL
                                 } else if(is.data.frame(value)) {

                                     if(dim(value)[1] != n)
                                         stop(state, " has unexpected length")

                                     which(apply(value, 1, function(x) any(is_not_valid(x))))
                                 } else if(is.list(value)) {

                                     which(
                                         vapply(value,
                                                function(v) {
                                                    any(is_not_valid(v))
                                                }, logical(1))
                                     )
                                 } else {
                                     if(length(value) > 1) {

                                         if(length(value) != n)
                                             stop(state, " has unexpected length")

                                         which(is_not_valid(value))
                                     }
                                 }
                             })

    no_valid_index <- unique(unlist(no_valid_index))

    # Give a warning if any missing values appear
    num_missing <- length(no_valid_index)
    if(num_missing > 0) {
        warning(
            paste0("Removed {",
                   paste0(sort(no_valid_index), collapse = ", "),
                   "}  as the ",
                   num_missing,
                   " ",
                   if (num_missing == 1) {
                       "observation which contains"
                   } else {
                       "observations which contain"
                   },
                   " ",
                   "missing values."),
            call. = FALSE
        )
    }

    valid_index <- setdiff(seq_len(n), no_valid_index)

    # remove parent environment NA
    valid_index_len <- length(valid_index)
    if(valid_index_len == 0)
        warning("No valid input", call. = FALSE)

    seq_n <- function(n, char = "") {

        if(char == "linkingKey") char <- ""
        if(char == "xTemp") return(NULL)
        if(char == "yTemp") return(NULL)

        if(n > 0)
            paste0(char, (seq_len(n)-1))
        else character(0)
    }

    lapply(n_dim_states,
           function(state) {

               value <- args[[state]]

               if(is.null(value)) {

                   # state is defined in ..., like linkingKey, tag, itemLabel, etc
                   if(!state %in% formalArgs(w))
                       args[[state]] <<- seq_n(n, char = state)[valid_index]

               } else if(is.data.frame(value)) {

                   args[[state]] <<- value[valid_index, ]

               } else if(is.list(value)) {

                   args[[state]] <<- value[valid_index]

               } else {
                   if(length(value) > 1)
                       args[[state]] <<- value[valid_index]
                   else
                       if(valid_index_len == 0) args[[state]] <<- character(0)
               }
           })

    # remove NULL
    args <- Filter(Negate(is.null), args)

    return(args)
}
