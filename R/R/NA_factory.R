NA_factory <- function(w, ..., envir = parent.frame()) {
    class(w) <- w
    UseMethod("NA_factory", w)
}

NA_factory.default <- function(w, ..., envir = parent.frame()) {

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
            paste0("Removed ",
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


l_nDimStateNames <- function(loon_plot = c("l_plot.default",
                                           "l_plot3D",
                                           "l_hist",
                                           "l_graph",
                                           "l_serialaxes",
                                           "l_layer_lines",
                                           "l_layer_polygons",
                                           "l_layer_texts",
                                           "l_layer_rectangles",
                                           "l_layer_points")) {

    plot_fun <- match.arg(loon_plot)

    switch(
        plot_fun,
        l_plot.default = c("itemLabel", "x", "y", "color",
                           "selected", "active", "size", "tag", "linkingKey", "glyph"),
        l_hist = c("x", "color", "selected", "active", "linkingKey"),
        l_graph = c("itemLabel","glyph" ,"linkingKey" ,"x" ,"y" ,
                    "color" , "selected" , "active" , "size" , "tag" ,
                    "nodes", "orbitAngle"),
        l_serialaxes = c("itemLabel", "linkingKey", "linewidth",
                         "data", "active", "color", "selected", "tag"),
        l_plot3D = c("itemLabel", "x", "y", "z", "color",
                     "selected", "active", "size", "tag", "linkingKey", "glyph"),
        l_layer_lines = c("x", "y", "tag", "itemLabel", "color", "active",
                          "group", "linewidth"),
        l_layer_polygons = c("x", "y", "tag", "itemLabel", "color", "active",
                             "group", "linecolor", "linewidth"),
        l_layer_texts = c("x", "y", "tag", "active", "itemLabel", "color", "text",
                          "angle", "size", "anchor", "justify"),
        l_layer_rectangles = c("x", "y", "tag", "active", "itemLabel", "color",
                               "linecolor", "linewidth"),
        l_layer_points = c("x", "y", "tag", "active", "itemLabel", "size", "color")
    )
}
