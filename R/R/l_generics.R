
# aliased in l_cget
#' @export
`[.loon` <- function(target, state) {
    l_cget(target, state)
}

# aliased in l_configure
#' @export
`[<-.loon` <- function(target, state, value) {
    args <- list(target, value)
    names(args) <- c('target', state)
    do.call("l_configure", args)
}

# aliased in l_cget
#' @export
`[.l_compound` <- function(target, state) {
    # l_compounds are a list of plots
    # If the "state" is one of the named elements of the target
    # then the assumption is that this is what the user intends to change
    if (state %in% names(target)) {
        classNames <- class(target)
        class(target) <- "list"
        values <- target[state]
        class(target) <- classNames
    } else {
        # go to the plots
        values <- l_cget(target, state)

    }
    values
}
# aliased in l_configure
#' @export
`[<-.l_compound` <- function(target, state, value) {
    # l_compounds are a list of plots
    # If the "state" is one of the named elements of the target
    # then the assumption is that this is what the user intends to change
    if (state %in% names(target)) {
        classNames <- class(target)
        class(target) <- "list"
        target[state] <- value
        class(target) <- classNames
    } else {
        # change the state for every plot having that state
        for (plot in l_getPlots(target)){
            if (state %in% l_state_names(plot)) {
                plot[state] <- value
            }
        }

    }

    invisible(target)
}



#' @title Print a summary of a loon layer object
#'
#' @description Prints the layer label and layer type
#'
#' @param x an \code{l_layer} object
#' @template param_dots_method_not_used
#'
#' @seealso \code{\link{l_layer}}
#'
#' @export
print.l_layer <- function(x, ...) {
    cat(paste0('loon layer "', l_layer_getLabel(attr(x, 'widget'), x),
              '" of type ', l_layer_getType(attr(x, 'widget'), x),
              ' of plot ',
              attr(x, 'widget'),
              '\n', sep=''))
    print(as.vector(x))
}
