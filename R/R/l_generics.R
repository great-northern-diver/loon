
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
    # then the assumption is that this is what the user intends to access
    if (state %in% names(target)) {
        # want to use the list accessor []
        class(target) <- "list"
        values <- target[state]
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
        # want to use the list assignment [<-
        class(target) <- "list"
        target[state] <- value
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


#' @export
`[.l_savedStates` <- function(target, state) {
    # l_savedStates may be a list of plots or plot info states
    # If the "state" is one of the named elements of the target
    # then the assumption is that this is what the user intends to access
    # This trys to mimic the functionality of l_compound's []
    if (state %in% names(target)) {
        # want to use the list accessor []
        class(target) <- "list"
        values <- target[state]
        # want to use the list accessor []
        class(values) <- "list"
        values <- values[[1]]
    } else {
        if ("l_compound" %in% attr(target, "l_plot_class")){
            # go to the plots
            # want to use the list accessor []
            class(target) <- "list"
            values <- lapply(seq_along(names(target)),
                             FUN = function(plot_i){
                                 plot_saved_state <- target[[plot_i]]
                                 result <- plot_saved_state[[state]]
                                 if (is.null(result)){
                                     warning("\"", state, "\"",
                                             " was not found in saved states of plot ",
                                             names(target)[[plot_i]],
                                             ", returning NULL",
                                             call. = FALSE)
                                 }
                                 result
                             }
            )
            names(values) <- names(target)

        } else {
            # There are no further plots to search, so state is missing
            # Return NULL
            values <- NULL
            warning("\"", state, "\"",
                    " was not found in the saved states, returning NULL",
                    call. = FALSE)
        }
    }
    values
}

#' @export
`[<-.l_savedStates` <- function(target, state, value) {
    # l_savedStates are a list of plot states or lists of l_savedStates
    # If the "state" is one of the named elements of the target
    # then the assumption is that this is what the user intends to change
    if (state %in% names(target)) {
        # want to use the list assignment [<-
        classNames <- class(target)
        class(target) <- "list"
        target[state][[1]] <- value
        class(target) <- classNames
    } else {
        # change the state for every plot having that state
        notAssignedAnywhere <- TRUE
        for (plot_i in seq_along(names(target))){
            plot_saved_states <- target[[plot_i]]
            if (state %in% names(plot_saved_states)) {
                target[[plot_i]][state] <- value
                notAssignedAnywhere <- FALSE
            }
        }
        if (notAssignedAnywhere){
            stop("\"", state, "\"", " is not found anywhere in the saved states. ")
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
