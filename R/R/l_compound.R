
#'@export
names.l_compound <- function(x) {attr(x, "names")}

#' For the target compound loon plot, determines all the loon plots
#' in that compound plot.
#'
#' @param target the (compound) loon  plot to be laid out.
#' @return a list of the named arguments and their values to be passed to
#' `gridExtra::arrangeGrob()`.
#'
#' @export
l_getPlots <- function(target) {
    UseMethod("l_getPlots", target)
}

#' @export
l_getPlots.default <- function(target) {
    stop("l_getPlots.default has no valid inheritance; not an l_compound plot")
}

#' @export
l_getPlots.l_compound <- function(target){
    stop("l_getPlots.l_compound has no valid inheritance; needs to be specialized for each type of l_compound")
}

#' @export
l_getPlots.loon <- function(target) {
    if(l_isLoonWidget(target)) {
        list(target)
    } else {
        l_throwErrorIfNotLoonWidget(target)
    }
}

#' For the target compound loon plot, determines location (only and excluding the grobs)
#' arguments to pass to `gridExtra::arrangeGrob()`
#'
#' @param target the (compound) loon  plot whose locations are needed lay it out.
#' @return a list of an appropriate subset of the named location arguments
#' `c("ncol", "nrow", "layout_matrix", "heights", "widths")` and their values
#'  to be passed to `gridExtra::arrangeGrob()`.
#'
#' @export
l_getLocations <- function(target) {
    UseMethod("l_getLocations", target)
}

#' @export
l_getLocations.default <- function(target) {
    stop("l_getLocations.default has no valid inheritance; not an l_compound plot")
}

#' @export
l_getLocations.l_compound <- function(target) {
    stop("l_getLocations.l_compound  has no valid inheritance; needs to be specialized for each type of l_compound")
}

#' @export
l_getLocations.loon <- function(target) {
    if(l_isLoonWidget(target)) {
        matrix(1)
    } else {
        l_throwErrorIfNotLoonWidget(target)
    }
}


#' @export
l_cget.l_compound <- function(target, state) {

    plotNames <- names(target)
    plots <- lapply(plotNames,
                    function(plotName) {
                        target[[plotName]]

                    })
    values <- lapply(plots,
                     function(plot, s) {
                         if(s %in% l_state_names(plot)){
                             l_cget(plot, s)
                         } else {
                             NA
                         }
                         }, state)
    names(values) <- plotNames
    values

}


#' @export
l_configure.l_compound <- function(target, ...) {

    args <- list(...)
    states <- names(args)
    if (is.null(states) || any("" %in% states))
        stop("configuration needs key=value pairs")

    plotNames <- names(target)
    plots <- lapply(plotNames,
                    function(plotName) {
                        target[[plotName]]

                    })
    for (state in states) {

        switch(
            state,
            linkingGroup = lapply(plots, l_configure,
                                  linkingGroup = args$linkingGroup, sync = "pull"),
            selected = stop("not implemented yet"),
            stop("state ", state, " not implemented")
        )
    }

    target
}

#' @export
l_info_states.l_compound <- function(target, states = "all") {

    plots <- l_getPlots(target)
    plotNames <- names(plots)
    values <- lapply(plots, l_info_states, states)
    names(values) <- plotNames
    values

}
