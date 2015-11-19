
#' @export
l_setLinkedStates <- function(widget, states) {
    invisible(as.character(tcl(widget, 'setLinkedStates', states)))
}

#' @export
l_getLinkedStates <- function(widget) {
    as.character(tcl(widget, 'getLinkedStates'))
}
