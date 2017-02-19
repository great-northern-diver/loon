
l_scaleto <- function(widget, ...) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'scaleto', ...)
    invisible()
}


#' @export
l_scaleto_world <- function(widget) {
    l_scaleto(widget, "world")
}

#' @export
l_scaleto_plot <- function(widget) {
    l_scaleto(widget, "plot")
}

#' @export
l_scaleto_selected <- function(widget) {
    l_scaleto(widget, "selected")
}

#' @export
l_scaleto_active <- function(widget) {
    l_scaleto(widget, "active")
}

#' @export
l_scaleto_layer <- function(target, layer) {
    if (is(target, "l_layer")) {
        layer <- target
        target <- attr(layer, "widget")
    }
    l_scaleto(target, "layer", layer)
}

