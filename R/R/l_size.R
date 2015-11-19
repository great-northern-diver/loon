

#' @export
l_size <- function(widget) {
    sapply(c(tkwinfo('width', widget), tkwinfo('height', widget)), as.numeric)
}

#' @export
'l_size<-' <- function(widget, value) {
    tcl("::loon::resize", widget, value[1], value[2])
    invisible()
    widget
}
