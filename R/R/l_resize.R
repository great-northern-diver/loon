
#' @export
l_resize <- function(widget, width, height) {
    tcl("::loon::resize", widget, width, height)
    invisible()
}
