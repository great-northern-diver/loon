
#' @title Resize Plot Widget
#'
#' @description Resizes the toplevel widget to a specific size.
#'
#' @template param_widget
#' @param width width in pixels
#' @param height in pixels
#'
#' @seealso \code{\link{l_size}}, \code{\link{l_size<-}}
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' p <- l_plot(iris)
#'
#' l_resize(p, 300, 300)
#' l_size(p) <- c(500, 500)
#'
#'}
l_resize <- function(widget, width, height) {
    tcl("::loon::resize", widget, width, height)
    invisible()
}
