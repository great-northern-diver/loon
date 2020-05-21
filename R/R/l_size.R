

#' @title Query Size of a Plot Display
#'
#' @description Get the width and height of a plot in pixels
#'
#' @template param_widget
#'
#' @return Vector width width and height in pixels
#'
#' @seealso \code{\link{l_resize}}, \code{\link{l_size<-}}
#'
#' @export
l_size <- function(widget) {
    sapply(c(tkwinfo('width', widget), tkwinfo('height', widget)), as.numeric)
}

#' @title Resize Plot Widget
#'
#' @description Resizes the toplevel widget to a specific size. This setter
#'   function uses \code{\link{l_resize}}.
#'
#' @template param_widget
#' @param value numeric vector of length 2 with width and height in pixels
#'
#'
#' @export
#'
#' @seealso \code{\link{l_resize}}, \code{\link{l_size}}
#'
#' @examples
#' if(interactive()){
#'
#' p <- l_plot(iris)
#'
#' l_resize(p, 300, 300)
#' l_size(p) <- c(500, 500)
#'
#' }
'l_size<-' <- function(widget, value) {
    tcl("::loon::resize", widget, value[1], value[2])
    invisible()
    widget
}
