
#' @title Force a Content Redraw of a Plot
#'
#' @description Force redraw the plot to make sure that all the visual elements
#'   are placed correctly.
#'
#' @template param_widget
#'
#' @details Note that this function is intended for debugging. If you find that
#'   the display does not display the data according to its plot states then
#'   please contact loon's package maintainer.
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' p <- l_plot(iris)
#' l_redraw(p)
#'
#' }
l_redraw <- function(widget) {
    tcl(widget, "redraw")
}
