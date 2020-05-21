
#' @title Convert color names to their 12 digit hexadecimal color representation
#'
#' @aliases l_hexcolor
#'
#' @description Color names in loon will be mapped to colors according to the Tk
#'   color specifications and are normalized to a 12 digit hexadecimal color
#'   representation.
#'
#' @param color a vector with color names
#'
#' @return a character vector with the 12 digit hexadecimal color strings.
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' p <- l_plot(1:2)
#' p['color'] <- 'red'
#' p['color']
#'
#' l_hexcolor('red')
#' }
l_hexcolor <- function(color) {
    as.character(tcl('::loon::listfns::toHexcolor', color))
}
