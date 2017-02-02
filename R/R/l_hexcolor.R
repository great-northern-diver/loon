
#' @title Convert color names to their 12 digit hexadecimal color representation
#'   
#' @description Color names in loon will be mapped to colors according to the Tk
#'   color specifications and are normalized to a 12 digit hexadecimal color 
#'   representation. This function maps Tk color names to 
#'   
#'   
#'   Colors in color states in loon are normalized usto a 12 digit hexadecimal 
#'   color representatio
#'   
#'   
#' @export
#' 
#' @examples 
#' p <- l_plot(1:2)
#' p['color'] <- 'red'
#' p['color']
#' 
#' l_hexcolor('red')
l_hexcolor <- function(colors) {
    as.character(tcl('::loon::listfns::toHexcolor', colors))
}
