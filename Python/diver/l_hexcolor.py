from .tk import tk
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
#' p <- l_plot(1:2)
#' p['color'] <- 'red'
#' p['color']
#' 
#' l_hexcolor('red')
def l_hexcolor(color):
    """Convert color names to their 12 digit hexadecimal color representation
       
    Color names in loon will be mapped to colors according to the Tk
    color specifications and are normalized to a 12 digit hexadecimal color 
    representation.
    
    Args:
        color: a list with color names
    
    Returns:
       a character vector with the 12 digit hexadecimal color strings.

    Examples: 
        >>> p = l_plot([1,2])
        >>> p['color'] = 'red'
        >>> p['color']
        >>> l_hexcolor('red')
    """
    return tk.tk.call('::loon::listfns::toHexcolor', color)
