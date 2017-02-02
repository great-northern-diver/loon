#' @title Create a palette with loon's color mapping
#'   
#' @description Used to map nominal data to colors that can be well 
#'   differentiated visually (e.g. to highlight the different groups)
#'   
#' @details This is the function that loon uses by default to map values to
#'   colors. Hence, if all values already represent colors they do not get
#'   mapped to the sequence of colors from the hcl color wheel as outlined
#'   below.
#'   
#'   loon currently uses its own mapping algorithm based on the hcl color wheel.
#'   For loon, it is desirable to have the first m colors of a color sample of 
#'   size m + 1 to be the same as the colors in a color sample of size m, for 
#'   all positive natural numbers m. Hence, loon's color mapping provides a 
#'   sequence of colors.
#'   
#'   For other mappings see the \code{\link[scales]{col_numeric}} and 
#'   \code{\link[scales]{col_factor}} functions from the scales package.
#'   
#'   
#' @return A function that takes a vector with values and maps them to a vector 
#'   of hexencoded colors (strings). If all the values that get passed to the 
#'   function are valid color names in Tcl then those colors get returned 
#'   hexencoded. Otherwise, if there is one or more elements that is not a valid
#'   color name it uses the loons default color mapping algorithm.
#'   
#' @seealso \code{\link{l_getColorList}} , \code{\link{l_setColorList}}
#'   
#' @export
#' 
#' @examples 
#' 
#' pal <- color_loon()
#' pal(letters[1:4]) 
#' pal(c('a','a','b','c'))
#' pal(c('green', 'yellow'))
#' 
color_loon <- function() {
    function(x) {
        if (!as.numeric(tcl('::loon::listfns::isColor', x))) {
            x <- tcl('::loon::listfns::mapColor', x)  
        } 
        as.character(tcl('::loon::listfns::toHexcolor', x))    
    }
}


#' @title Modify the colors used for mapping nominal values to distinct colors
#'   
#' @description When assigning values to a color state of a plot widget then, if
#'   all the values are valid (Tcl) color names, these colors get assigned
#'   to
#'   
#' @param colors vecor with valid color names or hex-encoded colors
#'   
#' @return NULL
#'   
#' @export
l_setColorList <- function(colors) {
    tcl('::loon::setColorList', 'custom', colors)
    invisible()   
}


#' @export
l_getColorList <- function() {
    as.character(tcl('::loon::getColorList'))
}


#' ColorBrewer
#' @export
#' 
#' @examples 
#' l_setColorList_ColorBrewer("Set1")
#' p <- l_plot(iris)
l_setColorList_ColorBrewer <- function(palette=c("Set1", "Set2", "Set3",
                                                    "Pasetl1", "Pastel2", "Paired",
                                                    "Dark2", "Accent")) {
    palette <- match.arg(palette)
    tcl('::loon::setColorList', 'ColorBrewer', palette)
    invisible()    
}


#' @export
l_setColorList_hcl <- function(chroma=56, luminance=51, hue_start=231) {
    tcl('::loon::setColorList', 'hcl', chroma, luminance, hue_start)
    invisible()
}

#' @export
l_setColorList_ggplot2 <- function() {
    tcl('::loon::setColorList', 'ggplot2')
    invisible()
}

#' @export
l_setColorList_baseR <- function() {
    tcl('::loon::setColorList', 'baseR')
    invisible()
}