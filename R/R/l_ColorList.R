#' Similar to col_numeric and col_factor from the scales package
#' @export
col_loon <- function() {
    function(x) {
        if (!as.numeric(tcl('::loon::listfns::isColor', x))) {
            x <- tcl('::loon::listfns::mapColor', x)  
        } 
        as.character(tcl('::loon::listfns::toHexcolor', x))    
    }
}


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