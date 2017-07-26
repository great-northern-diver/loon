#' @title Create a palette with loon's color mapping
#'   
#' @description Used to map nominal data to colors. By default these colors are 
#'   chosen so that the categories can be well differentiated visually (e.g. to 
#'   highlight the different groups)
#'   
#'   
#'   
#' @details This is the function that loon uses by default to map values to 
#'   colors. Loon's mapping algorithm is as follows:
#'   
#'   \enumerate{
#'   
#'   \item if all values already represent valid Tk colors (see
#'   \code{\link{tkcolors}}) then those colors are taken
#'   
#'   \item if the number of distinct values are less than number of values in 
#'   loon's color mapping list then they get mapped according to the color list,
#'   see \code{\link{l_setColorList}} and \code{\link{l_getColorList}}.
#'   
#'   \item if there are more distinct values as there are colors in loon's color
#'   mapping list then loon's own color mapping algorithm is used. See 
#'   \code{\link{loon_palette}} and the details section in the documentation of 
#'   \code{\link{l_setColorList}}.
#'   
#'   }
#'   
#'   For other mappings see the \code{\link[scales]{col_numeric}} and 
#'   \code{\link[scales]{col_factor}} functions from the scales package.
#'   
#'   
#' @return A function that takes a vector with values and maps them to a vector 
#'   of 6 digit hexadecimal encoded color representation (strings). Note that 
#'   loon uses internally 12 digit hexadecimal encoded color values.  If all the
#'   values that get passed to the function are valid color names in Tcl then 
#'   those colors get returned hexencoded. Otherwise, if there is one or more 
#'   elements that is not a valid color name it uses the loons default color 
#'   mapping algorithm.
#'   
#' @seealso \code{\link{l_setColorList}}, \code{\link{l_getColorList}}, 
#'   \code{\link{loon_palette}}
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
#' # show color choices for different n's
#' library(grid)
#' grid.newpage()
#' pushViewport(plotViewport())
#' grid.rect()
#' n <- 2^(1:5)
#' pushViewport(dataViewport(xscale=c(0, max(n)+1), yscale=c(0, length(n)+1)))
#' grid.yaxis(at=c(1:length(n)), label=paste("n =", n))
#' for (i in rev(seq_along(n))) {
#'  cols <- pal(1:n[i])
#'  grid.points(x = 1:n[i], y = rep(i, n[i]), default.units = "native", pch=15, gp=gpar(col=cols))
#' }
#' grid.text("note the fist i colors are shared for each n" , y=unit(1,"npc")+unit(1, "line"))
color_loon <- function() {
    function(x) {
        
        if (!as.numeric(tcl('::loon::listfns::isColor', x))) {
            x <- tcl('::loon::listfns::mapColor', x)  
        } 
        
        hex12 <- as.character(tcl('::loon::listfns::toHexcolor', x))
        hex12tohex6(hex12)
    }
}



#' @title Loon's color generator for creating color palettes
#'   
#' @description Loon has a color sequence generator implemented  creates a color
#'   palettes where the first \code{m} colors of a color palette of size 
#'   \code{m+1} are the same as the colors in a color palette of size \code{m}, 
#'   for all positive natural numbers \code{m}. See the details in the
#'   \code{\link{l_setColorList}} documentation.
#' 
#' @param n numer of different colors in the palette  
#' 
#' @return vector with hexencoded color values
#' 
#' @export
#' 
#' @seealso \code{\link{l_setColorList}}
#' 
#' @examples 
#' loon_palette(12)
loon_palette <- function(n) {
    if (length(n) != 1 && !is.numeric(n))
        stop("argument n needs to be numeric and of length 1")
    
    as.character(
        .Tcl(paste(
            '::loon::hcl::hue_mem_pal', n, '{*}$::loon::Options(colors-palette-hcl)'
        ))
    )
}



# @title Convert 12 digit color representations to 6 digit color
#   representations
# 
# @description 
# 
# @param x a vector with 12digit hexcolors
#    
# @details Function throws a warning if the conversion looses information. The
#   \code{\link{l_hexcolor}} function converts any Tcl color specification to a
#   12 digit hexadecimal color representation.
#       
# @examples 
# x <- l_hexcolor(c("red", "blue", "green", "orange"))
# hex12tohex6(x)
hex12tohex6 <- function(x) {
    # convert to hex6
    col1 <- paste0( "#", substr(x, 2, 3), substr(x, 6, 7), substr(x, 10, 11))
    col2 <- paste0( "#", substr(x, 4, 5), substr(x, 8, 9), substr(x, 12, 13))
    if (!identical(col1, col2)) {
        warning(paste("conversion of 12 digit hexadecimal color representation to",
                      "a 6 digit hexadecimal representation lost information."))
    }
    col1
}

#' @title Use custom colors for mapping nominal values to distinct colors
#'   
#' @description Modify loon's color mapping list to a set of custom colors.
#'   
#' @details There are two commonly used mapping schemes of data values to 
#'   colors: one scheme maps numeric values to colors on a color gradient and 
#'   the other maps nominal data to colors that can be well differentiated 
#'   visually (e.g. to highlight the different groups). Presently, loon always 
#'   uses the latter approach for its color mappings. You can use specialized 
#'   color pallettes to map continuous values to color gradients as shown in the
#'   examples below.
#'   
#'   When assigning values to a display state of type color then loon maps those
#'   values using the following rules
#'   
#'   \enumerate{
#'   
#'   \item if all values already represent valid Tk colors (see 
#'   \code{\link{tkcolors}}) then those colors are taken
#'   
#'   \item if the number of distinct values are less than number of values in 
#'   loon's color mapping list then they get mapped according to the color list,
#'   see \code{\link{l_setColorList}} and \code{\link{l_getColorList}}.
#'   
#'   \item if there are more distinct values as there are colors in loon's color
#'   mapping list then loon's own color mapping algorithm is used. See 
#'   \code{\link{loon_palette}} and for more details about the algorithm below 
#'   in this documentation.
#'   
#'   }
#'   
#'   Loon's default color list is composed of the first 11 colors from the 
#'   \emph{hcl} color wheel (displayed below in the html version of the 
#'   documentation). The letters in hcl stand for hue, chroma and luminance, and
#'   the hcl wheel is useful for finding "balanced colors" with the same chroma 
#'   (radius) and luminance but with different hues (angles), see Ross Ihaka 
#'   (2003) "Colour for presentation graphics", Proceedings of DSC, p. 2 
#'   (\url{https://www.stat.auckland.ac.nz/~ihaka/courses/787/color.pdf}).
#'   
#'   The colors in loon's internal color list are also the default ones listed 
#'   as the "modify color actions" in the analysis inspectors. To query and 
#'   modify loon's color list use \code{\link{l_getColorList}} and 
#'   \code{\link{l_setColorList}}.
#'   
#'   In the case where there are more unique data values than colors in loon's 
#'   color list then the colors for the mapping are taken from different 
#'   locations distributed on the hcl color wheel (see above).
#'   
#'   One of the advantages of using the hcl color wheel is that one can obtain 
#'   any number of "balanced colors" with distinct hues. This is useful in 
#'   encoding data with colors for a large number of groups; however, it should 
#'   be noted that the more groups we have the closer the colors sampled from 
#'   the wheel become and, therefore, the more similar in appearance.
#'   
#'   A common way to sample distinct "balanced colors" on the hcl wheel is to 
#'   choose evenly spaced hues distributed on the wheel (i.e. angles on the 
#'   wheel). However, this approach leads to color sets where most colors change
#'   when the sample size (i.e. the number of sampled colors from the wheel) 
#'   increases by one. For loon, it is desirable to have the first \code{m} 
#'   colors of a color sample of size \code{m+1} to be the same as the colors in
#'   a color sample of size \code{m}, for all positive natural numbers \code{m}.
#'   Hence, we prefer to have a sequence of colors. This way, the colors on the 
#'   inspectors stay relevant (i.e. they match with the colors of the data 
#'   points) when creating plots that encode with color a data variable with 
#'   different number of groups.
#'   
#'   We implemented such a color sampling scheme (or color sequence generator) 
#'   that also makes sure that neighboring colors in the sequence have different
#'   hues. In you can access this color sequence generator with 
#'   \code{\link{loon_palette}}. The color wheels below show the color 
#'   generating sequence twice, once for 16 colors and once for 32 colors.
#'   
#'   \if{html}{\figure{loon_color_wheel.png}{options: alt="Loon's color 
#'   mapping"}}
#'   
#'   Note, for the inspector: If there are more unique colors in the data points
#'   than there are on the inspectors then it is possible to add the next five 
#'   colors in the sequence of the colors with the \emph{+5} button. 
#'   Alternatively, the \emph{+} button on the modify color part of the analysis
#'   inspectors allows the user to pick any additional color with a color menu. 
#'   Also, if you change the color mapping list and close and re-open the loon 
#'   inspector these new colors show up in the modify color list.
#'   
#'   When other color mappings of data values are required (e.g. numerical data 
#'   to a color gradient) then the functions in the \code{\link[scales]{scales}}
#'   R package provide various mappings including mappings for qualitative, 
#'   diverging and sequential values.
#'   
#' @param colors vecor with valid color names or hex-encoded colors
#'   
#' @return NULL
#'   
#' @seealso \code{\link{l_setColorList}}, \code{\link{l_getColorList}}, 
#'   \code{\link{l_setColorList_ColorBrewer}}, \code{\link{l_setColorList_hcl}},
#'   \code{\link{l_setColorList_baseR}}
#'   
#' @export
#' 
#' @examples 
#' 
#' l_plot(1:3, color=1:3) # loon's default mapping
#' 
#' cols <- l_getColorList()
#' l_setColorList(c("red", "blue", "green", "orange"))
#' 
#' ## close and reopen inspector
#' 
#' l_plot(1:3, color=1:3)   # use the new color mapping
#' l_plot(1:10, color=1:10) # use loons default color mapping as color list is too small
#'  
#' # reset to default
#' l_setColorList(cols)
#' 
#' \dontrun{
#' # you can also perform the color mapping yourself, for example with
#' # the col_numeric function provided in the scales package
#' library(scales)
#' p_custom <- with(olive, l_plot(stearic ~ oleic,
#'      color = col_numeric("Greens", domain = NULL)(palmitic)))
#' }
#' 
l_setColorList <- function(colors) {
    tcl('::loon::setColorList', 'custom', colors)
    invisible()   
}


#' @title Get loon's color mapping list
#'   
#' @description The color mapping list is used by loon to convert nominal values
#'   to color values, see the documentation for \code{\link{l_setColorList}}.
#' 
#' @return a vector with hex-encoded colors
#' 
#' @seealso \code{\link{l_setColorList}}
#' 
#' @export
l_getColorList <- function() {
    as.character(tcl('::loon::getColorList'))
}



#' @templateVar type ColorBrewer
#' @template man_setColorList
#'   
#'   
#' @param palette one of the following RColorBrewer palette name: Set1, Set2,
#'   Set3, Pasetl1, Pastel2, Paired, Dark2, or Accent
#'   
#' @details Only the following palettes in ColorBrewer are available: Set1, 
#'   Set2, Set3, Pasetl1, Pastel2, Paired, Dark2, and Accent. See the examples 
#'   below.
#'   
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' library(RColorBrewer)
#' display.brewer.all()
#' }
#' 
#' l_setColorList_ColorBrewer("Set1")
#' p <- l_plot(iris)
l_setColorList_ColorBrewer <- function(palette=c("Set1", "Set2", "Set3",
                                                    "Pasetl1", "Pastel2", "Paired",
                                                    "Dark2", "Accent")) {
    palette <- match.arg(palette)
    tcl('::loon::setColorList', 'ColorBrewer', palette)
    invisible()    
}


#' @templateVar type hcl color wheen
#' @template man_setColorList
#'   
#'   
#' @param chroma The chroma of the color. The upper bound for chroma depends on 
#'   hue and luminance.
#' @param luminance A value in the range [0,100] giving the luminance of the 
#'   colour. For a given combination of hue and chroma, only a subset of this 
#'   range is possible.
#' @param hue_start The start hue for sampling. The hue of the color specified 
#'   as an angle in the range [0,360]. 0 yields red, 120 yields green 240 yields
#'   blue, etc.
#'   
#' @details Samples equally distant colors from the hcl color wheel. See the
#'   documentation for \code{\link[grDevices]{hcl}} for more information.
#'   
#'   
#' @export
l_setColorList_hcl <- function(chroma=56, luminance=51, hue_start=231) {
    tcl('::loon::setColorList', 'hcl', chroma, luminance, hue_start)
    invisible()
}

# 
l_setColorList_ggplot2 <- function() {
    tcl('::loon::setColorList', 'ggplot2')
    invisible()
}

#' @templateVar type base R
#' @template man_setColorList
#' 
#' @export
l_setColorList_baseR <- function() {
    tcl('::loon::setColorList', 'baseR')
    invisible()
}