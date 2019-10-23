from tk import tk

def hex12tohex6(x):
    """    
    Convert 12 hexadecimal digit color representations to 6 hexidecimal digit color representations
       
    Description:
        Tk colors must be in 6 hexadecimal format with two hexadecimal
        digits for each of the red, green, and blue components.  Twelve hexadecimal digit
        colors have 4 hexadecimal digits for each.  This function converts the 12 digit format to the 6
        provided the color is preserved.

    Args:
        x: a list with 12 digit hexcolors
    Details: 
        Function throws a warning if the conversion loses information. The
        l_hexcolor function converts any Tcl color specification to a
        12 digit hexadecimal color representation.

        For other mappings see the col_numeric and col_factor functions from the scales package.

    Returns:
       a list with 6 git hexcolors 
    Examples:
        @code
            x = l_hexcolor(["red", "green", "blue", "orange"])
            print(x)
            hex12tohex6(x)
        @endcode
    """
    col1 =  "#" + x[1:3] + x[5:7] + x[9: 11]
    col2 =  "#" + x[3:5] + x[7:9] + x[11:13]
    if(col1 != col2):
        Warning('conversion of 12 digit hexadecimal color representation to a 6 digit'+
                ' hexadecimal representation lost information.')
    return col1

def color_loon():
    """    
    Create a palette with loon's color mapping
       
    Description:
        Used to map nominal data to colors. By default these colors are
        chosen so that the categories can be well differentiated visually (e.g. to
        highlight the different groups)
    Details: 
        This is the function that loon uses by default to map values to
        colors. Loon's mapping algorithm is as follows:
        - if all values already represent valid Tk colors (see
          tkcolors) then those colors are taken

        - if the number of distinct values is less than the number of values in
          loon's color mapping list then they get mapped according to the color list,
          see l_setColorList and l_getColorList.

        - if there are more distinct values than there are colors in loon's color
          mapping list then loon's own color mapping algorithm is used. See
          loon_palette and the details section in the documentation of
          l_setColorList.

        For other mappings see the col_numeric and col_factor functions from the scales package.

    Returns:
        A function that takes a vector with values and maps them to a vector
        of 6 digit hexadecimal encoded color representation (strings). Note that
        loon uses internally 12 digit hexadecimal encoded color values.  If all the
        values that get passed to the function are valid color names in Tcl then
        those colors get returned hexencoded. Otherwise, if there is one or more
        elements that is not a valid color name it uses the loons default color
        mapping algorithm.
    @see l_setColorList, l_getColorList, loon_palette
    Examples: 
        @code
            pal = color_loon()
            pal(['a','b','c','d'])
            pal(['a','a','b','c'])
            pal(['green', 'yellow'])
        @endcode
    """
    def f(x):
        if (not isinstance(type(tk.tk.call('::loon::listfns::isColor', x)), (int,float))):
            x = tk.tk.call('::loon::listfns::mapColor', x)
        hex12 = tk.tk.call('::loon::listfns::toHexcolor', x)
        return [hex12tohex6(i) for i in hex12]
    return f

def loon_palette(n):
    """    
    Loon's color generator for creating color palettes
       
    Description:
        Loon has a color sequence generator implemented  creates a color
        palettes where the first m colors of a color palette of size
        m+1 are the same as the colors in a color palette of size m,
        for all positive natural numbers m. See the details in the
        l_setColorList documentation.

    Args:
        n: numer of different colors in the palette

    Returns:
       list with hexencoded color values
    @see l_setColorList
    Examples: 
        @code
            loon_palette(12)
        @endcode
    """
    if(not isinstance(n, int)):
        exit("argument n needs to be an integer number.")
    return tk.tk.eval('::loon::hcl::hue_mem_pal ' + str(n) + ' {*}$::loon::Options(colors-palette-hcl)').split(' ')




# #' @title Convert color representations having an alpha transparency level to 6 digit color
# #'   representations
# #'
# #' @description Colors in the standard tk used by loon do not allow for alpha transparency.
# #' This function allows loon to use color palettes (e.g. \code{\link{l_setColorList}}) that
# #' produce colors with alpha transparency by simply using only the rgb.
# #'
# #' @param col a vector of colors (potentially) containing an alpha level
# #'
# #' @examples
# #' x <- l_colRemoveAlpha(rainbow(6))
# #' # Also works with ordinary color string representations
# #' # since it just extracts the rgb values from the colors.
# #' x <- l_colRemoveAlpha(c("red", "blue", "green", "orange"))
# #' x
# #'
# #' @export
# def l_colRemoveAlpha(col):
#     if(col == None):
#         exit("Please provide a vector of colours.")
#     rgb(t(col2rgb(col)), maxColorValue = 255)



#' @title Use custom colors for mapping nominal values to distinct colors
#'
#' @description Modify loon's color mapping list to a set of custom colors.
#'
#'
#' @param colors vector with valid color names or hex-encoded colors
#'
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
#'   \code{\link{tkcolors}}) then those colors are taken.
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
#'   \if{html}{\figure{loon_color_wheel.png}{options: alt="Loon's color mapping"}}
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
#' l_plot([1,2,3], color=[1,2,3]) # loon's default mapping
#'
#' cols = l_getColorList()
#' l_setColorList(["red", "blue", "green", "orange"])
#'
#' ## close and reopen inspector
#'
#' l_plot([1,2,3], color=[1,2,3])   # use the new color mapping
#' l_plot(list(range(10)), color=list(range(10))) # use loons default color mapping as color list is too small
#'
#' # reset to default
#' l_setColorList(cols)
#'
#' \dontrun{
#' # you can also perform the color mapping yourself, for example with
#' # the col_numeric function provided in the scales package
#' if (requireNamespace("scales", quietly = TRUE)) {
#'   p_custom <- with(olive, l_plot(stearic ~ oleic,
#'        color = scales::col_numeric("Greens", domain = NULL)(palmitic)))
#'   }
#' }
#'
def l_setColorList(colors):
    """    
    Use custom colors for mapping nominal values to distinct colors
       
    Description:
        Modify loon's color mapping list to a set of custom colors.

    Args:
        colors: list with valid color names or hex-encoded colors
    Details: 
        There are two commonly used mapping schemes of data values to
        colors: one scheme maps numeric values to colors on a color gradient and
        the other maps nominal data to colors that can be well differentiated
        visually (e.g. to highlight the different groups). Presently, loon always
        uses the latter approach for its color mappings. You can use specialized
        color pallettes to map continuous values to color gradients as shown in the
        examples below.
        When assigning values to a display state of type color then loon maps those
        values using the following rules
        - if all values already represent valid Tk colors (see
          tkcolors) then those colors are taken.
        - if the number of distinct values are less than number of values in
          loon's color mapping list then they get mapped according to the color list,
          see l_setColorList and l_getColorList.
        - if there are more distinct values as there are colors in loon's color
          mapping list then loon's own color mapping algorithm is used. See
          loon_palette and for more details about the algorithm below
          in this documentation.
        
        Loon's default color list is composed of the first 11 colors from the
        hcl color wheel (displayed below in the html version of the
        documentation). The letters in hcl stand for hue, chroma and luminance, and
        the hcl wheel is useful for finding "balanced colors" with the same chroma
        (radius) and luminance but with different hues (angles), see Ross Ihaka
        (2003) "Colour for presentation graphics", Proceedings of DSC, p. 2
        (https://www.stat.auckland.ac.nz/~ihaka/courses/787/color.pdf).
        
        The colors in loon's internal color list are also the default ones listed
        as the "modify color actions" in the analysis inspectors. To query and
        modify loon's color list use l_getColorList and l_setColorList.
        
        In the case where there are more unique data values than colors in loon's
        color list then the colors for the mapping are taken from different
        locations distributed on the hcl color wheel (see above).
        
        One of the advantages of using the hcl color wheel is that one can obtain
        any number of "balanced colors" with distinct hues. This is useful in
        encoding data with colors for a large number of groups; however, it should
        be noted that the more groups we have the closer the colors sampled from
        the wheel become and, therefore, the more similar in appearance.
        
        A common way to sample distinct "balanced colors" on the hcl wheel is to
        choose evenly spaced hues distributed on the wheel (i.e. angles on the
        wheel). However, this approach leads to color sets where most colors change
        when the sample size (i.e. the number of sampled colors from the wheel)
        increases by one. For loon, it is desirable to have the first m
        colors of a color sample of size m+1 to be the same as the colors in
        a color sample of size m, for all positive natural numbers m.
        Hence, we prefer to have a sequence of colors. This way, the colors on the
        inspectors stay relevant (i.e. they match with the colors of the data
        points) when creating plots that encode with color a data variable with
        different number of groups.
        
        We implemented such a color sampling scheme (or color sequence generator)
        that also makes sure that neighboring colors in the sequence have different
        hues. In you can access this color sequence generator with
        loon_palette. The color wheels below show the color
        generating sequence twice, once for 16 colors and once for 32 colors.
        
        Note, for the inspector: If there are more unique colors in the data points
        than there are on the inspectors then it is possible to add the next five
        colors in the sequence of the colors with the +5 button.
        Alternatively, the + button on the modify color part of the analysis
        inspectors allows the user to pick any additional color with a color menu.
        Also, if you change the color mapping list and close and re-open the loon
        inspector these new colors show up in the modify color list.

        When other color mappings of data values are required (e.g. numerical data
        to a color gradient) then the functions in the scales

    Returns:
       None
    @see l_setColorList, l_getColorList, l_setColorList_ColorBrewer, l_setColorList_hcl
    Examples:
        @code
            l_plot([1,2,3], color=[1,2,3]) # loon's default mapping
            cols = l_getColorList()
            l_setColorList(["red", "blue", "green", "orange"])
            ## close and reopen inspector
            l_plot([1,2,3], color=[1,2,3])   # use the new color mapping
            l_plot(list(range(10)), color=list(range(10))) # use loons default color mapping as color list is too small
            # reset to default
            l_setColorList(cols)
        @endcode
    """
    tk.tk.call('::loon::setColorList', 'custom', colors)
    
def l_getColorList():
    """    
    Get loon's color mapping list
       
    Description:
        The color mapping list is used by loon to convert nominal values
        to color values, see the documentation for l_setColorList.

    Returns:
       a vector with hex-encoded colors
    @see l_setColorList
    Examples: 
        @code
            l_getColorList()
        @endcode
    """
    return tk.tk.eval('::loon::getColorList').split(' ')




# #' @templateVar type ColorBrewer
# #' @template man_setColorList
# #'
# #'
# #' @param palette one of the following RColorBrewer palette name: Set1, Set2,
# #'   Set3, Pastel1, Pastel2, Paired, Dark2, or Accent
# #'
# #' @details Only the following palettes in ColorBrewer are available: Set1,
# #'   Set2, Set3, Pastel1, Pastel2, Paired, Dark2, and Accent. See the examples
# #'   below.
# #'
# #' @export
# #'
# #' @examples
# #'
# #' \dontrun{
# #' if (requireNamespace("RColorBrewer", quietly = TRUE)) {
# #'   RColorBrewer::display.brewer.all()
# #' }
# #' }
# #'
# #' l_setColorList_ColorBrewer("Set1")
# #' p <- l_plot(iris)
# l_setColorList_ColorBrewer <- function(palette=c("Set1", "Set2", "Set3",
#                                                  "Pastel1", "Pastel2", "Paired",
#                                                  "Dark2", "Accent")) {
#     palette <- match.arg(palette)
#     tcl('::loon::setColorList', 'ColorBrewer', palette)
#     invisible()
# }


# #' @templateVar type hcl color wheen
# #' @template man_setColorList
# #'
# #'
# #' @param chroma The chroma of the color. The upper bound for chroma depends on
# #'   hue and luminance.
# #' @param luminance A value in the range [0,100] giving the luminance of the
# #'   colour. For a given combination of hue and chroma, only a subset of this
# #'   range is possible.
# #' @param hue_start The start hue for sampling. The hue of the color specified
# #'   as an angle in the range [0,360]. 0 yields red, 120 yields green 240 yields
# #'   blue, etc.
# #'
# #' @details Samples equally distant colors from the hcl color wheel. See the
# #'   documentation for \code{\link[grDevices]{hcl}} for more information.
# #'
# #'
# #' @export
# l_setColorList_hcl <- function(chroma=56, luminance=51, hue_start=231) {
#     tcl('::loon::setColorList', 'hcl', chroma, luminance, hue_start)
#     invisible()
# }

# #' @templateVar type ggplot2
# #' @template man_setColorList
# #'
# #' @export
# l_setColorList_ggplot2 <- function() {
#     tcl('::loon::setColorList', 'ggplot2')
#     invisible()
# }

# #' @templateVar type base R
# #' @template man_setColorList
# #'
# #' @export
# l_setColorList_baseR <- function() {
#     tcl('::loon::setColorList', 'baseR')
#     invisible()
# }


# #' @templateVar type loon defaults
# #' @template man_setColorList
# #'
# #' @export
# l_setColorList_loon <- function() {
#     tcl('::loon::setColorList', 'loon')
#     invisible()
# }
