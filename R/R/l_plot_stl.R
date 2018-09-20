#' @title l_plot method for seasonally decomposed by loess time series objects (i.e. via stl)
#'
#' @description a generic function for creating a decomposed time series plot based on 
#' objects of class \code{stl} 
#'
#' @inheritParams l_plot
#' @param x  an \code{stl} object
#' @param y  NULL, ignored
#' @param pcolor points colour of all time series. If NULL (the default) \code{lcolor} will be \code{l_getOption("foreground")}.
#' @param size points size of all time series. Default value is 1.
#' @param lcolor line colour of all time series. If NULL (the default) \code{lcolor} will be \code{l_getOption("foreground")}.
#' @param linewidth line width of all time series (incl. original and decomposed components. Default is 1.
#' @param xlabels the labels for the x axes: a length four character vector for each of the original 
#' time series, trend, seasonality and remainder. If \code{NULL}, xlabels is created as "time".
#' @param ylabels the labels for the vertical axes: a length four character vector for each of the original 
#' time series, trend, seasonality and remainder. If \code{NULL}, ylabels is created based on the information provided.
#' @param title an overall title for the entire display. If \code{NULL} (the default), the title will be "Seasonal Trend Analysis".
#' @param tk_title provides an alternative window name to Tk's \code{wm title}.  If \code{NULL}, \code{stl} will be used.
#' @param linkingGroup name of linking group.  
#'        If NULL, one is created from the data name and class associated with \code{stlOrDecomposedTS}.
#' @param showScales a logical as to whether to display the scales on all axes, default is TRUE.
#' @param showGuides a logical as to whether to display background guide lines on all plots, default is TRUE.
#' @param showLabels a logical as to whether to display axes labels on all plots, default is TRUE.
#' @param ... keyword value pairs passed off to \code{l_plot()} which constructs each loon scatterplot component.
#' 
#' @return A structure of class \code{"l_ts"} containing four loon plots each representing a part of the decomposition
#' by name: "original", "trend", "seasonal", and "remainder".
#'
#' @export
#' 
#' @examples
#' co2_stl <- stl(co2, "per")
#' p <- l_plot(co2_stl, title = "Atmospheric carbon dioxide over Mauna Loa")
#' # names of plots in the display
#' names(p)
#' # names of states associated with the seasonality plot
#' names(p$seasonal)
#' # which can be set
#' p$seasonal['color'] <- "steelblue"



l_plot.stl <- function(x, y = NULL,
                       pcolor = NULL, size = 1,
                       lcolor = NULL, linewidth = 1, 
                       xlabels = NULL,  ylabels = NULL, 
                       title = NULL, tk_title = NULL, 
                       linkingGroup = NULL,
                       showScales = TRUE,
                       showGuides = TRUE, 
                       showLabels = TRUE,
                       ...){
    if (!is.null(y)) warning("value of y argument is ignored")
    if (is.null(lcolor)) lcolor <- l_getOption("foreground")
    if (is.null(pcolor)) pcolor <- l_getOption("foreground")
    l_plot_ts(x,
              pcolor = pcolor, size = size,
              lcolor = lcolor, linewidth = linewidth, 
              xlabels = xlabels,  ylabels = ylabels, 
              title = title, tk_title = tk_title, 
              linkingGroup = linkingGroup,
              showScales = showScales,
              showGuides = showGuides, 
              showLabels = showLabels,
              ... )
    
}
