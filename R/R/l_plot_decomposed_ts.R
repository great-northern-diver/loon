#' @title The \code{\link{l_plot}} method for classical seasonal decomposition of a time series by moving averages (i.e. via \code{decompose()})
#'
#' @description Creates a decomposed time series plot based on
#' objects of class \code{decomposed.ts}
#'
#' @method l_plot decomposed.ts
#' @family two-dimensional plotting functions
#' @family time series decomposition plotting functions
#' @param x a \code{decomposed.ts} object
#' @param y  \code{NULL}, ignored
#' @param xlabel the labels for the x axes.  This is a length four character vector one for each: of the original
#' time series, the trend component, the seasonality component, and the remainder. If of length 1, the label is repeated; if \code{NULL}, \code{xlabel} is "time".
#' @param ylabel the labels for the vertical axes.  This is a length four character vector one for each: of the original
#' time series, the trend component, the seasonality component, and the remainder. If \code{NULL}, the default,
#' \code{ylabel} will be \code{c("data", "trend", "seasonality", "remainder")}; if a character vector of length 1, the label is repeated four times.
#' @param title an overall title for the entire display. If \code{NULL} (the default), the title will be "Seasonal Trend Analysis".
#' @param tk_title provides an alternative window name to Tk's \code{wm title}.  If \code{NULL}, \code{stl} will be used.
#' @param color points colour of all time series.
#'   Default given by \code{\link{l_getOption}("color")}.
#' @param size points size of all time series.
#'   Default given by \code{\link{l_getOption}("size")}.
#' @param lcolor line colour of all time series.
#'   Default given by \code{\link{l_getOption}("color")}.
#' @param linewidth line width of all time series (incl. original and decomposed components.
#'   Default given by \code{\link{l_getOption}("linewidth")}.
#' @param linkingGroup name of linking group.
#'        If \code{NULL}, one is created from the data name and class associated with \code{stlOrDecomposedTS}.
#' @param showScales a logical as to whether to display the scales on all axes, default is TRUE.
#' @param showGuides a logical as to whether to display background guide lines on all plots, default is TRUE.
#' @param showLabels a logical as to whether to display axes labels on all plots, default is TRUE.
#' @param ... keyword value pairs passed off to \code{\link{l_plot}()} which constructs each loon scatterplot component.
#'
#'
#'
#' @return A structure of class \code{"l_ts"} containing four loon plots each representing a part of the decomposition
#' by name: "original", "trend", "seasonal", and "remainder".
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' decompose <- decompose(co2)
#' p <- l_plot(decompose, title = "Atmospheric carbon dioxide over Mauna Loa")
#' # names of plots in the display
#' names(p)
#' # names of states associated with the seasonality plot
#' names(p$seasonal)
#' # which can be set
#' p$seasonal['color'] <- "steelblue"
#'
#' }


l_plot.decomposed.ts <- function(x, y = NULL,
                                 xlabel = NULL,  ylabel = NULL,
                                 title = NULL, tk_title = NULL,
                                 color = l_getOption("color"),
                                 size = l_getOption("size"),
                                 lcolor = l_getOption("color"),
                                 linewidth = l_getOption("linewidth"),
                                 linkingGroup = NULL,
                                 showScales = TRUE,
                                 showGuides = TRUE,
                                 showLabels = TRUE,
                                 ...
                                  ){

    if (!is.null(y)) warning("value of y argument is ignored")
    if (is.null(lcolor)) lcolor <- l_getOption("color")
    if (is.null(color)) color <- l_getOption("color")
    l_plot_ts(x,
              xlabel = xlabel,  ylabel = ylabel,
              title = title, tk_title = tk_title,
              color = color, size = size,
              lcolor = lcolor, linewidth = linewidth,
              linkingGroup = linkingGroup,
              showScales = showScales,
              showGuides = showGuides,
              showLabels = showLabels,
              ...
    )
}
