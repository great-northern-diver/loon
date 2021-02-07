#' @rdname l_plot
#' @param tk_title provides an alternative window name to Tk's \code{wm title}. If \code{NULL}, \code{stl} will be used.
#' @param linecolor line colour of all time series.
#'   Default given by \code{\link{l_getOption}("color")}.
#' @param linewidth line width of all time series (incl. original and decomposed components.
#'   Default given by \code{\link{l_getOption}("linewidth")}.
#' @param linkingGroup string giving the linkingGroup for all plots. If missing,
#' a default \code{linkingGroup} will be determined from deparsing the input \code{x}.
#'
#' @export


l_plot.decomposed.ts <- function(x, y = NULL,
                                 xlabel = NULL,  ylabel = NULL,
                                 title = NULL, tk_title = NULL,
                                 color = l_getOption("color"),
                                 size = l_getOption("size"),
                                 linecolor = l_getOption("color"),
                                 linewidth = l_getOption("linewidth"),
                                 linkingGroup,
                                 showScales = TRUE,
                                 showGuides = TRUE,
                                 showLabels = TRUE,
                                 ...
                                  ){

    if (!is.null(y)) warning("value of y argument is ignored")
    call <- match.call()

    l_plot_ts(x,
              xlabel = xlabel,  ylabel = ylabel,
              title = title, tk_title = tk_title,
              color = color, size = size,
              linecolor = linecolor, linewidth = linewidth,
              linkingGroup,
              showScales = showScales,
              showGuides = showGuides,
              showLabels = showLabels,
              call = call,
              ...
    )
}
