#' @rdname l_plot
#'
#' @export

l_plot.stl <- function(x, y = NULL,
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
                       ...){

    if (!is.null(y)) warning("value of y argument is ignored")
    call <- match.call()

    l_plot_ts(x,
              color = color, size = size,
              linecolor = linecolor, linewidth = linewidth,
              xlabel = xlabel,  ylabel = ylabel,
              title = title, tk_title = tk_title,
              linkingGroup,
              showScales = showScales,
              showGuides = showGuides,
              showLabels = showLabels,
              call = call,
              ... )

}
