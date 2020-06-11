#' @title The \code{l_plot} method for \code{density} objects.
#'
#' @description Creates an loon plot displaying contours
#'              of the supplied (kernel) density estimate
#'
#' @method l_plot density
#' @family two-dimensional plotting functions
#' @param x a \code{density} object
#' @param y \code{NULL},  ignored.
#' @param xlabel the graphical parameter xlabel labelling the x axis of the plot. If \code{NULL} (the default), an xlabel is created
#' based on the information available from the \code{density} objects.
#' @param ylabel the graphical parameter ylabel labelling the y axis of the plot. If \code{NULL} (the default), an ylabel is created
#' based on the combination of data name and "density".
#' @param title providing a title for the plot. If \code{NULL} (the default), the title will be the call which produced the result.
#' @param linewidth line width.
#' Default is given by \code{\link{l_getOption}("linewidth")}.
#' @param color line colour.
#' Default is given by \code{\link{l_getOption}("color")}.
#' @param size points size.
#' Default is given by \code{\link{l_getOption}("size")}.
#' @param ... named arguments being states passed to l_plot()
#'
#' @examples
#' if(interactive()){
#'
#' ds <- density(faithful$eruptions)
#' p <- l_plot(ds, color = "steelblue")
#'
#' }
#'
#' @seealso \code{\link{l_layer.density}}
#' @export

l_plot.density <- function(x, y = NULL, xlabel = NULL,  ylabel = NULL, title = NULL,
                           linewidth = l_getOption("linewidth"),
                           color = l_getOption("color"),
                           size = l_getOption("size"),
                           ...){
    if (!is.null(y)) warning("value of y argument is ignored")
    density <- x
    if (is.null(color)) color <- l_getOption("color")
    if(is.null(ylabel)){ylabel <- paste(density$data.name, "density")}
    if(is.null(xlabel)){xlabel <- paste("N =", density$n, " ", "Bandwidth =", round(density$bw, 3))}
    if(is.null(title)){title <- paste("density of", density$data.name)}

    p <- l_plot(title = title, xlabel = xlabel, ylabel = ylabel, ...)
    l <- l_layer.density(p, density, color = color, linewidth = linewidth)
    l_scaleto_world(p)
    p
}
