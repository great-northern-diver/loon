#' @rdname l_plot
#'
#' @seealso Density layer \code{\link{l_layer.density}}
#' @export

l_plot.density <- function(x, y = NULL, xlabel = NULL,  ylabel = NULL, title = NULL,
                           linewidth = l_getOption("linewidth"),
                           linecolor = l_getOption("color"),
                           ...){
    if (!is.null(y)) warning("value of y argument is ignored")
    density <- x
    if (is.null(linecolor)) linecolor <- l_getOption("color")
    if(is.null(ylabel)){ylabel <- paste(density$data.name, "density")}
    if(is.null(xlabel)){xlabel <- paste("N =", density$n, " ", "Bandwidth =", round(density$bw, 3))}
    if(is.null(title)){title <- paste("density of", density$data.name)}

    p <- l_plot(title = title, xlabel = xlabel, ylabel = ylabel, ...)
    l <- l_layer.density(p, density, color = linecolor, linewidth = linewidth)
    l_scaleto_world(p)
    p
}
