#' @rdname l_plot
#' @export
#'
#' @seealso Map layer \code{\link{l_layer}}, \code{\link{l_layer.map}},
#'   \code{\link[maps]{map}}

l_plot.map <-  function(x, y = NULL, ...) {

    p <- l_plot.default()
    l_layer.map(p, x, label="Map",  ...)
    l_scaleto_world(p)
    p
}
