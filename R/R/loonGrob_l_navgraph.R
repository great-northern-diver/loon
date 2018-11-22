
#' @rdname loonGrob
#'
#' @examples
#'
#' \dontrun{
#' ## navgraph examples
#'
#' ng <- l_navgraph(oliveAcids, separator='-', color=olive$Area)
#'
#' # To print directly use either
#' plot(ng)
#' # or
#' grid.loon(ng)
#' # or to save structure
#' lgrob <- loonGrob(ng)
#' library(grid)
#' grid.newpage()
#' grid.draw(lgrob)
#' }
#'
#' @export
loonGrob.l_navgraph <- function(target, name = NULL, gp = NULL, vp = NULL){

    widget <- target
    graph <-  widget$graph
    gTree(children = gList(loonGrob(graph, name = "l_navgraph")),
          name = if (is.null(name)) "navgraph" else name,
          gp = gp, vp = vp)
}

