
#' @rdname loonGrob
#' 
#' @examples 
#' 
#' \dontrun{
#' ## navgraph examples
#' 
#' ng <- l_navgraph(oliveAcids, separator='-', color=olive$Area)
#' 
#' library(grid)
#' navGrob <- loonGrob(ng)
#' grid.newpage()
#' grid.draw(navGrob)
#' }
#' 
#' @export
loonGrob.l_navgraph <- function(target, name = NULL, gp = NULL, vp = NULL){
    
    widget <- target
    graph <-  widget$graph
    gTree(children = gList(loonGrob(graph, name = "l_navgraph")),
          name = name, gp = gp, vp = vp)
}

