#' Create a graph navigator grob
#' 
#' @param widget \code{l_navgraph}, \code{l_ng_plots} or \code{l_ng_ranges} object of class inheriting from "l_navgraph"
#' 
#' @return a grid grob of the graph
#' 
#' @import grid
#' 
#' @export
#' 
#' @seealso \code{\link{loonGrob.l_graph}}
#' 
#' @examples 
#' 
#' ng <- l_navgraph(oliveAcids, separator='-', color=olive$Area)
#' 
#' library(grid)
#' navGrob <- loonGrob(ng)
#' grid.newpage()
#' grid.draw(navGrob)
#' 
loonGrob.l_navgraph <- function(target, name = NULL, gp = NULL, vp = NULL){
    
    widget <- target
    graph <-  widget$graph
    gTree(children = gList(loonGrob(graph, name = "l_navgraph")),
          name = name, gp = gp, vp = vp)
}

