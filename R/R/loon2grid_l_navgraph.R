#' Create a graph navigator grob
#' 
#' @param widget \code{l_navgraph}, \code{l_ng_plots} or \code{l_ng_ranges} object of class inheriting from "l_navgraph"
#' 
#' @return a grid grob
#' 
#' @import grid
#' 
#' @export
#' 
#' @seealso \code{\link{loon2grid.l_graph}}
#' 
#' @examples 
#' 
#' ########################### l_navgraph ###########################
#' ng <- l_navgraph(oliveAcids, separator='-', color=olive$Area)
#' 
#' library(grid)
#' ## loon2grid(ng) will just return navigator grob
#' navGrob <- loon2grid(ng)
#' grid.newpage()
#' grid.draw(navGrob)
#' 
#' ## loon2grid(ng$plot) will return scatterplot grob
#' scatterGrob <- loon2grid(ng$plot)
#' grid.newpage()
#' grid.draw(scatterGrob)
#' 
#' ########################### l_ng_plots ########################### 
#' q1 <- function(x)as.vector(quantile(x, probs=0.25))
#' nav <- l_ng_plots(sapply(oliveAcids, q1), oliveAcids)
#' 
#' library(grid)
#' ## loon2grid(nav) will just return the navigator grob
#' navGrob <- loon2grid(nav)
#' grid.newpage()
#' grid.draw(navGrob)
#' 
#' ## loon2grid(nav$plots) will return histogram grob or pairs Grob
#' histGrob <- loon2grid(nav$plots)
#' grid.newpage()
#' grid.draw(histGrob)
#' 
#' ## loon2grid(nav$plot) will return scatterplot grob
#' scatterGrob <- loon2grid(nav$plot)
#' grid.newpage()
#' grid.draw(scatterGrob)
#' ########################### l_ng_ranges ###########################
#' n <- 100
#' dat <- data.frame(
#'    A = rnorm(n), B = rnorm(n), C = rnorm(n),
#'    D = rnorm(n), E = rnorm(n)
#' )
#' m2d <- data.frame(
#'     cor = with(dat, c(cor(A,B), cor(A,C), cor(B,D), cor(D,E), cor(A,E))),
#'     my_measure = c(1, 3, 2, 1, 4),
#'     row.names = c('A:B', 'A:C', 'B:D', 'D:E', 'A:E')
#' )
#' 
#' # or m2d <- as.matrix(m2d)
#' nav <- l_ng_ranges(measures=m2d, data=dat)
#' 
#' library(grid)
#' ## loon2grid(nav) will just return the navigator grob
#' navGrob <- loon2grid(nav)
#' grid.newpage()
#' grid.draw(navGrob)
#' 
#' ## loon2grid(nav$plot) will return scatterplot grob
#' scatterGrob <- loon2grid(nav$plot)
#' grid.newpage()
#' grid.draw(scatterGrob)  


loon2grid.l_navgraph <- function(widget){
  
  graph <-  widget$graph
  navigator <- widget$navigator
  
  loon2grid.l_graph(graph, navigators = navigator)
}

