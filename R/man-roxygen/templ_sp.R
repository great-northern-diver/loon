#' @description Methods to plot map data defined in the \code{\link[sp]{sp}}
#'   package
#' 
#' @template param_widget
#' @param ... arguments forwarded to the relative \code{\link{l_layer}} function
#' 
#' @details Note that currently loon does neither support holes and ring
#'   directions.
#' 
#' @references Applied Spatial Data Analysis with R by Bivand, Roger S. and
#'   Pebesma, Edzer and Gómez-Rubio and Virgilio
#'   \url{http://www.springer.com/us/book/9781461476177}
#'
#' 
#' @return layer id
#'  
#' @seealso \code{\link[sp]{sp}}, \code{\link{l_layer}}
#'
#' @examples
#' library(sp)
#' library(rworldmap)
#' 
#' world <- getMap(resolution = "coarse")
#' p <- l_plot()
#' lmap <- l_layer(p, world, asSingleLayer=TRUE)
#' l_scaleto_world(p)
 
# attr(lmap,'hole')
# attr(lmap,'NAME')
