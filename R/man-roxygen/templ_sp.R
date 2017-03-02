#' @description Methods to plot map data defined in the \code{\link[sp]{sp}}
#'   package
#' 
#' @param widget widget widget path as a string or as an object handle
#' @param x an object defined in the class \code{\link[sp]{sp}}
#' @param ... arguments forwarded to the relative \code{\link{l_layer}} function
#' 
#' @details Note that currently loon does neither support holes and ring
#'   directions.
#' 
#' @references Applied Spatial Data Analysis with R by Bivand, Roger S. and
#'   Pebesma, Edzer and Gomez-Rubio and Virgilio
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
