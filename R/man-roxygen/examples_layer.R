#' @examples
#'
#' p <- (l_plot(x=c(1,10,1.5,7,4.3,9,5,2,8),
#'              y=c(1,10,7,3,4,3.3,8,3,4),
#'              title="Demo Layers")))
#' 
#' id.g <- l_layer.group(p, "A Layer Group")
#' id.pts <- l_layer.points(x=c(3,6), y=c(4,7), color="red", parent=id.g)
#' l_scaleto.layer(p, id.pts) 
#' l_layer.configure(p, id.pts, x=c(-5,5,12), y=c(-2,-5,18), color="lightgray")
#'
#' 


