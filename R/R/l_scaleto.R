
l_scaleto <- function(widget, ...) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'scaleto', ...)
    invisible()
}


#' @title Change Plot Region to Display All Plot Data
#'   
#' @description The function modifies the \code{zoomX}, \code{zoomY}, 
#'   \code{panX}, and \code{panY} so that all elements in the plot are
#'   displayed.
#' 
#' @template param_widget
#'   
#' @export
l_scaleto_world <- function(widget) {
    l_scaleto(widget, "world")
}

#' @title Change Plot Region to Display the All Data of the Model Layer
#'   
#' @description The function modifies the \code{zoomX}, \code{zoomY}, 
#'   \code{panX}, and \code{panY} so that all elements in the model layer of the
#'   plot are displayed.
#'   
#' @template param_widget
#'   
#' @export
l_scaleto_plot <- function(widget) {
    l_scaleto(widget, "plot")
}

#' @title Change Plot Region to Display All Selected Data
#'   
#' @description The function modifies the \code{zoomX}, \code{zoomY}, 
#'   \code{panX}, and \code{panY} so that all selected data points are
#'   displayed.
#'   
#' @template param_widget
#'   
#' @export
l_scaleto_selected <- function(widget) {
    l_scaleto(widget, "selected")
}

#' @title Change Plot Region to Display All Active Data
#'   
#' @description The function modifies the \code{zoomX}, \code{zoomY}, 
#'   \code{panX}, and \code{panY} so that all active data points are displayed.
#' 
#' @template param_widget
#'    
#' @export
l_scaleto_active <- function(widget) {
    l_scaleto(widget, "active")
}

#' @title Change Plot Region to Display All Elements of a Particular Layer
#'   
#' @description The function modifies the \code{zoomX}, \code{zoomY}, 
#'   \code{panX}, and \code{panY} so that all elements of a particular layer are
#'   displayed.
#'   
#' @template param_target
#' @param layer layer id
#' 
#' @seealso \code{\link{l_layer_ids}}
#'   
#' @export
l_scaleto_layer <- function(target, layer) {
    if (is(target, "l_layer")) {
        layer <- target
        target <- attr(layer, "widget")
    }
    l_scaleto(target, "layer", layer)
}

