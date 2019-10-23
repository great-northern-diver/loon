from l_throwErrorIfNotLoonWidget import *
from tk import tk
from loon_class import *
def l_scaleto(widget, *opt):
    if(isinstance(widget, loon_class.loon)):
        widget = widget.plot
    l_throwErrorIfNotLoonWidget(widget)
    tk.tk.call(widget, 'scaleto', *opt)



# #' @title Change Plot Region to Display All Plot Data
# #'   
# #' @description The function modifies the \code{zoomX}, \code{zoomY}, 
# #'   \code{panX}, and \code{panY} so that all elements in the plot are
# #'   displayed.
# #' 
# #' @template param_widget
# #'   
# #' @export
def l_scaleto_world(widget):
    l_scaleto(widget, "world")


# #' @title Change Plot Region to Display the All Data of the Model Layer
# #'   
# #' @description The function modifies the \code{zoomX}, \code{zoomY}, 
# #'   \code{panX}, and \code{panY} so that all elements in the model layer of the
# #'   plot are displayed.
# #'   
# #' @template param_widget
# #'   
# #' @export
def l_scaleto_plot(widget):
    l_scaleto(widget, "plot")


# #' @title Change Plot Region to Display All Selected Data
# #'   
# #' @description The function modifies the \code{zoomX}, \code{zoomY}, 
# #'   \code{panX}, and \code{panY} so that all selected data points are
# #'   displayed.
# #'   
# #' @template param_widget
# #'   
# #' @export
def l_scaleto_selected(widget):
    l_scaleto(widget, "selected")


# #' @title Change Plot Region to Display All Active Data
# #'   
# #' @description The function modifies the \code{zoomX}, \code{zoomY}, 
# #'   \code{panX}, and \code{panY} so that all active data points are displayed.
# #' 
# #' @template param_widget
# #'    
# #' @export
def l_scaleto_active(widget):
    l_scaleto(widget, "active")


# #' @title Change Plot Region to Display All Elements of a Particular Layer
# #'   
# #' @description The function modifies the \code{zoomX}, \code{zoomY}, 
# #'   \code{panX}, and \code{panY} so that all elements of a particular layer are
# #'   displayed.
# #'   
# #' @template param_target
# #' @param layer layer id
# #' 
# #' @seealso \code{\link{l_layer_ids}}
# #'   
# #' @export
# def l_scaleto_layer(target, layer):
#     if (is(target, "l_layer")) {
#         layer <- target
#         target <- attr(layer, "widget")
#     }
#     l_scaleto(target, "layer", layer)


