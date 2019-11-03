from tk import tk

#' @title Query Size of a Plot Display
#' 
#' @description Get the width and height of a plot in pixels
#' 
#' @template param_widget
#' 
#' @return Vector width width and height in pixels
#' 
#' @seealso \code{\link{l_resize}}, \code{\link{l_size<-}}
#' 
#' @export
def l_size(widget):
    if(not isinstance(widget,str)):
        widget = widget.plot
    width = tk.tk.call('winfo','width',widget)
    height = tk.tk.call('winfo','height',widget)
    return([width,height])
    
#' @title Resize Plot Widget
#'   
#' @description Resizes the toplevel widget to a specific size. This setter
#'   function uses \code{\link{l_resize}}.
#'   
#' @template param_widget
#' @param value numeric vector of length 2 with width and height in pixels
#'   
#'   
#' @export
#' 
#' @seealso \code{\link{l_resize}}, \code{\link{l_size}}
#' 
#' @examples 
#' p <- l_plot(iris)
#' 
#' l_resize(p, 300, 300)
#' l_size(p) <- c(500, 500)
def l_size(widget,value):
    if(not isinstance(widget,str)):
        widget = widget.plot
    tk.tk.call("::loon::resize", widget, value[0], value[1])