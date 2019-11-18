#' @title Query the aspect ratio of a plot
#'   
#'   
#' @template description_aspect_ratio
#' 
#' @template param_widget
#' 
#' @return aspect ratio
#'   
#' @export
#' 
#' @examples 
#' p <- with(iris, l_plot(Sepal.Length ~ Sepal.Width, color=Species))
#' 
#' l_aspect(p)
#' l_aspect(p) <- 1
l_aspect <- function(widget) {
    l_throwErrorIfNotLoonWidget(widget)
    return(as.numeric(tcl(widget, 'aspect')))
}


#' @title Set the aspect ratio of a plot
#'   
#' @template description_aspect_ratio
#'   
#' @template param_widget
#' @param value aspect ratio
#'   
#' @details Changing the aspect ratio with \code{l_aspect<-} changes effectively
#'   the \code{zoomY} state to obtain the desired aspect ratio. Note that the 
#'   aspect ratio in loon depends on the plot width, plot height and the states 
#'   \code{zoomX}, \code{zoomY}, \code{deltaX}, \code{deltaY} and 
#'   \code{swapAxes}. Hence, the aspect aspect ratio can not be set permanently
#'   for a loon plot.
#'   
#' @export
#' 
#' @examples 
#' p <- with(iris, l_plot(Sepal.Length ~ Sepal.Width, color=Species))
#' 
#' l_aspect(p)
#' l_aspect(p) <- 1
'l_aspect<-' <- function(widget, value) {
    l_throwErrorIfNotLoonWidget(widget)
    if (!is.numeric(value) || value <=0) {
        stop("aspect ractio needs to be a value > 0.")
    }
    tcl(widget, 'aspect', value)
    widget
}

#' @title Set the aspect ratio of a plot
#'   
#' @template description_aspect_ratio
#'   
#' @template param_widget
#' @param aspect aspect ratio, optional, if omitted then the \code{x} and 
#'   \code{y} arguments have to be specified.
#' @param x optional, if the \code{aspect} argument is missing then \code{x} and
#'   \code{y} can be specified and the aspect ratio is calculted usding 
#'   \code{y/x}.
#' @param y see description for \code{x} argument above
#'     
#' @export
#' 
#' @examples 
#' p <- with(iris, l_plot(Sepal.Length ~ Sepal.Width, color=Species))
#' 
#' l_aspect(p)
#' l_setAspect(p, x = 1, y = 2) 
l_setAspect <- function(widget, aspect, x , y) {
    l_throwErrorIfNotLoonWidget(widget)
    
    if(missing(aspect)) {
        aspect <- y/x
    }
    if (!is.numeric(aspect) || aspect <=0) {
        stop("aspect ractio needs to be a value > 0.")
    }
    tcl(widget, 'aspect', aspect)    
}