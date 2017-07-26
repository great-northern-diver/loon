
#' @title Create an interactive loon plot widget
#'   
#' @description \code{l_plot} is a generic function for creating interactive 
#'   visualization environments for \R objects.
#'   
#' @inheritParams graphics::plot
#' @param ... named arguments to modify plot states
#'   
#' @details 
#' 
#' \ifelse{html}{
#' \out{<div style="background: #dff0d8; padding: 15px;"> To get started with loon it is recommended to read loons website 
#'   which can be accessed via the <code>l_help()</code> function call. </div>
#' }}{
#' To get started with loon it is recommended to read loons website 
#'   which can be accessed via the \code{l_help()} function call.
#' }
#' 
#'   
#'   \if{html}{ The general direct manipulation and interaction gestures are
#'   outlined in the following figures.
#'   
#'   Zooming and Panning
#'   
#'   \figure{gestures_zoom_pan.png}{options: alt="Zoom pan gestures"} 
#'   
#'   Selecting Points/Objects
#'   
#'   \figure{gestures_select.png}{options: alt="Select gestures"}
#'   
#'   Moving Points on the Scatterplot Display
#'   
#'   \figure{gestures_move.png}{options: alt="Move gestures"}
#'   
#'   }
#'   
#'   
#' @template return_widget_handle
#'   
#' @seealso \code{\link{l_info_states}}
#'   
#' @export
#' 
#' @examples
#' # ordinary use
#' p <- with(iris, l_plot(Sepal.Width, Petal.Length, color=Species))
#' 
#' # link another plot with the previous plot
#' p['linkingGroup'] <- "iris_data"
#' p2 <- with(iris, l_plot(Sepal.Length, Petal.Width, linkingGroup="iris_data"))
#' 
#' # Use with other tk widgets
#' library(tcltk)
#' tt <- tktoplevel()
#' p1 <- l_plot(parent=tt, x=c(1,2,3), y=c(3,2,1))
#' p2 <- l_plot(parent=tt, x=c(4,3,1), y=c(6,8,4))
#' 
#' tkgrid(p1, row=0, column=0, sticky="nesw")
#' tkgrid(p2, row=0, column=1, sticky="nesw")
#' tkgrid.columnconfigure(tt, 0, weight=1)
#' tkgrid.columnconfigure(tt, 1, weight=1)
#' tkgrid.rowconfigure(tt, 0, weight=1)
#' 
#' tktitle(tt) <- "Loon plots with custom layout"
l_plot <- function(x, y, ...) {
    UseMethod("l_plot")
}


#' @title Create an interactive 2d scatterplot display
#'   
#' @description Creates an interactive 2d scatterplot. Also, if no loon 
#'   inspector is open then the \code{l_plot} call will also open a loon 
#'   inspector.
#'   
#'   
#' @inheritParams graphics::plot.default
#' @param y please read in the argument description for the \code{x} argument above. 
#' @param parent a valid Tk parent widget path. When the parent widget is
#'   specified (i.e. not \code{NULL}) then the plot widget needs to be placed using
#'   some geometry manager like \code{\link{tkpack}} or \code{\link{tkplace}} in
#'   order to be displayed. See the examples below.
#' @param ... named arguments to modify plot states.
#'   
#'       
#' @details The scatterplot displays a number of direct interactions with the 
#'   mouse and keyboard, these include: zooming towards the mouse cursor using 
#'   the mouse wheel, panning by right-click dragging and various selection 
#'   methods using the left mouse button such as sweeping, brushing and 
#'   individual point selection. See the documentation for \code{\link{l_plot}} 
#'   for more details about the interaction gestures.
#'   
#' @export
#' @export l_plot.default
#' 
#' @examples 
#' p1 <- with(iris, l_plot(Sepal.Length, Sepal.Width, color=Species))
#' 
#' p2 <- with(iris, l_plot(Petal.Length ~ Petal.Width, color=Species))
#' 
#' # link the two plots p1 and p2
#' l_configure(p1, linkingGroup="iris", sync="push")
#' l_configure(p2, linkingGroup="iris", sync="push")
#' p1['selected'] <- iris$Species == "setosa" 
l_plot.default <-  function(x, y=NULL, parent=NULL, ...) {
    
    if(missing(x)) {
        
        plot <- loonPlotFactory('::loon::plot', 'plot', 'loon scatterplot', parent, ...)

    } else {
        
        ## Get x, y, xlab and ylab
        ## similar as in plot.default use xy.coords
        xlabel <- if (!missing(x)) 
            gsub("\"", "", deparse(substitute(x)))
        ylabel <- if (!missing(y)) 
            gsub("\"", "", deparse(substitute(y)))
        
        xy <- xy.coords(x, y, xlabel, ylabel)

        ## xlab and ylab will be overwritten in they
        ## are defined in ...
        if (is.null(xy$xlab))
            xy$xlab <- ""
        if (is.null(xy$ylab))
            xy$ylab <- ""
        
        plot <- loonPlotFactory('::loon::plot', 'plot', 'loon scatterplot', parent,
                                x=xy$x, y=xy$y,
                                xlabel=xy$xlab, ylabel=xy$ylab,
                                ...)
    }

    return(plot)
}



