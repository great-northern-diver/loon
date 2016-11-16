
#' @title Create a 2d Scatterplot Widget
#'
#' @description
#'
#' Create a scatterplot widget. It the parent widget gets specified,
#' the plot widget needs to be placed using some geometry manager like
#' \code{\link{tkpack}} or \code{\link{tkplace}} in order to be
#' displayed.
#'
#' 
#' @details
#'
#' This function is like \code{loon_plot} except it requires a parent
#' widget and need to be packed manually. This is useful when placing
#' more than one scatterplot in a window, or writing a custom Tk GUI
#' with a scatterplot.
#'
#'
#' @export
#'
#' @param x coordinates
#' @param y coordinates
#' @param parent parent widget path of the scatterplot widget
#'
#' @return widget path name if was successful, otherwise NULL
#'
#' @examples
#' # ordinary use
#' p <- with(iris, l_plot(Sepal.Width, Petal.Length, color=Species))
#' 
#' # link another plot with the previous plot
#' p['linkingGroup'] <- "iris"
#' p2 <- with(iris, l_plot(Sepal.Length, Petal.Width, linkingGroup="iris"))
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

#' @export l_plot.default
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
        
        xy <- try(xy.coords(x, y, xlabel, ylabel))
        if (is(xy,'try-error')) {
            if(new.toplevel) tkdestroy(parent)
            return
        }
        
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



