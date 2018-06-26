#' @title Loon methods for STL objects
#'
#' @description a generic function for creating a decomposed time seires plot based on 
#' objects of class \code{stl} 
#'
#' @inheritParams graphics::plot
#' @param x  an \code{stl} object
#' @param y  NULL, ignored
#' @param ylabel the graphical parameter ylabel labelling the y axis of the plot. Here, it is a length four vector of original 
#' time seires, trend, seasonality and remainder. If \code{NULL}, ylabel is created based on the information provided.
#' @param xlabel the graphical parameter xlabel labelling the x axis of the plot. Here, it is a length four vector of original 
#' time seires, trend, seasonality and remainder. If \code{NULL}, xlabel is created as "Times".
#' @param tk_title provides an alternate interface to Tk's \code{wm title}. If \code{NULL}, \code{stl} will be created.
#' @param title an overall title of loon plot. If \code{NULL}, the default title would be "Seasonal Trend Analysis".
#' @param linkingGroup link groups
#' @param linewidth lines width of original time seires, trend, seasonality and remainder with default value 1.
#' @param linesColour lines colour of original time seires, trend, seasonality and remainder with default colour "firebrick".
#' @param size points size of original time seires, trend, seasonality and remainder with default value 1.
#' @param pointsColour points colour of original time seires, trend, seasonality and remainder with default colour "steelblue".
#' @param ... named arguments to modify plot states
#' 
#' @export
#' 
#' @examples
#' foo <- stl(co2, "per")
#' p <- l_plot(foo, title = "Atmospheric carbon dioxide over Mauna Loa")
#' names(l_info_states(p$p1))



l_plot.stl <- function(x, y = NULL, ylabel = NULL, xlabel = NULL, tk_title = NULL, title = NULL, 
                       linkingGroup = "ts", linewidth = 1, linesColour = "firebrick", 
                       size = 1, pointsColour = "steelblue", ...){
    if (!is.null(y)) warning("value of y argument is ignored")
    l_plotForts(x, pointsColour, size, ylabel, xlabel, title, tk_title, 
                linkingGroup,linesColour, linewidth)
    
}
