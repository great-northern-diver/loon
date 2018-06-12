#' @title Loon methods for moving average objects
#'
#' @description a generic function for creating a decomposed time seires plot based on 
#' objects of class \code{decomposed.ts} 
#'
#' @inheritParams graphics::plot
#' @param decompose \code{decomposed.ts} object
#' @param ylabel the graphical parameter ylabel labelling the y axis of the plot. Here, it is a length four vector of original 
#' time seires, trend, seasonality and remainder. If \code{NULL}, ylabel is created based on the information provided.
#' @param xlabel the graphical parameter xlabel labelling the x axis of the plot. Here, it is a length four vector of original 
#' time seires, trend, seasonality and remainder. If \code{NULL}, xlabel is created as "Times".
#' @param tk_title provides an alternate interface to Tk's \code{wm title}. If \code{NULL}, \code{decomposed.ts} will be created.
#' @param title an overall title of loon plot. If \code{NULL}, the default title would be "Seasonal Trend Analysis".
#' @param linkingGroup link groups
#' @param linewidth lines width of original time seires, trend, seasonality and remainder with default value 1.
#' @param linesColour lines colour of original time seires, trend, seasonality and remainder with default colour "firebrick".
#' @param size points size of original time seires, trend, seasonality and remainder with default value 1.
#' @param pointsColour points colour of original time seires, trend, seasonality and remainder with default colour "steelblue".
#' @param ... named arguments to modify plot states
#'
#'
#' @export
#' 
#' @examples
#' decompose <- decompose(co2)
#' p <- l_plot(decompose, title = "Atmospheric carbon dioxide over Mauna Loa")
#' names(l_info_states(p$p1)) # extract loon information


l_plot.decomposed.ts <- function(decompose, ylabel = NULL, xlabel = NULL, 
                                 tk_title = NULL, title = NULL, linkingGroup = "ts", 
                                 linewidth = 1, linesColour = "firebrick",
                                 size = 1, pointsColour = "steelblue", ...){
    
    l_plotForts(decompose, pointsColour, size, 
                ylabel, xlabel, title, tk_title, 
                linkingGroup, linesColour, linewidth)
}