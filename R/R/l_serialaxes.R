#' @title Create a Serialaxes Widget
#' 
#' @description The seerialaxes widget displays multivariate data either as a
#'   stacked star glyph plot, or as a parallel coordinate plot.
#'   
#' 
#' @param data a data frame with numerical data only
#' @param sequence vector with variable names that defines the axes sequence
#' @param scaling one of 'variable', 'data', 'observation' or 'none' to specify
#'   how the data is scaled. See Details for more information
#' @param axesLayout either \code{"radial"} or \code{"parallel"}
#' @param showAxes boolean to indicate whether axes should be shown or not
#' @template param_parent
#' @template param_dots_state_args
#'   
#' @details The \code{scaling} state defines how the data is scaled. The axes
#'   display 0 at one end and 1 at the other. For the following explanation
#'   assume that the data is in a nxp dimensional matrix. The scaling options
#'   are then
#' \tabular{ll}{
#' variable \tab per column scaling\cr
#' observation \tab per row scaling\cr
#' data \tab whole matrix scaling\cr
#' none \tab do not scale
#' }
#' 
#' 
#' @return plot handle object
#' 
#' @export
#' 
#' @examples 
#' s <- l_serialaxes(data=oliveAcids, color=olive$Area, title="olive data")
#' s['axesLayout'] <- 'parallel'
#' states <- l_info_states(s)
#' names(states)
l_serialaxes <- function(data, sequence, scaling="variable", axesLayout='radial', 
                         showAxes=TRUE, parent=NULL, ... ) {
    

    if (!missing(data) && missing(sequence)) {
        sequence <- names(data)
    }
    
    loonPlotFactory('::loon::serialaxes', 'serialaxes', 'loon serialaxes plot', parent,
                    data=l_data(data),
                    sequence=sequence,
                    showAxes=showAxes,
                    scaling=scaling,
                    axesLayout=axesLayout,
                    ...)
}