#' @export
l_serialaxes <- function(data, sequence, showAxes=TRUE, parent=NULL, ... ) {
    

    if (!missing(data) && missing(sequence)) {
        sequence <- names(data)
    }
    
    loonPlotFactory('::loon::serialaxes', 'serialaxes', 'loon serialaxes plot', parent,
                    data=l_data(data),
                    sequence=sequence,
                    showAxes=showAxes,
                    ...)
}