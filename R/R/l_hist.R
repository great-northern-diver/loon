
#' @export
l_hist <-  function(x, origin, binwidth, parent=NULL, ...) {
    

    if(missing(x)) {
        
        plot <- loonPlotFactory('::loon::histogram', 'hist', 'loon histogram', parent, ...)

    } else {
          
        if (!missing(x)) 
            xlabel <- gsub("\"", "", deparse(substitute(x)))
        
        ## xlab and ylab will be overwritten in they
        ## are defined in ...
        if (missing(origin))
            origin <- min(x)
        
        if (missing(binwidth)) {
            binwidth <- diff(range(x))/30
            if (binwidth < 0.0001)
                binwidth <- 0.00005
        }

        plot <- loonPlotFactory('::loon::histogram', 'hist', 'loon histogram', parent,
                                x = x,
                                origin=origin,
                                binwidth=binwidth,
                                xlabel=xlabel,
                                ...)
    }


    return(plot)
}
