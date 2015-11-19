
#' @export

l_ladder <- function(from=-5, to=5, alphaX=0, alphaY=0) {
    tt <- l_toplevel()

    plot <- l_ladder_widget(tt, from, to, alphaX, alphaY)
    tkpack(plot, fill="y", expand=TRUE)
    
    return(plot)
}

#' @export

l_ladder_widget <- function(parent, from=-5, to=5, alphaX=0, alphaY=0) {

    child <- l_subwin(parent, 'ladder')

    plot <- try(tcl("::loon::ladder", child))
    
    if(is(plot,'try-error')) {
        plot <- NULL
        tkdestroy(tt)
        stop("ladder widget could not be created.")
    } else {
        plot <- as.character(plot)        
    }
    
    tcl(plot, "configure", from=from, to=to, alphaX=alphaX, alphaY=alphaY)

    class(plot) <- "loon"
    
    return(plot)
}
