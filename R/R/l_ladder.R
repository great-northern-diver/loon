
# TODO
l_ladder <- function(from=-5, to=5, alphaX=0, alphaY=0, parent=NULL) {

    with_toplevel <- if (is.null(parent)) {
        parent <- l_toplevel()
        TRUE
    } else {
        FALSE
    }
    
    child <- l_subwin(parent, 'ladder')

    plot <- try(tcl("::loon::ladder", child))
    
    if(is(plot,'try-error')) {
        if (with_toplevel) tkdestroy(parent)
        stop("ladder widget could not be created.")
    } else {
        plot <- as.character(plot)
    }
    
    if (with_toplevel) tkpack(plot, fill="y", expand=TRUE)

    tcl(plot, "configure", from=from, to=to, alphaX=alphaX, alphaY=alphaY)

    class(plot) <- c("l_ladder", "loon")
    
    plot
}
