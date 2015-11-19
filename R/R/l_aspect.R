
#' @export
l_aspect <- function(widget) {
    l_throwErrorIfNotLoonWidget(widget)
    return(as.numeric(tcl(widget, 'aspect')))
}


#' @export
'l_aspect<-' <- function(widget, value) {
    l_throwErrorIfNotLoonWidget(widget)
    if (!is.numeric(value) || value <=0) {
        stop("aspect ractio needs to be a value > 0.")
    }
    tcl(widget, 'aspect', value)
    widget
}


#' @export
l_setAspect <- function(widget, aspect, x , y) {
    l_throwErrorIfNotLoonWidget(widget)
    
    if(missing(aspect)) {
        aspect <- y/x
    }
    if (!is.numeric(value) || value <=0) {
        stop("aspect ractio needs to be a value > 0.")
    }
    tcl(widget, 'aspect', aspect)    
}