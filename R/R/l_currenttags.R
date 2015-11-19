
#' Get tags for item that is below mouse cursors
#' @export
l_currenttags <- function(widget) {
    l_throwErrorIfNotLoonWidget(widget)

    tags <- tcl(widget, 'currenttags')
    n <- as.numeric(tcl('llength', tags))
    vapply(0:(n-1), function(i) {
        paste(as.character(tcl('lindex', tags, i)), collapse = '')
       }, FUN.VALUE = character(1))
}


#' @export
l_currentindex <- function(widget) {
    tags <- l_currenttags(widget)

    if (tags[1] == "layer") {
        index <- as.numeric(substring(tags[4],'5')) + 1
    } else {
        index <- -1
    }
    return(index)
}
