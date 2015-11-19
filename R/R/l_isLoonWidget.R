
#' @title Check if a widget path is a valid loon widget
#'
#' @inheritParams l_widget
#'
#' @export
l_isLoonWidget <- function(widget) {

    isLoon <- as.logical(as.character(tcl('::loon::isKnownWidget', widget)))

    if (isLoon) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
