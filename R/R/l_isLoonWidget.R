
#' @title Check if a widget path is a valid loon widget
#'
#' @description This function can be useful to check whether a loon widget is
#'   has been closed by the user.
#'
#' @template param_widget
#'
#' @return boolean, TRUE if the argument is a valid loon widget path, FALSE
#'   otherwise
#'
#' @export
l_isLoonWidget <- function(widget) {

    if(is.null(widget)) return(FALSE)
    if(is.list(widget)) return(FALSE)

    as.logical(as.character(tcl('::loon::isKnownWidget', widget)))
}
