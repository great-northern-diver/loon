

#' @title Throw an error if string is not associated with a loon widget
#'
#' @description
#'
#' Helper function to ensure that a widget path is associated with a
#' loon widget.
#'
#' @param widget widget path name as a string
#'
#' @return TRUE if the string is associated with a loon widget,
#' otherwise an error is thrown.
#'
#' 

l_throwErrorIfNotLoonWidget <- function(widget) {

    if(!l_isLoonWidget(widget)) {
        stop(paste('Widget path "', widget,
                   '" is not a known loon widget.', sep =''),
             call.=FALSE)
    }
    return(TRUE)
}
