
#' @title Convert a Tcl Object to some other R object
#'   
#' @description Return values from \code{\link{.Tcl}} and \code{\link{tcl}} are 
#'   of class \code{tclObj} and often need to be mapped to a different data 
#'   structure in \R. This function is a helper class to do this mapping.
#'   
#'   
#' @param x a \code{tclObj} object
#' @param cast a function to conver the object to some other \R object
#'   
#' @return A object that is returned by the function specified with the
#'   \code{cast} argument.
#'   
#' @export
l_toR <- function(x, cast=as.character) {
    
    if(!is.function(cast)) 
        stop('cast is expected to be a function')
    
    if (!grepl(' ', x))
        cast(x)        
    else
        cast(unlist(strsplit(x, " ", fixed=TRUE)))
    
}