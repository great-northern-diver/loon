
## package variable that holds references to callback functions
## such that they do not get erased
callbackFunctions <- new.env()


callbackFunctions$idle <- list()

#' @export
l_after_idle <- function(fun) {

    ## save function from garbage collection
    callback <- .Tcl.callback(fun)
    hex <- unlist(strsplit(callback,' ', fixed=TRUE))[1]
    callbackFunctions$idle[[hex]] <- fun

    tcl('after', 'idle', callback)

    ## Todo, delete reference such that the garbage collector can
    ## delete function
    
}
