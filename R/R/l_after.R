
## package variable that holds references to callback functions
## such that they do not get erased
callbackFunctions <- new.env()

callbackFunctions$idle <- list()


#' @title Evaluate a function on once the processor is idle
#'   
#' @description It is possible for an observer to call the configure method of 
#'   that plot while the plot is still in the configuration pipeline. In this 
#'   case, a warning is thrown as unwanted side effects can happen if the next 
#'   observer in line gets an outdated notification. In this case, it is 
#'   recommended to use the l_after_idle function that evaluates some code once 
#'   the processor is idle.
#'   
#' @param fun function to be evaluated once tcl interpreter is idle
#' 
#' @return NULL
#' 
#' @export
l_after_idle <- function(fun) {

    ## save function from garbage collection
    callback <- .Tcl.callback(fun)
    hex <- unlist(strsplit(callback,' ', fixed=TRUE))[1]
    callbackFunctions$idle[[hex]] <- fun

    tcl('after', 'idle', callback)

    ## Todo, delete reference such that the garbage collector can
    ## delete function
    
    invisible(NULL)
}
