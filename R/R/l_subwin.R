#' @title Create a child widget path
#'
#' @description
#'
#' Given a widget path name 'parent' create a child path name.
#' 
#' @details
#'
#' This function is similar to \code{.Tk.subwin} except that does not
#' the environment of the "tkwin" object to keep track of numbering
#' the subwidgets. Instead it creates a widget path (parent).looni,
#' where i is the smallest integer for which no widget exists yet.
#'
#'
#' @param parent parent widget path
#
#' @return widget path name of the subwin as a string

l_subwin <- function(parent, name="w") {
    if(is(parent,"tkwin")) {
        parent <- .Tk.ID(parent)
    }
    
    i <- 0
    child <- paste0(parent, '.', name)
    while(as.logical(tcl('winfo','exists', child))) {       
        i <- i + 1
        child <- paste0(parent, '.', name, i)        
    }
    
    return(child)
}
