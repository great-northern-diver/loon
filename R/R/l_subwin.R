#' @title Create a child widget path
#'   
#' @description This function is similar to \code{.Tk.subwin} except that does
#'   not the environment of the "tkwin" object to keep track of numbering the
#'   subwidgets. Instead it creates a widget path (parent).looni, where i is the
#'   smallest integer for which no widget exists yet.
#'   
#' @param parent parent widget path
#' @param name child name
#
#' @return widget path name as a string
#' 
#' @export
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
