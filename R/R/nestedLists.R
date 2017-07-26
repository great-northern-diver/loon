
#' @title Convert an R list to a nested Tcl list
#'   
#' @description This is a helper function to create a nested Tcl list from an R
#'   list (i.e. a list of vectors).
#'   
#' @param x a list of vectors
#' 
#' @return a string that represents the tcl nested list
#' 
#' @seealso \code{\link{l_nestedTclList2Rlist}}
#' 
#' @export
#' 
#' @examples 
#' x <- list(c(1,3,4), c(4,3,2,1), c(4,3,2,5,6))
#' l_Rlist2nestedTclList(x)
l_Rlist2nestedTclList <- function(x) {
    #tcl_x <- tclVar()
    #for(x_el in x) {
    #    tcl('lappend', tcl_x , x_el)        
    #}
    #return(paste0('{',tclvalue(tcl_x),'}'))
    paste(
        vapply(x, function(x_el) {
            paste0('{',paste(x_el, collapse = ' '),'}')
        }, character(1)),
        collapse = ' '
    )
}


#' @title Convert a Nested Tcl List to an R List
#'   
#' @description Helper function to work with \R and Tcl
#'   
#' @param tclobj a tcl object as returned by \code{\link[tcltk]{tcl}} and 
#'   \code{\link[tcltk]{.Tcl}}
#' @param transform a function to transfrom the string output to another data
#'   type
#'  
#' @return a nested \R list 
#' 
#' @seealso \code{\link{l_Rlist2nestedTclList}}
#' 
#' @export
#' 
#' @examples 
#' 
#' tclobj <- .Tcl('set a {{1 2 3} {2 3 4 4} {3 5 3 3}}')
#' l_nestedTclList2Rlist(tclobj)
l_nestedTclList2Rlist <- function(tclobj, transform=function(x) {as.numeric(x)}) {

    n <- as.numeric(tcl('llength', tclobj))
    x <- vector(mode = 'list', length = n)
    
    for (i in 1:n) {
        x[[i]] <- transform(tcl('lindex', tclobj, i-1))
    }
    return(x)
} 
