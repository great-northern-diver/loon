
#' Convert an R list to a nested tcl list
#' @export
#' @examples 
#' 
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
               }, character(1)), collapse = ' ')
}


#' Convert a nested tcl list to a R list
#' @export
#' @examples 
#' 
#' tclobj <- .Tcl('set a {{1 2 3} {2 3 4 4} {3 5 3 3}}')
#' l_nesteTclList2Rlist(tclobj)
l_nesteTclList2Rlist <- function(tclobj, transform=function(x) {as.numeric(x)}) {

    n <- as.numeric(tcl('llength', tclobj))
    x <- vector(mode = 'list', length = n)
    
    for (i in 1:n) {
        x[[i]] <- transform(tcl('lindex', tclobj, i-1))
    }
    return(x)
} 
