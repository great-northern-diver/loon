
#' @export
l_toR <- function(x, cast=as.character) {
    
    if(!is.function(cast)) 
        stop('cast is expected to be a function')
    
    if (!grepl(' ', x))
        cast(x)        
    else
        cast(unlist(strsplit(x, " ", fixed=TRUE)))
    
}