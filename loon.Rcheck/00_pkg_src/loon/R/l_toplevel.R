
l_toplevel <- function(path) {
    if(missing(path)) {

        i <- 0
        child <- paste('.l', i , sep="")
        while(as.logical(tcl('winfo','exists', child))) {       
            i <- i + 1
            child <- paste('.l', i , sep="")            
        }
        path <- child
        
    }

    tt <- as.character(tcl('toplevel',path))
    tcl('wm', 'iconphoto', tt, default='::loon::loonIcon')
    
    return(tt)
}
