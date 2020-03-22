
l_toplevel <- function(path) {

    if(missing(path)) {
        i <- valid_path()
        path <- paste('.l', i , sep="")
    }

    tt <- as.character(tcl('toplevel',path))
    tcl('wm', 'iconphoto', tt, default='::loon::loonIcon')

    return(tt)
}


valid_path <- function() {
    i <- 0
    child <- paste('.l', i , sep="")
    while(as.logical(tcl('winfo','exists', child))) {
        i <- i + 1
        child <- paste('.l', i , sep="")
    }
    return(i)
}
