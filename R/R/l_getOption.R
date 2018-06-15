

l_getOption <- function(option) {
    
    if(length(option) != 1) stop("only one option can be queried at a time")
    
    if (!(option %in%  l_getOptionNames())) 
        stop("option ", option, " is not valid")
    
    as.character(.Tcl(paste0("set ::loon::Options(", option, ")")))
    
}

l_getOptionNames <- function() {
    as.character(.Tcl('array names ::loon::Options'))
}

l_setOption <- function(option, value) {
    if (!(option %in%  l_getOptionNames())) 
        stop("option ", option, " is not valid")
    
    tcl("set", paste0("::loon::Options(", option, ")"), value)
    
    value
}
