

#' @export
l_getOption <- function(option) {
    
    if(length(option) != 1) stop("only one option can be queried at a time")
    
    if (!(option %in%  l_getOptionNames())) 
        stop("option ", option, " is not valid")
    
    as.character(.Tcl(paste0("set ::loon::Options(", option, ")")))
    
}

#' @export
l_getOptionNames <- function() {
    as.character(.Tcl('array names ::loon::Options'))
}

#' @export
l_userOptions <- function() {
    c("select_color",
      "background",
      "foreground",
      "guidesBackground",
      "guidelines",
      "brush_color",
      "brush_color_handle"
    )
}

l_userOptionDefault <- function(option){
    switch(
        option,
        select_color = "magenta",
        background = "white",
        foreground = "black",
        guidesBackground = "gray92",
        guidelines = "white",
        brush_color = "lightgray",
        brush_color_handle = "gray",
        stop("option ", option, " is not settable by user, so no default value.")
    )
}

#' @export
l_setOption <- function(option, value) {
    if (value == "default") {
        l_setOption(option, l_userOptionDefault(option))
    }
    
    if (!(option %in%  l_getOptionNames())) 
        stop("option ", option, " is not valid")
    if (!(option %in% l_userOptions()))
        stop("option ", option, " is not settable by user.")
    tcl("set", paste0("::loon::Options(", option, ")"), value)
    
    value
}
