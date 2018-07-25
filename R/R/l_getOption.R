
#' @title Get the value of a loon display option
#' 
#' @description All of loon's displays access a set of common options.  
#' This function accesses and returns the current value of the named option.
#'   
#' @param option the name of the option being queried.
#'   
#' @return the value of the named option.
#'   
#' @seealso \code{\link{l_getOptionNames}}, \code{\link{l_userOptions}},
#'   \code{\link{l_userOptionDefault}}, \code{\link{l_setOption}}
#'   
#' @export
#' 
#' @examples 
#' l_getOption("background")
#' 
l_getOption <- function(option) {
    
    if(length(option) != 1) stop("only one option can be queried at a time")
    
    if (!(option %in%  l_getOptionNames())) 
        stop("option ", option, " is not valid")
    
    as.character(.Tcl(paste0("set ::loon::Options(", option, ")")))
    
}


#' @title Get the names of all loon display options
#' 
#' @description All of loon's displays access a set of common options.  
#' This function accesses and returns the names of all loon options.
#'   
#' @return a vector of all loon display option names.
#'   
#' @seealso \code{\link{l_getOption}}, \code{\link{l_userOptions}},
#'   \code{\link{l_userOptionDefault}}, \code{\link{l_setOption}}
#'   
#' @export
#' 
#' @examples 
#' l_getOptionNames()
#' 
l_getOptionNames <- function() {
    as.character(.Tcl('array names ::loon::Options'))
}


#' @title Get the names of all loon display options that can be set by the user.
#' 
#' @description All of loon's displays access a set of common options.  
#' This function accesses and returns the names of the subset of loon options which
#' can be changed by the user.
#'   
#' @return a vector of all user settable option names.
#'   
#' @seealso \code{\link{l_getOptionNames}}, \code{\link{l_getOption}},
#'   \code{\link{l_userOptionDefault}}, \code{\link{l_setOption}}
#'   
#' @export
#' 
#' @examples 
#' l_userOptions()
#' 
l_userOptions <- function() {
    c("select-color",
      "background",
      "foreground",
      "guidesBackground",
      "guidelines",
      "brush_color",
      "brush_color_handle"
    )
}

#' @title Get loon's system default value for the named display option.
#' 
#' @description All of loon's displays access a set of common options.  
#' This function accesses and returns the default value for the named option.
#'   
#' @param option the name of the user changeable loon display option 
#' whose default value is to be determined.
#' 
#' @return the default value for the named option
#' 
#' @seealso \code{\link{l_getOptionNames}}, \code{\link{l_getOption}},
#'   \code{\link{l_userOptionDefault}}, \code{\link{l_userOptions}}
#'   
#' @export
#' 
#' @examples 
#' l_userOptionDefault("background")
#' 
l_userOptionDefault <- function(option){
   # if (option == "select-color") {
   #     "magenta"
   # } else {
        switch(
            option,
            "select-color" = "magenta",  ## need to rename select-color to select_color
            background = "white",
            foreground = "black",
            guidesBackground = "gray92",
            guidelines = "white",
            brush_color = "lightgray",
            brush_color_handle = "gray",
            stop("option ", option, " is not settable by user, so no default value.")
        )
   # }
}

#' @title Set the value of a loon display option
#' 
#' @description All of loon's displays access a set of common options.  
#' This function assigns the value to the named option.
#'   
#' @param option the name of the option being set
#' @param value the value to be assigned to the option.  If value == "default", 
#'        then the option is set to loon's default value for it.
#'   
#' @return the new value
#'   
#' @seealso \code{\link{l_getOption}}, \code{\link{l_getOptionNames}}, \code{\link{l_userOptions}},
#'   \code{\link{l_userOptionDefault}}
#'   
#' @export
#' 
#' @examples 
#' l_setOption("select-color", "red")
#' l_setOption("select-color", "default")
#' 
l_setOption <- function(option, value) {
    if (value == "default") {
        value <-  l_userOptionDefault(option)
        l_setOption(option, value)
    }
    
    if (!(option %in%  l_getOptionNames())) 
        stop("option ", option, " is not valid")
    if (!(option %in% l_userOptions()))
        stop("option ", option, " is not settable by user.")
    tcl("set", paste0("::loon::Options(", option, ")"), value)
    
    value
}
