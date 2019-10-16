#' @title Convert an R data.frame to a Tcl dictionary
#'   
#' @description This is a helper function to convert an \R data.frame object to 
#'   a Tcl data frame object. This function is useful when changing a data state
#'   with \code{\link{l_configure}}.
#'   
#' @param data a data.frame object
#'   
#' @return a string that represents with data.frame with a Tcl dictionary data
#'   structure.
#' 
#' @export
l_data <- function(data) {

    if (!is.data.frame(data)) stop("data is expected to be a data.frame")
    
    ## Create a dict
    dict <- ""
    for (n in names(data)) {
        if (is.character(data[[n]])) {
            #print(paste(n,'character'))
            dict <- paste(dict, ' {', n, '} ' ,
                          "{ ", paste(sapply(data[[n]],
                                             function(s)paste("{",s,"}", sep='')),
                                      collapse=" "),
                          "}", sep="")
        } else if (is.factor(data[[n]])) {
            #print(paste(n,'factor'))
            dict <- paste(dict, ' {', n, '} ' ,
                          "{ ", paste(sapply(as.character(data[[n]]),
                                             function(s)paste("{",s,"}", sep='')),
                                      collapse=" "),
                          "}", sep="")
        } else {
            #print(paste(n,'numeric'))
            dict <- paste(dict, ' {', n, '} ' ,
                          "{ ", paste(data[[n]], collapse=" "),
                          "}", sep="")
        }
    }
    return(dict)
}
