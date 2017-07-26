
#' @title Closure of One Dimensional Measures
#'   
#' @description Function creates a 1d measures object that can be used with 
#'   \code{\link{l_ng_plots}} and \code{\link{l_ng_ranges}}.
#'   
#' @param data a data.frame with the data used to calculate the measures
#' @param ... named arguments, name is the function name and argument is the 
#'   function to calculate the measure for each variable.
#'   
#'   
#' @templateVar page  learn_R_display_graph
#' @templateVar section measures
#' @template see_l_help
#'   
#' @return a measures object
#'   
#' @seealso \code{\link{l_ng_plots}}, \code{\link{l_ng_ranges}},
#'   \code{\link{measures2d}}
#'   
#'   
#' @export
#' 
#' @examples
#' m1 <- measures1d(oliveAcids, mean=mean, median=median,
#'      sd=sd, q1=function(x)as.vector(quantile(x, probs=0.25)),
#'      q3=function(x)as.vector(quantile(x, probs=0.75)))
#'      
#' m1
#' m1()
#' m1(olive$palmitoleic>100)
#' m1('data')
#' m1('measures')
measures1d <- function(data, ...) {
    
    if (!is.data.frame(data))
        data <- as.data.frame(data)
    
    n <- dim(data)[1]
    p <- dim(data)[2]
    
    funs <- list(...)
    if(!is.null(funs[['separator']])) {
        separator <- funs[['separator']]
        funs[['separator']] <- NULL
    } else {
        separator <- ':'
    }
    if (length(funs)<1) {
        stop("specify functions")
    }
    
    structure(
        function(keep) {
            if (missing(keep))
                keep <- rep(TRUE, n)
            
            ## Check for keywords
            if(length(keep)==1) {
                if(keep=='data') {
                    return(data)
                } else if(keep == 'measures') {
                    return(names(funs))
                } else if(keep == 'separator') {
                    return(separator)
                }
            }
            
            ## if not keyword, calculate measures
            if (!is.logical(keep)) 
                stop('keep must be logical')
            
            if (length(keep) != n) 
                stop(paste0('length of keep is ', length(keep), ' but must be ', n))
            
            vapply(funs,
                   function(fun) { sapply(subset(data, keep), fun) },
                   numeric(p),
                   USE.NAMES = TRUE)
            
        },
        class=c('measures', 'measures1d')
    )
}


#' @title Print function names from measure1d object
#'   
#' @description Prints the function names of a measure1d object using
#'   \code{print.default}.
#'   
#' @param x measures1d object
#' @param ... arguments passed on to print.default
#'   
#' @export
print.measures1d <- function(x, ...) {
    print.default(paste("1d measure closure with measures:",
              paste(x('measures'), collapse = ', '), "\n"), ...)
}   



#' @title Closure of Two Dimensional Measures
#'   
#' @description Function creates a 2d measures object that can be used with 
#'   \code{\link{l_ng_plots}} and \code{\link{l_ng_ranges}}.
#' 
#' 
#' @inheritParams measures1d
#'   
#'   
#' @templateVar page  learn_R_display_graph
#' @templateVar section measures
#' @template see_l_help
#'   
#' @return a measures object
#'   
#' @seealso \code{\link{l_ng_plots}}, \code{\link{l_ng_ranges}},
#'   \code{\link{measures2d}}
#'   
#' @export
#' 
#' @examples 
#' m <- measures2d(oliveAcids, separator='*', cov=cov, cor=cor)
#' m
#' m()
#' m(keep=olive$palmitic>1360)
#' m('data')
#' m('grid')
#' m('measures')
measures2d <- function(data, ...) {
    
    if (!is.data.frame(data))
        data <- as.data.frame(data)
    
    funs <- list(...)
    if(!is.null(funs[['separator']])) {
        separator <- funs[['separator']]
        funs[['separator']] <- NULL
    } else {
        separator <- ':'
    }
    if (length(funs)<1) {
        stop("specify functions")
    }
    
    n <- dim(data)[1]
    p <- dim(data)[2]
    pairs <- t(utils::combn(p, 2))
    colnames(pairs) <- c('x','y')
    
    rownames(pairs) <- apply(pairs, 1, function(pair) {
        paste(names(data)[pair], collapse= separator)
    })
    
    structure(
        function(keep) {
            if (missing(keep))
                keep <- rep(TRUE, n)
            
            if (length(keep) == 1) {
                if (keep == 'data') {
                    return(data)
                } else if (keep == 'grid') {
                    return(pairs)
                } else if (keep == 'measures') {
                    return(names(funs))
                } else if (keep == 'separator') {
                    return(separator)
                }
            }
            
            ## if not keyword return measures
            if (!is.logical(keep))
                stop('keep must be logical')
            
            if (length(keep) != n) 
                stop(paste0('leength of keep is ', length(keep), ' but must be ', n))
            
            X <- vapply(funs, function(fun) {
                apply(pairs, 1, function(pair) {
                    fun(data[keep, pair[1]], data[keep, pair[2]])
                })
            }, numeric(dim(pairs)[1]))
            
            rownames(X) <- rownames(pairs)
            X
        },
        class=c('measures', 'measures2d')
    )
}

#' @title Print function names from measure2d object
#'   
#' @description Prints the function names of a measure2d object using
#'   \code{print.default}.
#' 
#' @inheritParams print.measures1d  
#' @param x measures2d object
#' 
#' @export
print.measures2d <- function(x, ...) {
    cat(paste("2d measure closure with measures:",
              paste(x('measures'), collapse = ', '), "\n"))
}
