
#' One Dimensional Measures
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

#' @export
print.measures1d <- function(x, ...) {
    cat(paste("1d measure closure with measures:",
              paste(x('measures'), collapse = ', '), "\n"))
}   


#' Two Dimensional Measures
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
    pairs <- t(combn(p, 2))
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

#' @export
print.measures2d <- function(x, ...) {
    cat(paste("2d measure closure with measures:",
              paste(x('measures'), collapse = ', '), "\n"))
}
