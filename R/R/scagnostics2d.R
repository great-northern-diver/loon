
#' @title Closure of Two Dimensional Scagnostic Measures
#'   
#' @description Function creates a 2d measures object that can be used with 
#'   \code{\link{l_ng_plots}} and \code{\link{l_ng_ranges}}.
#'   
#'   
#' @inheritParams measures1d
#' @param scagnostics vector with valid scanostics meausure names, i.e "Clumpy",
#'   "Monotonic", "Convex", "Stringy", "Skinny", "Outlying", "Sparse", 
#'   "Striated", "Skewed". Also the prefix "Not" can be added to each measure
#'   which equals 1-measure.
#' @template param_separator
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
#' m <- scagnostics2d(oliveAcids, separator='**')
#' m
#' m()
#' m(olive$palmitoleic > 80)
#' m('data')
#' m('grid')
#' m('measures')
scagnostics2d <- function(data, scagnostics=c(
                          "Clumpy",
                          "Monotonic",
                          "Convex", 
                          "Stringy",
                          "Skinny", 
                          "Outlying",
                          "Sparse", 
                          "Striated", 
                          "Skewed"),
                          separator=':') {
    
    if (!is.data.frame(data))
        data <- as.data.frame(data)
    

    if (!requireNamespace("scagnostics", quietly = TRUE)) {
        stop("scagnostics package needed for this function to work. Please install it.",
             call. = FALSE)
    }
    
    allMeasures <- c( "Clumpy", "NotClumpy",
                      "Monotonic", "NotMonotonic", "Convex", "NotConvex", "Stringy",
                      "NotStringy", "Skinny", "NotSkinny", "Outlying","NotOutlying",
                      "Sparse", "NotSparse", "Striated", "NotStriated", "Skewed",
                      "NotSkewed")
    
    if (length(scagnostics)==1) {
        if (scagnostics == 'all') {
            scagnostics <- allMeasures
        }
    }
    
    ## check if all args are ok
    scagnostics <- match.arg(scagnostics, allMeasures, TRUE)
    
    n <- dim(data)[1]
    p <- dim(data)[2]
    
    tmp_scags <- scagnostics::scagnostics.data.frame(data[1:min(10,n),])
    grid <- scagnostics::scagnosticsGrid(tmp_scags)
    rownames(grid) <- gsub(' * ', separator, colnames(tmp_scags), fixed=TRUE)
    
        
    structure(
        function(keep) {
            if (missing(keep))
                keep <- rep(TRUE, n)
            
            if (length(keep) == 1) {
                if (keep == 'data') {
                    return(data)
                } else if (keep == 'grid') {
                    return(grid)
                } else if (keep == 'measures') {
                    return(scagnostics)
                } else if (keep == 'separator') {
                    return(separator)
                }
            }
            
            ## if not keyword return measures
            if (!is.logical(keep))
                stop('keep must be logical')
            
            if (length(keep) != n)
                stop(paste0('leength of keep is ', length(keep), ' but must be ', n))
            
            subsetData <- subset(data, keep)
            
            tryCatch( {
                scags <- t(unclass(scagnostics::scagnostics.data.frame(subsetData)))
                rownames(scags) <- gsub(" * ", separator, rownames(scags), fixed = TRUE)
                
            },
            error = function(e) {
                stop(e)
            } )
            
            X <- vapply(scagnostics, FUN=function(measure) {
                if (substr(measure, 1,3) == "Not") {
                    1 - scags[, substring(measure,4)]
                } else {
                    scags[, measure]
                }
            }, numeric(dim(scags)[1]), USE.NAMES=TRUE)
            
            rownames(X) <- rownames(scags)
            X
        },
        class=c('measures', 'measures2d')
    )
}
