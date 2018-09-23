#' @export
l_getPlots.l_ts <- function(target){
    # throw errors if not loon widget
    lapply(target, 
           function(tar){l_throwErrorIfNotLoonWidget(tar) }
    )
    target
}

#' @export
l_getLocations.l_ts <- function(target){
    nPlots <- length(target)
    list(
        nrow = nPlots,
        ncol = 1,
        heights = c(1.3, rep(1, nPlots-1))
    )
}

