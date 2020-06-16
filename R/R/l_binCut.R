#' @title Get labels for each observation according to bin cuts in the histogram.
#'
#' @description
#' \code{l_binCut} divides \code{l_hist} widget \code{x} into current histogram intervals and codes values
#' \code{x} according to which interval they fall (if active). It is modelled on \code{\link{cut}} in \code{base} package.
#'
#' @param widget A loon histogram widget.
#' @param labels  Labels to identify which bin observations are in.
#' By default, labels are constructed using "(a,b]" interval notation.
#' If \code{labels = FALSE}, simple integer codes given by the histogram's bin number are returned instead of a factor.
#' The \code{labels} can also be any vector of length equal to the number of bins; these will be used to
#' construct a vector identifying the bins.
#' @param digits The number of digits used in formatting the breaks for default labels.
#' @param inactive The value to use for inactive observations when labels is a vector.
#' Default depends on \code{labels}.
#'
#' @return A vector of bin identifiers having length equal to the total number of observations in the histogram.
#' The type of vector depends on the \code{labels} argument.
#' For default \code{labels = NULL}, a factor is returned, for \code{labels = FALSE}, a vector of bin numbers, and
#' for arbitrary vector \code{labels} a vector of bins labelled in order of \code{labels} will be returned.
#' Inactive cases appear in no bin and so are assigned the value of \code{active} when given.
#' The default \code{active} value also depends on \code{labels}:  when \code{labels = NULL}, the default \code{active} is \code{"(-Inf, Inf)"};
#' when 'code{labels = FALSE}, the default \code{active} is  \code{-1}; and when \code{labels} is a vector of length equal
#' to the number of bins, the default \code{active} is \code{NA}.
#' The value of \code{active} denotes the bin name for the inactive cases.
#'
#' @seealso \code{\link{l_getBinData}}, \code{\link{l_getBinIds}}, \code{\link{l_breaks}}
#'
#'
#'
#' @export
#'
#' @examples
#' h <- l_hist(iris)
#' h["active"] <- iris$Species != "setosa"
#' binCut <- l_binCut(h)
#' h['color'] <- binCut
#' ## number of bins
#' nBins <- length(l_getBinIds(h))
#' ## ggplot color hue
#' gg_color_hue <- function(n) {
#'   hues <- seq(15, 375, length = n + 1)
#'   hcl(h = hues, l = 65, c = 100)[1:n]
#' }
#' h['color'] <- l_binCut(h, labels = gg_color_hue(nBins), inactive = "firebrick")
#' h["active"] <- TRUE
l_binCut <- function(widget, labels, digits = 2, inactive) {

    stopifnot({inherits(widget, "l_hist")})

    if(missing(labels)) labels <- NULL
    x <- widget['x']
    len_x <- length(x)
    if(len_x == 0) return(character(0))

    # Get the cuts from the active bins of the histogram
    bins <- l_getBinData(widget)
    nBins <- length(bins)
    # all bins share one bin width
    binwidth <- widget['binwidth']
    # bin left can determine groups
    x_left <- numeric(length=len_x)
    binIndex <- numeric(length=len_x)
    activeCases <- logical(length = len_x)
    #cuts <- rep(-1, len_x)
    lapply(1:nBins,
           function(i) {
               bin <- bins[[i]]
               pointsIds <- bin$points$all
               binIndex[pointsIds] <<- i
               x_left[pointsIds] <<- bin$x0
               activeCases[pointsIds] <<- TRUE
           })

    # When labels == NULL
    if (is.null(labels)){
        # fix inactive
        if(missing(inactive)) {
            inactive <- "(-Inf, Inf)"
        }
        # Construct labels
        binLabels <- sapply(x_left,
                            function(left) {
                                x0 <- round(left, digits)
                                x1 <- round(left + binwidth, digits)
                                paste0("(", x0, ", ", x1, "]")
                            })
        binLabels[!activeCases] <- inactive
        binLabels <- as.factor(binLabels)
    } else {
        if(is.logical(labels)) {
            if (labels) {
                stop("Ambiguous value: labels = TRUE.")
            } else {
                # labels == FALSE
                binLabels <- as.numeric(sapply(names(bins),
                                               function(binName){sub("bin ", "", binName)}))
                # fix inactive
                if(missing(inactive)) {
                    inactive <- -1
                } else {
                    if(!is.integer(inactive)) {
                        inactive <- -1
                    }
                }
                binLabels[!activeCases] <- inactive
            }
        } else {
            # General labels
            if(length(labels) != nBins) stop(paste0("labels must be of length = number of bins = ", nBins))
            binLabels <- rep(labels, length.out = len_x)
            if(missing(inactive)) inactive <- NA
            activeIndex <- binIndex[activeCases]
            binLabels[activeCases] <- labels[activeIndex]
            binLabels[!activeCases] <- inactive

        }
    }

    binLabels
}
