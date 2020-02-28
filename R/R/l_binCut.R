#' @rdname Extract-Bin-Info
#'
#' @inheritParams Extract-Bin-Info
#' @param labels Labels for the levels of the resulting category.
#' By default, labels are constructed using "(a,b]" interval notation.
#' If labels = FALSE, simple integer codes are returned instead of a factor.
#' If labels are recognized as colors, character will be returned directly.
#' @param digits The number of digits used in formatting the breaks.
#'
#' @details \code{l_binCut} divides \code{l_hist} widget \code{x} into current histogram intervals and codes values
#' \code{x} according to which interval they fall. It is very similar to \code{\link{cut}} in \code{base} package.
#'
#' @export
#'
#' @examples
#' # l_binCut
#' p <- l_hist(iris)
#' binCut <- l_binCut(p)
#' p['color'] <- binCut
#' ## ggplot color hue
#' gg_color_hue <- function(n) {
#'   hues <- seq(15, 375, length = n + 1)
#'   hcl(h = hues, l = 65, c = 100)[1:n]
#' }
#'
#' # l_getBinIds
#' ## number of bins
#' binIds <- l_getBinIds(p)
#' p['color'] <- l_binCut(p, labels = gg_color_hue(length(binIds)))
#'
#' ## The count of each bin
#' lengths(binIds)
#' # l_breaks
#' breaks <- l_breaks(p)

l_binCut <- function(widget, labels = NULL, digits = 2) {

    stopifnot({
        inherits(widget, "l_hist")
    })

    x <- widget['x']
    len_x <- length(x)
    if(len_x == 0) return(character(0))

    bin <- getBinData(widget)
    # all bins share one bin width
    # bin left can help to determine groups
    x_left <- c()
    cuts <- rep(0, len_x)
    binwidth <- widget['binwidth']
    lapply(bin,
           function(b) {
               x0 <- b$x0
               pointsId <- b$points$all
               if(x0 %in% x_left) {
                   group_len <- length(x_left)
                   cuts[pointsId] <- group_len
               } else {
                   x_left <<- c(x_left, x0)
                   group_len <- length(x_left)
                   cuts[pointsId] <- group_len
               }
               # update cuts
               cuts <<- cuts
           })

    is.color <- function(colors) {
        col <- sapply(colors,
                      function(color) {
                          tryCatch(is.matrix(grDevices::col2rgb(color)),
                                   error = function(e) FALSE)
                      })

        all(col)
    }

    asFactor <- TRUE

    if(!is.null(labels)) {

        if(is.logical(labels)) {
            if(!labels) return(cuts) else labels <- NULL
        } else {
            if(length(labels) < length(x_left)) {
                warning("The length of label is smaller than the number of bins")
                labels <- rep_len(labels, length(x_left))
            } else {
                if(is.color(labels)) asFactor <- FALSE
            }
        }
    }

    cutsChar <- vapply(cuts,
                       function(cut) {
                           if(is.null(labels)) {
                               x0 <- round(x_left[cut], digits)
                               x1 <- round(x0 + binwidth, digits)
                               paste0("(", x0, ", ", x1, "]")
                           } else {
                               labels[which(x_left[cut] == x_left)]
                           }
                       }, character(1))

    if(asFactor)
        factor(cutsChar)
    else
        cutsChar
}
