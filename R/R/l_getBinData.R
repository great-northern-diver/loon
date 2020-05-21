#' @title Get information on current bins from a histogram
#'
#' @description Queries the histogram and returns information about all active cases
#' contained by the histogram's bins.
#'
#' @param widget A loon histogram widget.
#'
#' @return A nested list of the bins in the histogram which contain active points.
#' Each bin is a list of the counts, the point indices, and the minimum (x0) and maximum (x1)
#' of that bin.  Loon histogram bins are open  on the left and closed on the right by default, namely "(x0, x1]".
#' The counts and the points further identify the number and ids of all points,
#' those which are selected, and those of each colour in that bin (identified by their hex12 colour from tcl).
#'
#' @seealso \code{\link{l_getBinIds}}, \code{\link{l_breaks}},
#'   \code{\link{l_binCut}}
#'
#' @export
#'
#'
l_getBinData <- function(widget) {

    stopifnot({inherits(widget, "l_hist")})
    l_throwErrorIfNotLoonWidget(widget)

    tcl_obj_varname <- function(widget, varname = NULL) {
        x <- tcl("info", "object", "namespace", widget)

        if (!is.null(varname)) {
            x <- paste(x, varname, sep="::")
        }
        x
    }

    dict_get <- function(d, keys) {
        .Tcl(paste0("dict get $", d, " ", paste(keys, collapse = " ")))
    }

    dict_with <- function(d, expr) {
        as.character(.Tcl(paste("dict with", paste(d, collapse = " "), paste("{", expr, "}"))))
    }

    tclbins <- tcl_obj_varname(widget, "bins")

    ## see oo_Histogram_Model.tcl
    binNames <- sort(as.numeric(dict_with(tclbins, "dict keys $bin")))
    setNames(
        lapply(binNames,
               function(binid) {

                   keys_count <- dict_with(c(tclbins, "bin", binid), "dict keys $count")
                   keys_points <- dict_with(c(tclbins, "bin", binid), "dict keys $points")

                   list(
                       count = sapply(keys_count, function(x) {
                           as.numeric(dict_get(tclbins, c("bin", binid, "count", x)))
                       }, USE.NAMES = TRUE, simplify = FALSE),
                       points = sapply(keys_points, function(x) {
                           as.numeric(dict_get(tclbins, c("bin", binid, "points", x))) + 1
                       }, USE.NAMES = TRUE, simplify = FALSE),
                       x0 = as.numeric(dict_get(tclbins, c("bin", binid, "x0"))),
                       x1 = as.numeric(dict_get(tclbins, c("bin", binid, "x1")))
                   )
               }),
        paste("bin", binNames + 1)
    )
}

#' @title Gets the ids of the active points in each bin of a histogram
#'
#' @description Queries the histogram and returns the ids of all active points in each bin that contains active points.
#'
#' @param widget A loon histogram widget.
#'
#' @return A named list of the bins in the histogram and the ids of their active points.
#'
#' @seealso \code{\link{l_getBinData}}, \code{\link{l_breaks}},
#'   \code{\link{l_binCut}}
#'
#' @export
#'
#'
l_getBinIds <- function(widget) {

    stopifnot({
        inherits(widget, "l_hist")
    })

    x <- widget['x']
    len_x <- length(x)
    if(len_x == 0) return(numeric(0))

    bin <- l_getBinData(widget)

    lapply(bin,
           function(b) {
               b$points$all
           })
}

#'
#' @title Gets the boundaries of the histogram bins containing active points.
#'
#' @description Queries the histogram and returns the ids of all active points in each bin that contains active points.
#'
#' @param widget A loon histogram widget.
#'
#' @return A named list of the minimum and maximum values of the boundaries for each active bins in the histogram.
#'
#' @seealso \code{\link{l_getBinData}}, \code{\link{l_getBinIds}},
#'   \code{\link{l_binCut}}
#'
#' @export
#'
#'

l_breaks <- function(widget) {

    stopifnot({
        inherits(widget, "l_hist")
    })

    x <- widget['x']
    len_x <- length(x)
    if(len_x == 0) return(numeric(0))

    bin <- l_getBinData(widget)

    lapply(bin,
           function(b) {
               c(b$x0, b$x1)
           })
}
