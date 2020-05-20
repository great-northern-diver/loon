#' @title Some useful \code{loon} histogram functions
#' @name Extract-Bin-Info
#' @description Here list several useful functions for \code{loon} histograms.
#'
#' @param widget A \code{loon} \code{l_hist} widget.
#'
#' @details
#' \code{getBinData} is utilized to extract \code{loon} bin information,
#' such as the counts, locations, points id of each colored bin.
#' All rest useful functions in this page are built based on it.
#'
#' @export

getBinData <- function(widget) {

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

#' @rdname Extract-Bin-Info
#'
#' @details \code{l_getBinIds} returns the ids of each bin
#'
#' @export
l_getBinIds <- function(widget) {

    stopifnot({
        inherits(widget, "l_hist")
    })

    x <- widget['x']
    len_x <- length(x)
    if(len_x == 0) return(numeric(0))

    bin <- getBinData(widget)

    lapply(bin,
           function(b) {
               b$points$all
           })
}

#' @rdname Extract-Bin-Info
#'
#' @details \code{l_breaks} returns the range (breaks) of each bin
#'
#' @export

l_breaks <- function(widget) {

    stopifnot({
        inherits(widget, "l_hist")
    })

    x <- widget['x']
    len_x <- length(x)
    if(len_x == 0) return(numeric(0))

    bin <- getBinData(widget)

    lapply(bin,
           function(b) {
               c(b$x0, b$x1)
           })
}
