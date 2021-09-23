#' @title Instantiate a Grob
#' @name loonGrobInstantiation
#' @description Functions used to
#' instantiate grob descriptions appearing in the \code{gTree} produced
#' by \code{loonGrob()}.
#' @param grobDesc A grob description. Generally, it is created by the
#' function \code{grob()}.
#' @param grobFun A new grob function. If missing,
#' a best guess (based on \code{gPath}) will be tried.
#' @param ... arguments used to set in the new grob function
#' @details \itemize{
#' \item {\code{l_updateGrob}: query arguments from a grob description and assign
#' these arguments to a new grob function.}
#' \item {\code{l_instantiateGrob}: query a descendant from a \code{loonGrob},
#' update it via a new grob function, then return the new editted \code{loonGrob}}
#' }
#' @seealso \code{\link{loonGrob}}
#' @export
#' @examples
#' library(grid)
#' grobDesc <- grob(label = "loon",
#'                  gp = gpar(col = "red"))
#' grid.newpage()
#' # Nothing is displayed
#' grid.draw(grobDesc)
#' textDesc <- l_updateGrob(grobDesc, grid::textGrob)
#' grid.newpage()
#' # label "loon" is shown
#' grid.draw(textDesc)
#'
#' if(interactive()) {
#' # a loon plot with hidden labels
#' p <- l_plot(iris, showLabels = FALSE)
#' lg <- loonGrob(p)
#' # x label and y label are invisible
#' grid.newpage()
#' grid.draw(lg)
#'
#' # show x label
#' lg <- l_instantiateGrob(lg, "x label: textGrob arguments")
#' # show y label
#' lg <- l_instantiateGrob(lg, "y label: textGrob arguments")
#' # reset margins
#' lg <- l_setGrobPlotView(lg)
#' grid.newpage()
#' grid.draw(lg)
#'
#' # show axes
#' if(packageVersion("loon") < '1.3.8') {
#'   lg <- l_instantiateGrob(lg, "x axis: .xaxisGrob arguments")
#'   lg <- l_instantiateGrob(lg, "y axis: .yaxisGrob arguments")
#' } else {
#'   lg <- l_instantiateGrob(lg, "x axis: xaxisGrob arguments")
#'   lg <- l_instantiateGrob(lg, "y axis: yaxisGrob arguments")
#' }
#'
#' lg <- l_setGrobPlotView(lg)
#' grid.newpage()
#' # the labels are too close to the plot
#' grid.draw(lg)
#'
#' # reset the labels' positions
#' lg <- l_instantiateGrob(lg, "x label: textGrob arguments",
#'                         y = unit(-3.5, "lines"))
#' lg <- l_instantiateGrob(lg, "y label: textGrob arguments",
#'                         x = unit(-6.5, "lines"))
#' grid.newpage()
#' grid.draw(lg)
#' }
#'
l_updateGrob <- function (grobDesc, grobFun, ...) {

    grobNames <- names(grobDesc)
    grobArgs <- setNames(
        lapply(grobNames, function(name) grobDesc[[name]]),
        grobNames
    )
    dotArgs <- list(...)

    keys <- unique(c(names(dotArgs), names(grobArgs)))
    # `dotArgs` (set in ...) are the priority
    #
    # For example,
    # --------------------------
    # dotArgs <- list(x = -10, y = -10, size = 5)
    # grobArgs <- list(x = 1, y = 1, label = "loon")
    # # the returned args should be
    # list(x = -10, y = -10, size = 5, label = "loon")
    # --------------------------
    args <- setNames(
        mapply(function(x, y) if(is.null(x)) y else x,
               dotArgs[keys],
               grobArgs[keys],
               SIMPLIFY = FALSE),
        keys)

    tryCatch(
        do.call(grobFun, args),
        error = function(e) {
            args <- args[names(args) %in% formalArgs(grobFun)]
            do.call(grobFun, args)
        }
    )
}

#' @rdname loonGrobInstantiation
#' @param loonGrob A loonGrob (a \code{gTree} object)
#' @param gPath A \code{grob} path object specifing a descendant
#' of the specified \code{gTree}
#' @export
l_instantiateGrob <- function (loonGrob, gPath, grobFun, ...) {

    if (missing(grobFun)) {
        # Try finding it in path
        grobString <- gsub(".*: (.+) arguments.*", "\\1", gPath)
        grobFun <- tryCatch(
            get(grobString),
            error = function(e) {
                NULL
            }
        )

        if(is.function(grobFun)) {
            grobFun <- grobString
        } else {
            stop("Must supply a grobFun, e.g. textGrob", call. = FALSE)
        }
    }
    grobDesc <- getGrob(loonGrob, gPath)
    newGrob <- l_updateGrob(grobDesc, grobFun, ...)
    setGrob(
        gTree = loonGrob,
        gPath = gPath,
        newGrob = newGrob
    )
}

#' @rdname loonGrobInstantiation
#' @param margins plot view margins. If missing, a loon default margin will be
#' used.
#' @export
l_setGrobPlotView <- function (loonGrob, margins) {

    loonplot <- getGrob(loonGrob, "loon plot")

    if (missing(margins)) {
        # Figure it out
        showLabels <- FALSE
        showScales <- FALSE

        `%||%` <- function(a, b) {
            if (!is.null(a)) a else b
        }

        xaxis_grob <- getGrob(loonplot, "x axis: xaxisGrob arguments") %||% getGrob(loonplot, "x axis")
        if(inherits(xaxis_grob, "xaxis"))
            showScales <- TRUE

        xlabel_grob <- getGrob(loonplot, "x label: textGrob arguments") %||% getGrob(loonplot, "x label")
        xlabel <- xlabel_grob$label
        if(inherits(xlabel_grob, "text"))
            showLabels <- TRUE
        ylabel_grob <- getGrob(loonplot, "y label: textGrob arguments") %||% getGrob(loonplot, "y label")
        ylabel <- ylabel_grob$label
        title_grob <- getGrob(loonplot, "title: textGrob arguments") %||% getGrob(loonplot, "title")
        title <- title_grob$label

        minimumMargins <- rep(1, 4)
        margins <- c(0, 0, 0, 0)
        if (showLabels) {
            labelMargins <- c(1.5, 1.5, 3, 0)
            if(xlabel == "") labelMargins[1] <- minimumMargins[1]
            if(ylabel == "") labelMargins[2] <- minimumMargins[2]
            if(title == "") labelMargins[3] <- minimumMargins[3]
            margins <- margins + labelMargins
        }
        if(showScales) {margins <- margins + c(1.5, 4, 0, 0)}
        if(showLabels || showScales) {
            margins <- apply(cbind(margins, minimumMargins), 1, max)
        }
    }

    namesVp <- vapply(loonplot$vp, function(x) x$name, character(1L))

    loonplot$vp[[which(namesVp == "plotViewport")]] <-
        plotViewport(margins = margins,
                     name = "plotViewport")

    setGrob(
        gTree = loonGrob,
        gPath = "loon plot",
        newGrob = loonplot
    )
}
