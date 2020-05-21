#' @title Layer Contour Lines
#'
#' @description This function is a wrapper around
#'   \code{\link[grDevices]{contourLines}} that adds the countourlines to a loon
#'   plot which is based on the cartesian coordinate system.
#'
#' @inheritParams grDevices::contourLines
#' @param y see description for the \code{x} argument
#' @template param_widget
#' @param asSingleLayer if \code{TRUE} a lines layer is used for the line,
#'   otherwise if \code{FALSE} a group with nested line layers for each line is
#'   created
#' @template param_parent
#' @template param_index
#' @param ... argumnets forwarded to \code{\link{l_layer_line}}
#'
#' @templateVar page learn_R_layer
#' @templateVar section countourlines-heatimage-rasterimage
#' @template see_l_help
#'
#' @return layer id of group or lines layer
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' p <- l_plot()
#' x <- 10*1:nrow(volcano)
#' y <- 10*1:ncol(volcano)
#' lcl <- l_layer_contourLines(p, x, y, volcano)
#' l_scaleto_world(p)
#'
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'
#'   p1 <- with(iris, l_plot(Sepal.Length~Sepal.Width, color=Species))
#'   lcl <- with(iris, l_layer_contourLines(p1, MASS::kde2d(Sepal.Width,Sepal.Length)))
#'
#'   p2 <- with(iris, l_plot(Sepal.Length~Sepal.Width, color=Species))
#'   layers <- sapply(split(cbind(iris, color=p2['color']), iris$Species), function(dat) {
#'        kest <- with(dat, MASS::kde2d(Sepal.Width,Sepal.Length))
#'        l_layer_contourLines(p2, kest, color=as.character(dat$color[1]), linewidth=2,
#'             label=paste0(as.character(dat$Species[1]), " contours"))
#'   })
#' }
#'
#' }


l_layer_contourLines <- function (widget, x = seq(0, 1, length.out = nrow(z)),
                              y = seq(0, 1, length.out = ncol(z)),
                              z,
                              nlevels = 10, levels = pretty(range(z,na.rm = TRUE), nlevels),
                              asSingleLayer=TRUE, parent="root", index="end", ...) {

    l_throwErrorIfNotLoonWidget(widget)

    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            } else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        } else stop("no 'z' matrix specified")
    } else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
        stop("increasing 'x' and 'y' values expected")
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1)
        stop("no proper 'z' matrix specified")
    if (1 * length(x) * length(y) != length(z))
        stop("dimensions of 'x', 'y' and 'z' do not match")


    lines <- grDevices::contourLines(x=x,y=y,z=z,nlevels=nlevels, levels=levels)
    levels <- vapply(lines, function(x)x$level, numeric(1))
    xcoords <- sapply(lines, FUN=function(x)x$x)
    ycoords <- sapply(lines, FUN=function(x)x$y)

    if(length(levels) != length(xcoords) && length(levels) != length(ycoords))
        stop("cannot parse output of contourLines correct.")

    if(asSingleLayer) {
        id <- l_layer_lines(widget, x=xcoords, y=ycoords, parent=parent, index=index, tag=levels, ...)
    } else {
        id <- l_layer_group(widget, label="contour lines", parent=parent, index=index)
        ids <- Map(function(l, x, y) {
            l_layer_line(widget, x=x, y=y, label=l, parent=id, ...)
        }, levels, xcoords, ycoords)
        attr(id, "ids") <- ids
    }
    attr(id, "levels") <- levels
    return(id)
}
