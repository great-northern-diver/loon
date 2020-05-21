
#' @title Layer a Raster Image
#'
#' @description This function is very similar to the
#'   \code{\link[graphics]{rasterImage}} function. It works with every loon plot
#'   which is based on the cartesian coordinate system.
#'
#'
#' @inheritParams l_layer_heatImage
#' @inheritParams graphics::rasterImage
#'
#' @templateVar page learn_R_layer
#' @templateVar section countourlines-heatimage-rasterimage
#' @template see_l_help
#'
#' @return layer id of group or rectangles layer
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' plot(1,1, xlim = c(0,1), ylim=c(0,1))
#' mat <- matrix(c(0,0,0,0, 1,1), ncol=2)
#' rasterImage(mat, 0,0,1,1, interpolate = FALSE)
#'
#' p <- l_plot()
#' l_layer_rasterImage(p, mat, 0,0,1,1)
#' l_scaleto_world(p)
#'
#' image <- as.raster(matrix(0:1, ncol = 5, nrow = 3))
#' p <- l_plot(showScales=TRUE, background="thistle", useLoonInspector=FALSE)
#' l_layer_rasterImage(p, image, 100, 300, 150, 350, interpolate = FALSE)
#' l_layer_rasterImage(p, image, 100, 400, 150, 450)
#' l_layer_rasterImage(p, image, 200, 300, 200 + 10, 300 + 10,
#'    interpolate = FALSE)
#' l_scaleto_world(p)
#'
#' # from examples(rasterImage)
#'
#' # set up the plot region:
#' op <- par(bg = "thistle")
#' plot(c(100, 250), c(300, 450), type = "n", xlab = "", ylab = "")
#' rasterImage(image, 100, 300, 150, 350, interpolate = FALSE)
#' rasterImage(image, 100, 400, 150, 450)
#' rasterImage(image, 200, 300, 200 + 10, 300 + 10,
#'            interpolate = FALSE)
#'
#' }

l_layer_rasterImage <-  function (widget, image, xleft, ybottom, xright, ytop,
                                  angle = 0, interpolate = FALSE,
                                  parent = "root", index = "end", ...) {

    l_throwErrorIfNotLoonWidget(widget)

    if(angle != 0) stop("only angle=0 is supported.")
    if(interpolate) warning("interpolation is not supported, interpolate=FALSE used.")

    image <- if (inherits(image, "nativeRaster")) image else grDevices::as.raster(image)

    x <- seq(xleft, xright, length.out = dim(image)[2]+1)
    y <- seq(ytop, ybottom, length.out = dim(image)[1]+1)

    nx <- length(x)
    ny <- length(y)

    x0 <- x[-nx]
    x1 <- x[-1]

    y0 <- y[-ny]
    y1 <- y[-1]

    xcoords <- Map(function(xl, xu){c(xl, xu)}, rep(x0, ny-1), rep(x1, ny-1))
    ycoords <- Map(function(yl, yu){c(yl, yu)}, rep(y0, each=nx-1), rep(y1, each=nx-1))

    color <- substr(as.vector(image),1,7)

    l_layer_rectangles(widget, x=xcoords, y=ycoords, color=color, linecolor="",
                       parent = parent, index=index)

}
