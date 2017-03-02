#' @title Display a Heat Image
#'   
#' @description This function is very similar to the 
#'   \code{\link[graphics]{image}} function. It works with every loon plot which
#'   is based on the cartesian coordinate system.
#'   
#' @inheritParams graphics::image
#' @param x locations of grid lines at which the values in z are measured. These
#'   must be finite, non-missing and in (strictly) ascending order. By default,
#'   equally spaced values from 0 to 1 are used. If x is a list, its components
#'   x$x and x$y are used for x and y, respectively. If the list has component z
#'   this is used for z.
#' @param y see description for the \code{x} argument above
#' @param xlim range for the plotted x values, defaulting to the range of x
#' @param ylim range for the plotted y values, defaulting to the range of y
#' @template param_widget
#' @template param_parent
#' @template param_index
#' @param ... argumnets forwarded to \code{\link{l_layer_line}}
#'   
#' @return layer id of group or rectangles layer
#'   
#' @templateVar page learn_R_layer
#' @templateVar section countourlines-heatimage-rasterimage
#' @template see_l_help
#'   
#' @export
#' 
#' @examples 
#' library(MASS)
#' kest <- with(iris, MASS::kde2d(Sepal.Width,Sepal.Length))
#' image(kest)
#' contour(kest, add=TRUE)
#' 
#' p <- l_plot()
#' lcl <- l_layer_contourLines(p, kest, label='contour lines')
#' limg <- l_layer_heatImage(p, kest, label='heatmap') 
#' l_scaleto_world(p)
#' 
#' # from examples(image)
#' x <- y <- seq(-4*pi, 4*pi, len = 27)
#' r <- sqrt(outer(x^2, y^2, "+"))
#' p1 <- l_plot()
#' l_layer_heatImage(p1, z = z <- cos(r^2)*exp(-r/6), col  = gray((0:32)/32))
#' l_scaleto_world(p1)
#' 
#' image(z = z <- cos(r^2)*exp(-r/6), col  = gray((0:32)/32))
l_layer_heatImage <-function (widget,
                              x = seq(0, 1, length.out = nrow(z)),
                              y = seq(0, 1, length.out = ncol(z)),
                              z, 
                              zlim = range(z[is.finite(z)]), 
                              xlim = range(x),
                              ylim = range(y),
                              col = grDevices::heat.colors(12), 
                              breaks, 
                              oldstyle = FALSE,
                              useRaster,
                              index="end", 
                              parent="root",
                              ...) {
    
    l_throwErrorIfNotLoonWidget(widget)
    
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z; y <- x$y; x <- x$x
            } else {
                if(is.null(dim(x)))
                    stop("argument must be matrix-like")
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        } else stop("no 'z' matrix specified")#' limg <- l_layer_heatimage(p, kest) 
    } else if (is.list(x)) {
        y <- x$y
        x <- x$x
    } 
    
    if (any(!is.finite(x)) || any(!is.finite(y)))
        stop("'x' and 'y' values must be finite and non-missing")
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
        stop("increasing 'x' and 'y' values expected")
    if (!is.matrix(z))
        stop("'z' must be a matrix")
    if (!typeof(z) %in% c("logical", "integer", "double"))
        stop("'z' must be numeric or logical")
    if (length(x) > 1 && length(x) == nrow(z)) { # midpoints
        dx <- 0.5*diff(x)
        x <- c(x[1L] - dx[1L], x[-length(x)] + dx,
               x[length(x)] + dx[length(x)-1])
    }
    if (length(y) > 1 && length(y) == ncol(z)) { # midpoints
        dy <- 0.5*diff(y)
        y <- c(y[1L] - dy[1L], y[-length(y)] + dy,
               y[length(y)] + dy[length(y)-1L])
    }
    
    if (missing(breaks)) {
        nc <- length(col)
        if (!missing(zlim) && (any(!is.finite(zlim)) || diff(zlim) < 0))
            stop("invalid z limits")
        if (diff(zlim) == 0)
            zlim <- if (zlim[1L] == 0) {
                c(-1, 1)  
            } else {
                zlim[1L] + c(-.4, .4)*abs(zlim[1L])
            }
        
        z <- (z - zlim[1L])/diff(zlim)
        zi <- if (oldstyle) {
            floor((nc - 1) * z + 0.5)
        } else {
            floor((nc - 1e-5) * z + 1e-7)
        }
        zi[zi < 0 | zi >= nc] <- NA
    } else {
        if (length(breaks) != length(col) + 1)
            stop("must have one more break than colour")
        if (any(!is.finite(breaks)))
            stop("'breaks' must all be finite")
        if (is.unsorted(breaks)) {
            warning("unsorted 'breaks' will be sorted before use")
            breaks <- sort(breaks)
        }
        ## spatstat passes a factor matrix here, but .bincode converts to double
        zi <- .bincode(z, breaks, TRUE, TRUE) - 1L
    }
    
    ## need plot set up before we do this
    if (length(x) <= 1) x <- graphics::par("usr")[1L:2]
    if (length(y) <= 1) y <- graphics::par("usr")[3:4]
    if (length(x) != nrow(z)+1 || length(y) != ncol(z)+1)
        stop("dimensions of z are not length(x)(-1) times length(y)(-1)")
    
    check_irregular <- function(x, y)
    {
        # check that the grid is regular
        dx <- diff(x)
        dy <- diff(y)
        (length(dx) && !isTRUE(all.equal(dx, rep(dx[1], length(dx))))) ||
            (length(dy) && !isTRUE(all.equal(dy, rep(dy[1], length(dy)))))
    }
    if (missing(useRaster)) {
        useRaster <- getOption("preferRaster", FALSE)
        if (useRaster && check_irregular(x, y)) useRaster <- FALSE
        if (useRaster) {
            useRaster <- FALSE
            ras <- grDevices::dev.capabilities("rasterImage")$rasterImage
            if(identical(ras, "yes")) useRaster <- TRUE
            if(identical(ras, "non-missing")) useRaster <- all(!is.na(zi))
        }
    }
    if (useRaster) {#' limg <- l_layer_heatimage(p, kest) 
        if(check_irregular(x,y))
            stop(gettextf("%s can only be used with a regular grid",
                          sQuote("useRaster = TRUE")),
                 domain = NA)
        # this should be mostly equivalent to RGBpar3 with bg = R_TRANWHITE
        if (!is.character(col)) {
            col <- as.integer(col)
            # if (any(!is.na(col) & col < 0L)).External.graphics(C_image, x, y, zi, col)
            stop("integer colors must be non-negative")
            col[col < 1L] <- NA_integer_
            p <- grDevices::palette()
            col <- p[((col - 1L) %% length(p)) + 1L]
        }
        zc <- col[zi + 1L]
        dim(zc) <- dim(z)
        zc <- t(zc)[ncol(zc):1L,, drop = FALSE]
        
        id <- l_layer_rasterImage(widget, grDevices::as.raster(zc), min(x), min(y), max(x), max(y))
        
    } else {
        ## by column
        
        nx <- length(x)
        x0 <- x[-nx]
        x1 <- x[-1]
        
        ny <- length(y)
        y0 <- y[-ny]
        y1 <- y[-1]	
        
        xcoords <- Map(function(xl, xu){c(xl, xu)}, rep(x0, ny-1), rep(x1, ny-1))
        ycoords <- Map(function(yl, yu){c(yl, yu)}, rep(y0, each=nx-1), rep(y1, each=nx-1))
        color <- substr(col[as.vector(zi)+1],1,7)
        
        id <- l_layer_rectangles(widget, x=xcoords, y=ycoords, color=color, linecolor="",
                                 parent = parent, index=index, ...)
    }
    
    return(id)
}
