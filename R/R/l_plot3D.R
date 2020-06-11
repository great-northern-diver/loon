#' @title Scale for 3d plotting
#'
#' @description \code{l_scale3D} scales its argument in a variety of ways
#' used for 3D visualization.
#' @family three-dimensional plotting functions
#' @param x the matrix or data.frame whose columns are to be scaled.
#' Any \code{NA} entries will be preserved but ignored in calculations.
#' \code{x} must have exactly 3 columns for \code{method = "sphere"}.
#' @param center either a logical value or numeric-alike vector of length equal
#' to the number of columns of \code{x}, where ‘numeric-alike’ means that
#' \code{as.numeric(.)}
#' will be applied successfully if \code{is.numeric(.)} is not true.
#' @param method the scaling method to use.
#' If \code{method = "box"} (the default) then the columns are scaled to
#' have equal ranges and, when \code{center = TRUE}, to be centred by the
#' average of the min and max;
#' If \code{method = "sphere"} then \code{x} must be three dimensional.
#' For sphering, on each of the original 3 dimensions \code{x} is first centred
#' (mean centred when \code{center = TRUE}) and scaled to equal standard deviation on.
#' The V matrix of the singular value decomposition (svd) is applied to the right
#' resulting in uncorrelated variables. Coordinates are then divided by (non-zero as
#' tested by \code{!all.equal(0, .)}) singular values.
#' If \code{x} contains no \code{NA}s, the resulting coordinates are simply the
#' U matrix of the svd.
#'
#' @seealso \code{\link{l_plot3D}}, \code{\link{scale}}, and \code{\link{prcomp}}.
#'
#' @return a data.frame whose columns are centred and scaled according to
#' the given arguments. For \code{method = "sphere")}, the three variable names are
#' \code{x1},  \code{x2}, and  \code{x3}.
#'
#' @examples
#'
#' ##### Iris data
#' #
#' # All variables (including Species as a factor)
#' result_box <- l_scale3D(iris)
#' head(result_box, n = 3)
#' apply(result_box, 2, FUN = range)
#' # Note mean is not zero.
#' apply(result_box, 2, FUN = mean)
#'
#'
#' # Sphering only on 3D data.
#' result_sphere <- l_scale3D(iris[, 1:3], method = "sphere")
#' head(result_sphere, n = 3)
#' apply(result_sphere, 2, FUN = range)
#' # Note mean is numerically zero.
#' apply(result_sphere, 2, FUN = mean)
#'
#'
#' #  With NAs
#' x <- iris
#' x[c(1, 3), 1] <- NA
#' x[2, 3] <- NA
#'
#' result_box <- l_scale3D(x)
#' head(result_box, n = 5)
#' apply(result_box, 2, FUN = function(x) {range(x, na.rm = TRUE)})
#'
#' # Sphering only on 3D data.
#' result_sphere <- l_scale3D(x[, 1:3], method = "sphere")
#' # Rows having had any NA are all NA after sphering.
#' head(result_sphere, n = 5)
#' # Note with NAs mean is no longer numerically zero.
#' # because centring was based on all non-NAs in each column
#' apply(result_sphere, 2, FUN = function(x) {mean(x, na.rm = TRUE)})
#'
#'
#' @export
#'
l_scale3D <- function(x,
                      center = TRUE,
                      method = c("box", "sphere")){
    if (is.data.frame(x)) {
        if (sum(sapply(x, FUN = function(v) !is.numeric(v))) > 0){
            x <- data.frame(lapply(x, as.numeric))
        }
    } else {
        if (is.matrix(x)) {
            if (!is.numeric(x)) {
                stop("A matrix x must be numeric")
            }
        } else {
            stop("x must be a data.frame or a numeric matrix")
        }
    }
    #


    method = match.arg(method)
    if (method == "box") {
        ranges <- apply(x, 2, FUN = function(x) range(x, na.rm =TRUE))
        width <- apply(ranges, 2, FUN = diff)

        if (is.logical(center)) {
            if (center){
                center <- apply(ranges, 2, FUN = mean)
            }
        }

        scaled_x <- as.data.frame(scale(x, center = center, scale = width))

        result <- scaled_x
    } else {
        if (method == "sphere") {
            # Only allow sphering for 3D data.
            # If the user wants more dimensions (or fewer)
            # then they can do principal components themselves.
            #
            if(ncol(x) != 3) {
                stop(
                    paste0("The sphere method is only to be used on three dimensional data, not ncol(x) = ",
                           ncol(x), ".") )
            }
            sds <- apply(x, 2, FUN = function(xi) sd(xi, na.rm =TRUE))

            if (is.logical(center)) {
                if (center){
                    center <- apply(x, 2, FUN = function(xi) mean(xi, na.rm =TRUE))
                }
            }

            scaled_x <- as.data.frame(scale(x, center = center, scale = sds))

            if (sum(is.na(scaled_x)) == 0) {
                # No NAs
                result <- svd(scaled_x)$u
            } else # there are NAs
            {   # Need to preserve all rows in result
                # but use only complete rows for svd.
                svd_result <- svd(na.omit(scaled_x))
                # Note some rows in result will become all NA
                result <- as.matrix(scaled_x) %*% svd_result$v
                d <- svd_result$d
                for (i in 1:3) {
                    if (!is.logical(all.equal(d[i],  0))) {
                        result[, i] <- result[, i] / d[i]
                    } else {
                        warning(paste0("Singular value ", 3, " was ", d[i],
                                       " suggesting a singular matrix.",
                                       "  No scaling done on x", i, "."))
                    }
                }
            }
            result  <- as.data.frame(result)
            names(result) <- paste0("x", 1:ncol(result))
        } else {
            stop("Unknown method")
        }
    }
    result
}

#' @title Create an interactive loon 3d plot widget
#'
#' @description \code{l_plot3D} is a generic function for creating interactive
#'   visualization environments for \R objects.
#'
#' @family three-dimensional plotting functions
#'
#' @param x the x, y and z arguments provide the x, y and z coordinates for the plot.
#'          Any reasonable way of defining the coordinates is acceptable.
#'          See the function xyz.coords for details.
#'
#'          If supplied separately, they must be of the same length.
#'
#' @param y the y coordinates of points in the plot,
#'          optional if x is an appropriate structure.
#' @param z the z coordinates of points in the plot,
#'          optional if x is an appropriate structure.
#' @param axisScaleFactor the amount to scale the axes at the centre of the rotation.
#'          Default is 1.
#'          All numerical values are acceptable (0 removes the axes, < 0 reverses their direction.)
#' @param ... named arguments to modify plot states.
#'
#' @details
#'
#' \ifelse{html}{
#' \out{<div style="background: #dff0d8; padding: 15px;"> To get started with loon it is recommended to read loons website
#'   which can be accessed via the <code>l_help()</code> function call. </div>
#' }}{
#' To get started with loon it is recommended to read loons website
#'   which can be accessed via the \code{l_help()} function call.
#' }
#'
#'
#'   \if{html}{ The general direct manipulation and interaction gestures are
#'   outlined in the following figures.
#'
#'   Rotating
#'
#'   Press 'R' to toggle rotation mode.
#'   When rotation mode is active, either use the below mouse gestures or arrow keys to rotate the plot.
#'
#'   \figure{gestures_rotate.png}{options: alt="Rotate gestures"}
#'
#'   The centre of the rotation can be changed by panning the plot.
#'   To reset the rotation, use the tripod icon in the plot inspector.
#'
#'   Zooming and Panning
#'
#'   \figure{gestures_zoom_pan.png}{options: alt="Zoom pan gestures"}
#'
#'   Selecting Points/Objects
#'
#'   \figure{gestures_select.png}{options: alt="Select gestures"}
#'
#'   Moving Points on the Scatterplot Display
#'
#'   \figure{gestures_move.png}{options: alt="Move gestures"}
#'
#'   }
#'
#'   NOTE: Although it is possible to programmatically add layers to an l_plot3D, these will not
#'   appear as part of the 3D plot's display. There is no provision at present to incorporate
#'   rotation of 3D geometric objects other than point glyphs.
#'
#'
#' @template return_widget_handle
#'
#' @seealso \code{\link{l_info_states}}
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' with(quakes,
#'      l_plot3D(long, lat, depth, linkingGroup = "quakes")
#' )
#'
#'
#' with(l_scale3D(quakes),
#'      l_plot3D(long, lat, depth, linkingGroup = "quakes")
#' )
#'
#' scaled_quakes <- l_scale3D(quakes)
#' with(scaled_quakes,
#'      l_plot3D(long, lat, depth, linkingGroup = "quakes")
#' )
#'
#' with(scaled_quakes,
#'      l_plot3D(mag, stations, depth, linkingGroup = "quakes")
#' )
#'
#' # Or together:
#' with(scaled_quakes,{
#'      l_plot3D(long, lat, depth, linkingGroup = "quakes")
#'      l_plot3D(mag, stations, depth, linkingGroup = "quakes")
#'      }
#' )
#'
#'}
l_plot3D <- function(x, y, z, axisScaleFactor, ...) {
    UseMethod("l_plot3D")
}


#' @title The default \code{l_plot} method to create 3d interactive scatterplot
#'
#' @description Creates an interactive 3d scatterplot. Also, if no loon
#'   inspector is open then the \code{l_plot3D} call will also open a loon
#'   inspector.
#'
#' @method l_plot3D default
#'
#' @family three-dimensional plotting functions
#'
#' @param x the x, y and z arguments provide the x, y and z coordinates for the plot.
#'          Any reasonable way of defining the coordinates is acceptable.
#'          See the function xyz.coords for details.
#'
#'          If supplied separately, they must be of the same length.
#'
#' @param y the y coordinates of points in the plot,
#'          optional if x is an appropriate structure.
#' @param z the z coordinates of points in the plot,
#'          optional if x is an appropriate structure.
#' @param axisScaleFactor the amount to scale the axes at the centre of the rotation.
#'          Default is 1.
#'              All numerical values are acceptable (0 removes the axes, < 0 inverts the direction of
#'              all axes.)
#' @param by loon plot can be separated by some variables into multiple panels.
#' This argument can take a \code{vector}, a \code{list} of same lengths or a \code{data.frame} as input.
#' @param layout layout facets as \code{'grid'}, \code{'wrap'} or \code{'separate'}
#' @param connectedScales Determines how the scales of the facets are to be connected depending
#' on which \code{layout} is used.  For each value of \code{layout}, the scales are connected
#' as follows:
#' \itemize{
#' \item{\code{layout = "wrap":}  Across all facets, when \code{connectedScales} is
#'    \itemize{
#'    \item{\code{"x"}, then  only the "x"  scales are connected}
#'    \item{\code{"y"}, then only the "y" scales are connected}
#'    \item{\code{"both"},  both "x" and "y" scales are connected}
#'    \item{\code{"none"},  neither "x" nor "y" scales are connected.}
#'    For any other value, only the "y" scale is connected.
#'    }
#'    }
#' \item{\code{layout = "grid":}  Across all facets, when \code{connectedScales} is
#'    \itemize{
#'    \item{\code{"cross"}, then only the scales in the same row and the same column are connected}
#'    \item{\code{"row"}, then both "x" and "y" scales of facets in the same row are connected}
#'    \item{\code{"column"}, then both "x" and "y" scales of facets in the same column are connected}
#'    \item{\code{"x"}, then all of the "x"  scales are connected (regardless of column)}
#'    \item{\code{"y"}, then all of the "y" scales are connected (regardless of row)}
#'    \item{\code{"both"},  both "x" and "y" scales are connected in all facets}
#'    \item{\code{"none"},  neither "x" nor "y" scales are connected in any facets.}
#'    }
#'    }
#'  }
#' @param color colours of points; colours are repeated until matching the number points.
#'     Default is given by \code{\link{l_getOption}("color")}.
#' @param glyph shape of point; must be one of the primitive glyphs
#'              "circle", "ccircle", "ocircle", "square", "csquare", "osquare", "triangle", "ctriangle",
#'              "otriangle", "diamond", "cdiamond", or "odiamond".
#'
#'              Prefixes "c" and "o" mean closed and open, respectively.
#'              Default is given by \code{\link{l_getOption}("glyph")}.
#'
#'              Non-primitive glyphs such as polygons, images, text, point ranges, and even interactive glyphs like
#'              serial axes glyphs may be added, but only after the plot has been created.
#'
#' @param size size of the symbol (roughly in terms of area).
#'     Default is given by \code{\link{l_getOption}("size")}.
#' @param active a logical determining whether points appear or not
#' (default is \code{TRUE} for all points). If a logical vector is given of length
#' equal to the number of points, then it identifies which points appear (\code{TRUE})
#' and which do not (\code{FALSE}).
#' @param selected a logical determining whether points appear selected at first
#' (default is \code{FALSE} for all points). If a logical vector is given of length
#' equal to the number of points, then it identifies which points are (\code{TRUE})
#' and which are not (\code{FALSE}).
#' @param xlabel Label for the horizontal (x) axis. If missing,
#'               one will be inferred from \code{x} if possible.
#' @param ylabel Label for the vertical (y) axis. If missing,
#'               one will be inferred from \code{y} (or \code{x}) if possible.
#' @param zlabel Label for the third (perpendicular to the screen) (z) axis. If missing,
#'               one will be inferred from \code{z} (or \code{x}) if possible.
#' @param title Title for the plot, default is an empty string.
#' @param showLabels logical to determine whether axes label (and title) should be presented.
#' @param showScales logical to determine whether numerical scales should
#'               be presented on both axes.
#' @param showGuides logical to determine whether to present background guidelines
#'               to help determine locations.
#' @param guidelines colour of the guidelines shown when \code{showGuides = TRUE}.
#'     Default is given by \code{\link{l_getOption}("guidelines")}.
#' @param guidesBackground  colour of the background to the guidelines shown when
#'               \code{showGuides = TRUE}.
#'     Default is given by \code{\link{l_getOption}("guidesBackground")}.
#' @param foreground foreground colour used by all other drawing.
#'     Default is given by \code{\link{l_getOption}("foreground")}.
#' @param background background colour used for the plot.
#'     Default is given by \code{\link{l_getOption}("background")}.
#' @param parent a valid Tk parent widget path. When the parent widget is
#'   specified (i.e. not \code{NULL}) then the plot widget needs to be placed using
#'   some geometry manager like \code{\link{tkpack}} or \code{\link{tkplace}} in
#'   order to be displayed. See the examples below.
#' @param ... named arguments to modify plot states.
#'
#'
#' @details The scatterplot displays a number of direct interactions with the
#'   mouse and keyboard, these include: rotating, zooming towards the mouse cursor using
#'   the mouse wheel, panning by right-click dragging and various selection
#'   methods using the left mouse button such as sweeping, brushing and
#'   individual point selection. See the documentation for \code{\link{l_plot3D}}
#'   for more details about the interaction gestures.
#'
#' @export
#' @export l_plot3D.default
#'
#' @examples
#' if(interactive()){
#'
#' # default use as scatterplot
#'
#' p1 <- with(quakes,
#'            l_plot3D(long, lat, depth)
#'            )
#'
#' p2 <- with(quakes,
#'            l_plot3D(mag, stations, depth)
#'            )
#'
#' # link the two plots p1 and p2
#' l_configure(p1, linkingGroup = "quakes", sync = "push")
#' l_configure(p2, linkingGroup = "quakes", sync = "push")
#'
#'}
l_plot3D.default <-  function(x,  y = NULL, z = NULL,
                              axisScaleFactor = 1,
                              by = NULL,
                              layout = c("grid", "wrap", "separate"),
                              connectedScales = c("cross", "row", "column", "both", "x", "y", "none"),
                              color = l_getOption("color"),
                              glyph = l_getOption("glyph"),
                              size = l_getOption("size"),
                              active = TRUE,
                              selected = FALSE,
                              xlabel, ylabel, zlabel,
                              title,
                              showLabels = TRUE,
                              showScales = FALSE,
                              showGuides = TRUE,
                              guidelines = l_getOption("guidelines"),
                              guidesBackground = l_getOption("guidesBackground"),
                              foreground = l_getOption("foreground"),
                              background = l_getOption("background"),
                              parent = NULL, ...) {

    args <- list(...)
    # set by args, used for facetting
    by_args <- args[l_byArgs()]
    # args passed into loonPlotFactory
    args[l_byArgs()] <- NULL

    if (missing(title)) { title <- "" }

    if(missing(x)) {

        ## Get x, y, z, xlab, ylab, zlab
        ## similar as in plot.default use xyz.coords
        if (missing(xlabel)){
            xlabel <- ""
        }

        if (missing(ylabel)) {
            ylabel <- ""
        }

        if (missing(zlabel)) {
            zlabel <- ""
        }

        plot <- do.call(
            loonPlotFactory,
            c(
                args,
                list(
                    factory_tclcmd = '::loon::plot3D',
                    factory_path = 'plot3D',
                    factory_window_title = 'loon scatterplot3D',
                    parent = parent,
                    axisScaleFactor = axisScaleFactor,
                    xlabel = xlabel,
                    ylabel = ylabel,
                    zlabel = zlabel,
                    title = title,
                    showLabels = showLabels,
                    showScales = showScales,
                    showGuides = showGuides,
                    guidelines = guidelines,
                    guidesBackground = guidesBackground,
                    foreground = foreground,
                    background = background
                )
            )
        )

        class(plot) <- c("l_plot3D", "l_plot", class(plot))
        return(plot)

    } else {

        xOrigin <- x
        xlab <- deparse(substitute(x))
        ylab <- deparse(substitute(y))
        zlab <- deparse(substitute(z))

        xyz <- xyz.coords(x, y, z)
        x <- xyz$x
        y <- xyz$y
        z <- xyz$z

        ## Get x, y, z, xlab, ylab, zlab
        ## similar as in plot.default use xyz.coords
        if (missing(xlabel)){
            xlabel <- if (is.null(xyz$xlab)) xlab else xyz$xlab
        }

        if (missing(ylabel)) {
            ylabel <- if (is.null(xyz$ylab)) ylab else xyz$ylab
        }

        if (missing(zlabel)) {
            zlabel <- if (is.null(xyz$zlab)) zlab else xyz$zlab
        }

        sync <- args$sync

        if(is.null(sync)) {
            sync <- "pull"
            if(length(color) > 1) {
                sync <- "push"
            } else {
                if(length(color) == 1 && !is.na(color) && color != "grey60") sync <- "push"
            }

            if(length(size) != 1) {
                sync <- "push"
            } else {
                if(length(size) == 1 && !is.na(size) && size != 4) sync <- "push"
            }

            if(length(glyph) != 1) {
                sync <- "push"
            } else {
                if(length(glyph) == 1 && !is.na(glyph) && glyph != "ccircle") sync <- "push"
            }
        }

        n <- length(x)
        len_color <- length(color)
        if (len_color > 1) {
            if (len_color != n) {
                color <- rep_len(color, n)
            }
        } else {
            if(is.na(color)) color <- "grey60"
        }

        len_size <- length(size)
        if (len_size > 1) {
            if (len_size != n) {
                size <- rep_len(size, n)
            }
        } else {
            if(is.na(size)) size <- 4
        }

        len_active <- length(active)
        if (len_active > 1) {
            if (len_active != n)
                stop(paste0("When more than length 1, length of active must match number of points:",
                            n)
                )
        } else {
            if(is.na(active)) active <- TRUE
        }

        len_selected <- length(selected)
        if (len_selected > 1) {
            if (len_selected != n)
                stop(paste0("When more than length 1, length of selected must match number of points:",
                            n)
                )
        } else {
            if(is.na(selected)) selected <- FALSE
        }

        len_glyph <- length(glyph)
        if (len_glyph > 1) {
            if (len_glyph != n)
                stop(paste0("When more than length 1, length of glyph must match number of points:",
                            n)
                )
        } else {
            if(is.na(glyph)) glyph <- "ccircle"
        }

        linkingGroup <- args[["linkingGroup"]]
        args$linkingGroup <- NULL
        # n dimensional states NA check
        args$x <- x
        args$y <- y
        args$z <- z
        args$color <- color
        args$glyph <- glyph
        args$size <- size
        args$active <- active
        args$selected <- selected

        if(is.null(by)) {
            args <- l_na_omit("l_plot3D", args)

            plot <- do.call(
                loonPlotFactory,
                c(
                    args,
                    list(
                        factory_tclcmd = '::loon::plot3D',
                        factory_path = 'plot3D',
                        factory_window_title = 'loon scatterplot3D',
                        parent = parent,
                        xlabel = xlabel,
                        ylabel = ylabel,
                        zlabel = zlabel,
                        title = title,
                        showLabels = showLabels,
                        showScales = showScales,
                        showGuides = showGuides,
                        guidelines = guidelines,
                        guidesBackground = guidesBackground,
                        foreground = foreground,
                        background = background
                    )
                )
            )

            if(!is.null(linkingGroup)) {
                l_configure(plot,
                            linkingGroup = linkingGroup,
                            sync = sync)
            }

            class(plot) <- c("l_plot3D", "l_plot", class(plot))
            return(plot)

        } else {

            # convert all types of 'by' to a data frame
            byDeparse <- deparse(substitute(by))

            if(is.atomic(by)) {
                if(length(by) == n) {
                    by <- setNames(data.frame(by, stringsAsFactors = FALSE), byDeparse)
                } else {
                    # 'by' is a char vector
                    # 'x' should be a data.frame
                    if(!is.data.frame(xOrigin))
                        stop("If 'by' are variable names, ",
                             xlab,
                             " should be a data frame")
                    by <- xOrigin[by]
                }
            } else {

                if(is.null(names(by))) {

                    by <- as.data.frame(by, stringsAsFactors = FALSE)
                    names(by) <- NULL
                } else {
                    by <- as.data.frame(by, stringsAsFactors = FALSE)
                }
            }

            plots <- loonFacets(type = c("l_plot3D", "l_plot"),
                                by,
                                args,
                                byDeparse = byDeparse,
                                layout = match.arg(layout),
                                connectedScales = match.arg(connectedScales),
                                by_args = Filter(Negate(is.null), by_args),
                                factory_tclcmd = '::loon::plot3D',
                                factory_path = 'plot3D',
                                factory_window_title = 'loon scatterplot3D',
                                linkingGroup = linkingGroup,
                                sync = sync,
                                parent = parent,
                                xlabel = xlabel,
                                ylabel = ylabel,
                                title = title,
                                showLabels = showLabels,
                                showScales = showScales,
                                showGuides = showGuides,
                                guidelines = guidelines,
                                guidesBackground = guidesBackground,
                                foreground = foreground,
                                background = background)

            return(plots)
        }
    }
}



