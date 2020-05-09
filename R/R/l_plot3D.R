#' @title Scale for 3d plotting
#'
#' @description \code{l_scale3D} scales its argument in a variety of ways used for 3D visualization.
#' @family three-dimensional plotting functions
#' @param x the matrix or data.frame whose columns are to be scaled.
#' @param center either a logical value or numeric-alike vector of length equal
#' to the number of columns of x, where ‘numeric-alike’ means that as.numeric(.)
#' will be applied successfully if is.numeric(.) is not true.
#' @param method the scaling method to use: if "box" (the default) then the columns are scaled to
#' have equal ranges; if "sphere" then x is centered, scaled to equal standard deviation and then
#' decomposed via a singular value decomposition so that the resulting variables are uncorrelated.
#'
#' @seealso \code{\link{l_plot3D}} \code{\link{scale}}
#'
#' @return a data.frame whose columns are centred and scaled according to the given arguments.
#'
#' @examples
#'
#' ##### Iris
#' with(l_scale3D(iris[,1:4]),
#'      l_plot3D(Petal.Length, Petal.Width, Sepal.Length, linkingGroup = "iris"))
#'
#' with(l_scale3D(iris[,1:4], method = "sphere"),
#'      l_plot3D(pc1, pc2, pc3, linkingGroup = "iris"))
#'
#' # With the Species as a factor
#'
#' with(l_scale3D(iris),
#'      l_plot3D(Petal.Length, Petal.Width, Sepal.Length, linkingGroup = "iris"))
#'
#' with(l_scale3D(iris, method = "sphere"),
#'      l_plot3D(pc1, pc2, pc3, linkingGroup = "iris"))
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
        ranges <- apply(x, 2, FUN = function(x) diff(range(x)))
        scaled_x <- as.data.frame(scale(x, center = center, scale = ranges))
        result <- scaled_x
    } else {
        if (method == "sphere") {
            scaled_x <- as.data.frame(scale(x, center = center, scale = TRUE))
            sphered_x <- as.data.frame(svd(scaled_x)$u)
            names(sphered_x) <- paste0("pc", 1:ncol(scaled_x))
            result <- sphered_x
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
#'              All numerical values are acceptable (0 removes the axes, < 0 reverses their direction.)
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
#'           l_plot3D(long, lat, depth, linkingGroup = "quakes")
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
#'
#' # Get an R (grid) graphics plot of a loon plot
#' p <- with(scaled_quakes,
#'           l_plot3D(long, lat, depth, linkingGroup = "quakes"))
#' plot(p)
#' # or with more control about grid parameters
#' grid.loon(p)
#' # or to save the grid data structure (grob) for later use
#' pg <- loonGrob(p)
#'
#' # Use with other tk widgets
#' tt <- tktoplevel()
#' p1 <- l_plot3D(parent=tt, x=c(1,2,3), y=c(3,2,1), z=c(1,2,3))
#' p2 <- l_plot3D(parent=tt, x=c(4,3,1), y=c(6,8,4), z=c(3,2,1))
#'
#' tkgrid(p1, row=0, column=0, sticky="nesw")
#' tkgrid(p2, row=0, column=1, sticky="nesw")
#' tkgrid.columnconfigure(tt, 0, weight=1)
#' tkgrid.columnconfigure(tt, 1, weight=1)
#' tkgrid.rowconfigure(tt, 0, weight=1)
#'
#' tktitle(tt) <- "Loon plots with custom layout"
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
#' @param by loon plot can be separated by some variables into mutiple panels.
#' This argument can take a \code{vector}, a \code{list} of same lengths or a \code{data.frame} as input.
#' @param layout layouts in a \code{'grid'} or a \code{'wrap'}
#' @param color colours of points; colours are repeated until matching the number points.
#' @param glyph shape of point; must be one of the primitive glyphs
#'              "circle", "ccircle", "ocircle", "square", "csquare", "osquare", "triangle", "ctriangle",
#'              "otriangle", "diamond", "cdiamond", or "odiamond".
#'
#'              Prefixes "c" and "o" mean closed and open, respectively.
#'              Default is "ccircle" meaning a closed circle glyph.
#'
#'              Non-primitive glyphs such as polygons, images, text, point ranges, and even interactive glyphs like
#'              serial axes glyphs may be added, but only after the plot has been created.
#' @param size size of the symbol (roughly in terms of area)
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
#' @param guidelines colour of the guidelines shown when \code{showGuides = TRUE} (default "white").
#' @param guidesBackground  colour of the background to the guidelines shown when
#'               \code{showGuides = TRUE} (default "grey92").
#' @param foreground foreground colour used by all other drawing (default "black").
#' @param background background colour used for the plot (default "white")
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
l_plot3D.default <-  function(x,  y = NULL, z = NULL,
                              axisScaleFactor = 1,
                              by = NULL,
                              layout = c("grid", "wrap"),
                              color = "grey60",
                              glyph = "ccircle",
                              size = 4,
                              active = TRUE,
                              selected = FALSE,
                              xlabel, ylabel, zlabel,
                              title,
                              showLabels = TRUE,
                              showScales = FALSE,
                              showGuides = TRUE,
                              guidelines = "white",
                              guidesBackground = "grey92",
                              foreground = "black",
                              background = "white",
                              parent = NULL, ...) {

    args <- list(...)
    # set by args, used for facetting
    by_args <- args[byArgs]
    # args passed into loonPlotFactory
    args[byArgs] <- NULL

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

            if(is.atomic(by)) {
                by <- setNames(data.frame(by, stringsAsFactors = FALSE), deparse(substitute(by)))
            } else
                by <- as.data.frame(by, stringsAsFactors = FALSE)

            plots <- loonLayouts(type = c("l_plot3D", "l_plot"),
                                 by,
                                 args,
                                 layout = match.arg(layout),
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
                                 showLabels = TRUE,
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



