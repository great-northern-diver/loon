#' @title Scale for 3d plotting
#'
#' @description \code{l_scale3D} scales its argument in a variety of ways used for 3D visualization.
#'
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
#' @inheritParams graphics::plot
#' @param z the z coordinates of points in the plot, optional if x is an appropriate structure.
#' @param ... named arguments to modify plot states
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
l_plot3D <- function(x, y, z, ...) {
    UseMethod("l_plot3D")
}


#' @title Create an interactive 3d scatterplot display
#'
#' @description Creates an interactive 3d scatterplot. Also, if no loon
#'   inspector is open then the \code{l_plot3D} call will also open a loon
#'   inspector.
#'
#'
#' @param x the x, y and z arguments provide the x, y and z coordinates for the plot. Any reasonable way of defining the coordinates is acceptable. See the function xyz.coords for details. If supplied separately, they must be of the same length.
#' @param y please read in the argument description for the \code{x} argument above.
#' @param z please read in the argument description for the \code{x} argument above.
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
l_plot3D.default <-  function(x, y=NULL, z=NULL, parent=NULL, ...) {

    if(missing(x)) {

        plot <- loonPlotFactory('::loon::plot3D', 'plot3D', 'loon scatterplot3D', parent, ...)

    } else {

        ## Get x, y, z, xlab, ylab, zlab
        ## similar as in plot.default use xyz.coords
        xlabel <- if (!missing(x))
            gsub("\"", "", deparse(substitute(x)))
        ylabel <- if (!missing(y))
            gsub("\"", "", deparse(substitute(y)))
        zlabel <- if (!missing(z))
            gsub("\"", "", deparse(substitute(z)))

        xyz <- xyz.coords(x, y, z, xlabel, ylabel, zlabel)

        if (is.null(xyz$xlab))
            xyz$xlab <- ""
        if (is.null(xyz$ylab))
            xyz$ylab <- ""
        if (is.null(xyz$zlab))
            xyz$zlab <- ""

        plot <- loonPlotFactory('::loon::plot3D', 'plot3D', 'loon scatterplot3D', parent,
                                x=xyz$x, y=xyz$y, z=xyz$z,
                                xlabel=xyz$xlab, ylabel=xyz$ylab, zlabel=xyz$zlab,
                                ...)

    }

    class(plot) <- c("l_plot3D", "l_plot", class(plot))
    return(plot)
}



