
#' @title Create an interactive loon plot widget
#'
#' @description \code{l_plot} is a generic function for creating interactive
#'   visualization environments for \R objects.
#'
#' @family two-dimensional plotting functions
#' @family loon interactive states
#'
#' @param x	the coordinates of points in the \code{\link{l_plot}}.
#' Alternatively, a single plotting structure, function, or any R object having an \code{\link{l_plot}}
#' method can be provided.
#' @param y	the y coordinates of points in the \code{\link{l_plot}}, optional if x is an
#' appropriate structure.
#' @param ... named arguments to modify plot states. See \code{\link{l_info_states}}
#' of any instantiated \code{l_plot} for examples of names and values.
#'
#' @details  Like \code{\link{plot}} in \R, \code{\link{l_plot}} is
#' the generic plotting function for objects in loon.
#' The default method \code{\link{l_plot.default}} produces the interactive
#' scatterplot in loon.
#' This is the workhorse of `loon` and is often a key part of many
#' other displays (e.g. \code{\link{l_pairs}} and \code{\link{l_navgraph}}).
#'
#' For example, the  methods include \code{\link{l_plot.default}} (the basic interactive scatterplot),
#' \code{\link{l_plot.density}} (layers output of \code{\link{density}} in an empty scatterplot),
#'  \code{\link{l_plot.map}}  (layers a map in an empty scatterplot), and
#' \code{\link{l_plot.stl}} (a compound display of the output of \code{\link{stl}}).
#'
#'
#' A complete list is had from \code{methods(l_plot)}.
#'
#' \ifelse{html}{
#' \out{<div style="background: #dff0d8; padding: 15px;"> To get started with loon
#'   it is recommended to follow the introductory loon vignette <code>vignette(topic = "introduction", package  = "loon")</code>
#'   and to explore loon's website accessible via <code>l_help()</code>. </div>
#' }}{
#' To get started with loon it is recommended to follow the introductory loon vignette
#' \code{vignette(topic = "introduction", package  = "loon")}
#'   and to explore loon's website accessible via \code{l_help()}.
#' }
#'
#'
#'   \if{html}{ The general direct manipulation and interaction gestures are
#'   outlined in the following figures.
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
#'
#' @template return_widget_handle
#'
#' @seealso  \code{\link{l_plot_arguments}} and \code{\link{l_plot.default}}.
#'
#'
#' @export
#'
#' @examples
#' # ordinary use
#' p <- with(iris, l_plot(Sepal.Width, Petal.Length, color=Species))
#'
#' versi <- iris$Species == "versicolor"
#' p["glyph"][versi] <- "ctriangle"
#'
#' # Get an R (grid) graphics plot of the current loon plot
#' plot(p)
#' # or with more control about grid parameters
#' grid.loon(p)
#' # or to save the grid data structure (grob) for later use
#' pg <- loonGrob(p)
#'
#' # plot a density estimate
#' set.seed(314159)
#' ds <- density(rnorm(1000))
#' p <- l_plot(ds,  title = "density estimate",
#'             xlabel = "x", ylabel = "density",
#'             showScales = TRUE)
#' plot(p)
#'
#'
l_plot <- function(x, y, ...) {
    UseMethod("l_plot")
}


#' @title The default \code{l_plot} for plotting data
#'
#' @description Creates an interactive 2d scatterplot. Also, if no loon
#'   inspector is open then the \code{l_plot} call will also open a loon
#'   inspector.
#'
#' @family two-dimensional plotting functions
#' @family loon interactive states
#' @method l_plot default
#' @param x the \code{x} and \code{y} arguments provide the x and y
#'  coordinates for the plot.
#'  Any reasonable way of defining the coordinates is acceptable.
#'  See the function \code{\link{xy.coords}} for details.
#'  If supplied separately, they must be of the same length.
#' @param y argument description is as for the \code{x} argument above.
#' @param color colours of points (default "grey60"); colours are repeated
#'  until matching the number points,
#' @param glyph shape of point; must be one of the primitive glyphs
#' "circle", "ccircle", "ocircle", "square", "csquare", "osquare", "triangle", "ctriangle",
#' "otriangle", "diamond", "cdiamond", or "odiamond". Prefixes "c" and "o" mean closed and open, respectively.
#' Default is "ccircle" meaning a closed circle glyph.
#'
#' Non-primitive glyphs such as polygons, images, text, point ranges, and even interactive glyphs like
#' serial axes glyphs may be added, but only after the plot has been created.
#' @param size size of the symbol (roughly in terms of area)
#' @param active a logical determining whether points appear or not
#' (default is TRUE for all points). If a logical vector is given of length
#' equal to the number of points, then it identifies which points appear (TRUE)
#' and which do not (FALSE).
#' @param selected a logical determining whether points appear selected at first
#' (default is FALSE for all points). If a logical vector is given of length
#' equal to the number of points, then it identifies which points are (TRUE)
#' and which are not (FALSE).
#' @param xlabel Label for the horizontal (x) axis. If missing,
#' one will be inferred from \code{x} if possible.
#' @param ylabel Label for the vertical (y) axis. If missing,
#' one will be inferred from \code{y} (or \code{x}) if possible.
#' @param title Title for the plot, default is an empty string.
#' @param showLabels logical to determine whether axes label (and title) should
#' be presented.
#' @param showScales logical to determine whether numerical scales should
#' be presented on both axes.
#' @param showGuides logical to determine whether to present background guidelines
#' to help determine locations.
#' @param guidelines colour of the guidelines shown when \code{showGuides = TRUE} (default "white").
#' @param guidesBackground  colour of the background to the guidelines shown when
#' \code{showGuides = TRUE} (default "grey92").
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
#'   mouse and keyboard, these include: zooming towards the mouse cursor using
#'   the mouse wheel, panning by right-click dragging and various selection
#'   methods using the left mouse button such as sweeping, brushing and
#'   individual point selection. See the documentation for \code{\link{l_plot}}
#'   for more details about the interaction gestures.
#'
#' @seealso  \code{\link{l_plot_arguments}}
#' @export
#' @export l_plot.default
#'
#' @examples
#'
#' # default use as scatterplot
#'
#' p1 <- with(iris, l_plot(Sepal.Length, Sepal.Width, color=Species,
#'                         title = "First plot"))
#'
#' # The names of the info states that can be
#' # accessed or set.  They can also be given values as
#' # arguments to l_plot.default()
#' names(p1)
#' p1["size"] <- 10
#'
#' p2 <- with(iris, l_plot(Petal.Length ~ Petal.Width,
#'                         linkingGroup="iris_data",
#'                         title = "Second plot",
#'                         showGuides = FALSE))
#' p2["showScales"] <- TRUE
#'
#' # link first plot with the second plot requires
#' # l_configure to coordinate the synchroniztion
#' l_configure(p1, linkingGroup = "iris_data", sync = "push")
#'
#' p1['selected'] <- iris$Species == "versicolor"
#' p2["glyph"][p1['selected']] <- "cdiamond"
#'
#' gridExtra::grid.arrange(loonGrob(p1), loonGrob(p2), nrow = 1)
#'
#'
#' # Use with other tk widgets
#' tt <- tktoplevel()
#' tktitle(tt) <- "Loon plots with custom layout"
#'
#' p1 <- l_plot(parent=tt, x=c(1,2,3), y=c(3,2,1))
#' p2 <- l_plot(parent=tt, x=c(4,3,1), y=c(6,8,4))
#'
#' tkgrid(p1, row=0, column=0, sticky="nesw")
#' tkgrid(p2, row=0, column=1, sticky="nesw")
#'
#' tkgrid.columnconfigure(tt, 0, weight=1)
#' tkgrid.columnconfigure(tt, 1, weight=1)
#'
#' tkgrid.rowconfigure(tt, 0, weight=1)
#'
l_plot.default <-  function(x, y = NULL,
                            color = "grey60",
                            glyph = "ccircle",
                            size = 4,
                            active = TRUE,
                            selected = FALSE,
                            xlabel, ylabel, title,
                            showLabels = TRUE,
                            showScales = FALSE,
                            showGuides = TRUE,
                            guidelines = "white",
                            guidesBackground = "grey92",
                            foreground = "black",
                            background = "white",
                            parent = NULL, ...) {

    ## Check for missing arguments
    ## Get x, y, xlab and ylab
    if (missing(xlabel)){
        if (!missing(x)){
            xlabel <- gsub("\"", "", deparse(substitute(x)))
        } else {
            xlabel <- ""
        }
    }

    if (missing(ylabel)) {
        if (!missing(y)){
            ylabel <- gsub("\"", "", deparse(substitute(y)))
        } else {
            ylabel <- ""
        }
    }
    if (missing(title)) { title <- "" }


    if(missing(x)) {

        plot <- loonPlotFactory('::loon::plot', 'plot',
                                'loon scatterplot',
                                parent,
                                # No info about points
                                # to be passed on
                                xlabel = xlabel,
                                ylabel = ylabel,
                                title = title,
                                showLabels = showLabels,
                                showScales = showScales,
                                showGuides = showGuides,
                                guidelines = guidelines,
                                guidesBackground = guidesBackground,
                                foreground = foreground,
                                background = background,
                                ...)

    } else {
        ## Get x, y, xlab, ylab
        ## similar to plot.default use of xy.coords
        xy <- xy.coords(x, y, xlabel, ylabel)
        x <- xy$x
        y <- xy$y
        if (is.null(xy$xlab)) xy$xlab <- ""
        if (is.null(xy$ylab)) xy$ylab <- ""
        ## make sure points parameters are right
        if (length(color) > 1) {
            if (length(color) != length(x)) {
                color <- rep_len(color, length(x))
            }
        }
        if (!is.numeric(size)) stop("size must be numeric")
        if (length(size) > 1) {
            if (length(size) != length(x)) {
                size <- rep_len(size, length(x))
            }
        }
        if (length(active) > 1) {
            if (length(active) != length(x))
                stop(paste0("When more than length 1, length of active must match number of points:",
                            length(x))
                )
        }
        if (length(selected) > 1) {
            if (length(selected) != length(x))
                stop(paste0("When more than length 1, length of selected must match number of points:",
                            length(x))
                )
        }
        if (length(glyph) > 1) {
            if (length(glyph) != length(x))
                stop(paste0("When more than length 1, length of glyph must match number of points:",
                            length(x))
                )
        }
        plot <- loonPlotFactory('::loon::plot', 'plot', 'loon scatterplot',
                                parent,
                                x = x, y = y,
                                color = color,
                                glyph = glyph,
                                size = size,
                                active = active,
                                selected = selected,
                                xlabel = xy$xlab,
                                ylabel = xy$ylab,
                                title = title,
                                showLabels = showLabels,
                                showScales = showScales,
                                showGuides = showGuides,
                                guidelines = guidelines,
                                guidesBackground = guidesBackground,
                                foreground = foreground,
                                background = background,
                                ...)

    }

    class(plot) <- c("l_plot", class(plot))
    return(plot)
}



