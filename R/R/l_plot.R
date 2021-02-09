
#' @title Create an interactive loon plot widget
#' @name l_plot
#' @description \code{l_plot} is a generic function for creating an interactive
#'   visualization environments for R objects.
#'
#' @family two-dimensional plotting functions
#' @family loon interactive states
#'
#' @param x the coordinates of points in the \code{\link{l_plot}}.
#' Alternatively, a single plotting structure (see the function \code{\link{xy.coords}} for details),
#' \code{\link{formula}}, or any \R object (e.g. \code{\link{density}},\code{\link{stl}}, etc)
#' is accommodated.
#' @param y the y coordinates of points in the \code{\link{l_plot}}, optional if x is an
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
#' For example, the  methods include \code{l_plot.default} (the basic interactive scatterplot),
#' \code{l_plot.density} (layers output of \code{\link{density}} in an empty scatterplot),
#'  \code{l_plot.map} (layers a map in an empty scatterplot), and
#' \code{l_plot.stl} (a compound display of the output of \code{\link{stl}}).
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
#' @return \itemize{
#' \item{
#' The input is a \code{stl} or a \code{decomposed.ts} object,
#' a structure of class \code{"l_ts"} containing four loon plots
#' each representing a part of the decomposition
#' by name: "original", "trend", "seasonal", and "remainder"
#' }
#' \item{
#' The input is a vector, formula, data.frame, ...
#' \itemize{
#' \item{\code{by = NULL}: a \code{loon} widget will be returned}
#' \item{\code{by} is not \code{NULL}: an \code{l_facet} object (a list) will be returned and
#' each element is a \code{loon} widget displaying a subset of interest.}
#' }
#' }
#' }
#'
#'
#'
#' @seealso Turn interactive loon plot static \code{\link{loonGrob}}, \code{\link{grid.loon}}, \code{\link{plot.loon}}.
#'
#'
#' @export
#' @examples
#' if(interactive()) {
#' ########################## l_plot.default ##########################
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
#' # Layout facets
#' ### facet wrap
#' p3 <- with(mtcars, l_plot(wt, mpg, by = cyl, layout = "wrap"))
#' # it is equivalent to
#' # p3 <- l_plot(mpg~wt, by = ~cyl, layout = "wrap", on = mtcars)
#'
#' ### facet grid
#' p4 <- l_plot(x = 1:6, y = 1:6,
#'              by = size ~ color,
#'              size = c(rep(50, 2), rep(25, 2), rep(50, 2)),
#'              color = c(rep("red", 3), rep("green", 3)))
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
#' ########################## l_plot.decomposed.ts ##########################
#' decompose <- decompose(co2)
#' p <- l_plot(decompose, title = "Atmospheric carbon dioxide over Mauna Loa")
#' # names of plots in the display
#' names(p)
#' # names of states associated with the seasonality plot
#' names(p$seasonal)
#' # which can be set
#' p$seasonal['color'] <- "steelblue"
#'
#' ########################## l_plot.stl ##########################
#' co2_stl <- stl(co2, "per")
#' p <- l_plot(co2_stl, title = "Atmospheric carbon dioxide over Mauna Loa")
#' # names of plots in the display
#' names(p)
#' # names of states associated with the seasonality plot
#' names(p$seasonal)
#' # which can be set
#' p$seasonal['color'] <- "steelblue"
#' ########################## l_plot.density ##########################
#' # plot a density estimate
#' set.seed(314159)
#' ds <- density(rnorm(1000))
#' p <- l_plot(ds,  title = "density estimate",
#'             xlabel = "x", ylabel = "density",
#'             showScales = TRUE)
#'
#' ########################## l_plot.map ##########################
#' if (requireNamespace("maps", quietly = TRUE)) {
#'    p <- l_plot(maps::map('world', fill=TRUE, plot=FALSE))
#' }
#'}
#'
l_plot <- function(x, y, ...) {
    UseMethod("l_plot")
}


#' @title The default \code{l_plot} for plotting data
#' @rdname l_plot
#'
#' @method l_plot default
#' @template param_by
#' @template param_on
#' @template param_layout
#' @template param_connectedScales
#' @template param_pointcolor
#' @template param_glyph
#' @template param_pointsize
#' @template param_active
#' @template param_selected
#' @template param_xlabel
#' @template param_ylabel
#' @template param_title
#' @template param_showLabels
#' @template param_showScales
#' @template param_showGuides
#' @template param_guidelines
#' @template param_guidesBackground
#' @template param_foreground
#' @template param_background
#' @template param_parent
#'
#' @details
#' The scatterplot displays a number of direct interactions with the
#' mouse and keyboard, these include: zooming towards the mouse cursor using
#' the mouse wheel, panning by right-click dragging and various selection
#' methods using the left mouse button such as sweeping, brushing and
#' individual point selection. See the documentation for \code{\link{l_plot}}
#' for more details about the interaction gestures.
#'
#'
#' Some arguments to modify layouts can be passed through,
#' e.g. "separate", "ncol", "nrow", etc. Check \code{\link{l_facet}}
#' to see how these arguments work.
#'
#' @export
l_plot.default <-  function(x, y = NULL,
                            by = NULL,
                            on,
                            layout = c("grid", "wrap", "separate"),
                            connectedScales = c("cross", "row", "column", "both", "x", "y", "none"),
                            color = l_getOption("color"),
                            glyph = l_getOption("glyph"),
                            size = l_getOption("size"),
                            active = TRUE,
                            selected = FALSE,
                            xlabel, ylabel, title,
                            showLabels = TRUE,
                            showScales = FALSE,
                            showGuides = TRUE,
                            guidelines = l_getOption("guidelines"),
                            guidesBackground = l_getOption("guidesBackground"),
                            foreground = l_getOption("foreground"),
                            background = l_getOption("background"),
                            parent = NULL,
                            ...) {

    dotArgs <- list(...)
    # set by args, used for facetting
    byArgs <- dotArgs[l_byArgs()]
    # args passed into loonPlotFactory
    dotArgs[l_byArgs()] <- NULL

    l_className <- "l_plot"

    if(missing(title)) { title <- "" }

    if(missing(x)) {

        ## Check for missing arguments
        ## Get x, y, xlab and ylab
        if (missing(xlabel)){
            xlabel <- ""
        }

        if (missing(ylabel)) {
            ylabel <- ""
        }
        # Should we check `by` var here?
        plot <- do.call(
            loonPlotFactory,
            c(
                dotArgs,
                list(
                    factory_tclcmd = '::loon::plot',
                    factory_path = 'plot',
                    factory_window_title = 'loon scatterplot',
                    parent = parent,
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
                    background = background
                )
            )
        )

        class(plot) <- c(l_className, class(plot))
        return(plot)

    } else {

        ## Get x, y, xlab, ylab
        ## similar to plot.default use of xy.coords
        xlab <- deparse(substitute(x))
        ylab <- deparse(substitute(y))
        xOrigin <- x

        if(is.atomic(x)) {
            if(is.character(x)) x <- as.factor(x)
            if(!is.null(y) && is.character(y)) y <- as.factor(y)
        }
        if(inherits(x, "formula") && !missing(on)) {

            x <- model.frame(x, data = on)
            xy <- list()
            colnames <- colnames(x)
            xy$y <- x[[1L]]
            xy$ylab <- colnames[1L]

            if(ncol(x) > 1L) {
                # the first column is y
                # the second column is x
                # reverse x and y
                xy$x <- x[[2L]]
                xy$xlab <- colnames[2L]
            } else {
                xy$x <- seq(length(x[[1L]]))
                xy$xlab <- "index"
            }
        } else {
            xy <- xy.coords(x, y)
        }


        x <- xy$x
        y <- xy$y

        if (missing(xlabel))
            xlabel <- if (is.null(xy$xlab)) xlab else xy$xlab

        if (missing(ylabel))
            ylabel <- if (is.null(xy$ylab)) ylab else xy$ylab

        ## make sure points parameters are right
        n <- length(x)

        # check which states are modified
        call <- match.call()
        modifiedLinkedStates <- l_modifiedLinkedStates(l_className, names(call))

        color <- aes_settings(color, n, ifNoStop = FALSE)
        size <- aes_settings(size, n, ifNoStop = FALSE)
        glyph <- aes_settings(glyph, n, ifNoStop = FALSE)
        active <- aes_settings(active, n, ifNoStop = TRUE)
        selected <- aes_settings(selected, n, ifNoStop = TRUE)

        # `sync` and `linkingGroup` are set after the plot is created
        # reason: set aesthetics first, then pull aesthetics from other plots (if they exist)
        linkingGroup <- dotArgs[["linkingGroup"]]
        dotArgs$linkingGroup <- NULL
        sync <- dotArgs[["sync"]]
        # if null, it is always **pull**
        if(is.null(sync)) sync <- "pull"
        dotArgs$sync <- NULL

        # n dimensional states NA check
        dotArgs$x <- x
        dotArgs$y <- y
        dotArgs$color <- color
        dotArgs$glyph <- glyph
        dotArgs$size <- size
        dotArgs$active <- active
        dotArgs$selected <- selected

        if(is.null(by)) {
            dotArgs <- l_na_omit(paste0(c(l_className, "default"), collapse = "."), dotArgs)

            plot <- do.call(
                loonPlotFactory,
                c(
                    dotArgs,
                    list(factory_tclcmd = '::loon::plot',
                         factory_path = 'plot',
                         factory_window_title = 'loon scatterplot',
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
                )
            )

            if(!is.null(linkingGroup)) {

                syncTemp <- ifelse(length(modifiedLinkedStates) == 0,  sync, "pull")
                if(syncTemp == "push")
                    message("The modification of linked states is not detected",
                            " so that the default settings will be pushed to all plots")
                # configure plot (linking)
                l_configure(plot,
                            linkingGroup = linkingGroup,
                            sync = syncTemp)

                if(sync == "push" && length(modifiedLinkedStates) > 0) {

                    do.call(l_configure,
                            c(
                                list(
                                    target = plot,
                                    linkingGroup = linkingGroup,
                                    sync = sync
                                ),
                                dotArgs[modifiedLinkedStates]
                            )
                    )
                } else {
                    l_linkingWarning(plot, sync, args = dotArgs,
                                     modifiedLinkedStates = modifiedLinkedStates,
                                     l_className = l_className)
                }
            }

            class(plot) <- c(l_className, class(plot))
            return(plot)

        } else {

            plots <- loonFacets(type = l_className,
                                by = by,
                                args = dotArgs,
                                on = on,
                                bySubstitute = substitute(by), # for warning or error generations
                                layout = match.arg(layout),
                                connectedScales = match.arg(connectedScales),
                                byArgs = Filter(Negate(is.null), byArgs),
                                linkingGroup = linkingGroup,
                                sync = sync,
                                parent = parent,
                                xlabel = xlabel,
                                ylabel = ylabel,
                                title = title,
                                factory_tclcmd = '::loon::plot',
                                factory_path = 'plot',
                                factory_window_title = 'loon scatterplot',
                                modifiedLinkedStates = modifiedLinkedStates,
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

