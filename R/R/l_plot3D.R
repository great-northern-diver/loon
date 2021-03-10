#' @title Create an interactive loon 3d plot widget
#' @name l_plot3D
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
#'   The general direct manipulation and interaction gestures are
#'   outlined in the following figures.
#'
#'   Rotating
#'
#'   Press 'R' to toggle rotation mode.
#'   When rotation mode is active, either use the below mouse gestures or arrow keys to rotate the plot.
#'
#'
#'  \if{html}{\figure{gestures_rotate.png}{options: alt="Rotate gestures"}}
#'
#'
#'   The centre of the rotation can be changed by panning the plot.
#'   To reset the rotation, use the tripod icon in the plot inspector.
#'
#'
#'   Zooming and Panning
#'
#'
#'   \if{html}{\figure{gestures_zoom_pan.png}{options: alt="Zoom pan gestures"}}
#'
#'
#'   Selecting Points/Objects
#'
#'
#'   \if{html}{\figure{gestures_select.png}{options: alt="Select gestures"}}
#'
#'
#'   Moving Points on the Scatterplot Display
#'
#'
#'   \if{html}{\figure{gestures_move.png}{options: alt="Move gestures"}}
#'
#'   NOTE: Although it is possible to programmatically add layers to an l_plot3D, these will not
#'   appear as part of the 3D plot's display. There is no provision at present to incorporate
#'   rotation of 3D geometric objects other than point glyphs.
#'
#' @template return_widget_handle
#'
#' @seealso Turn interactive loon plot static \code{\link{loonGrob}}, \code{\link{grid.loon}}, \code{\link{plot.loon}}.
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
l_plot3D <- function(x, y, z, ...) {
    UseMethod("l_plot3D")
}


#' @rdname l_plot3D
#' @param axisScaleFactor the amount to scale the axes at the centre of the rotation.
#'          Default is 1.
#'              All numerical values are acceptable (0 removes the axes, < 0 inverts the direction of
#'              all axes.)
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
#' @param zlabel Label for the third (perpendicular to the screen) (z) axis. If missing,
#'               one will be inferred from \code{z} (or \code{x}) if possible.
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
#'
#' @details The scatterplot displays a number of direct interactions with the
#'   mouse and keyboard, these include: rotating, zooming towards the mouse cursor using
#'   the mouse wheel, panning by right-click dragging and various selection
#'   methods using the left mouse button such as sweeping, brushing and
#'   individual point selection. See the documentation for \code{\link{l_plot3D}}
#'   for more details about the interaction gestures.
#'
#' @export
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
                              on,
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
                              parent = NULL,
                              ...) {

    dotArgs <- list(...)
    # set by dotArgs, used for facetting
    byArgs <- dotArgs[l_byArgs()]
    # dotArgs passed into loonPlotFactory
    dotArgs[l_byArgs()] <- NULL

    l_className <- c("l_plot3D", "l_plot")

    if(missing(title)) { title <- "" }

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
                dotArgs,
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

        class(plot) <- c(l_className, class(plot))
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

        # check which states are modified
        call <- match.call()
        modifiedLinkedStates <- l_modifiedLinkedStates(l_className[1L], names(call))

        n <- length(x)
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
        dotArgs$z <- z
        dotArgs$color <- color
        dotArgs$glyph <- glyph
        dotArgs$size <- size
        dotArgs$active <- active
        dotArgs$selected <- selected

        if(is.null(by)) {
            dotArgs <- l_na_omit(l_className[1L], dotArgs)

            plot <- do.call(
                loonPlotFactory,
                c(
                    dotArgs,
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
                                     l_className = l_className[1L])
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
                                factory_tclcmd = '::loon::plot3D',
                                factory_path = 'plot3D',
                                factory_window_title = 'loon scatterplot3D',
                                modifiedLinkedStates = modifiedLinkedStates,
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
