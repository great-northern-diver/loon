#' @title Create an interactive serialaxes (parallel axes or radial axes) plot
#' @name l_serialaxes
#' @family loon interactive states
#' @description \code{l_serialaxes} is a generic function for displaying multivariate data either as a
#' stacked star glyph plot, or as a parallel coordinate plot.
#' @param data a data frame with numerical data only
#' @param ... named arguments to modify the serialaxes plot states or layouts
#'
#' @templateVar page  learn_R_display_hist
#' @template see_l_help_page
#'
#' @template return_widget_handle
#'
#'
#' @export
l_serialaxes <- function(data, ...) {
    UseMethod("l_serialaxes")
}


#' @rdname l_serialaxes
#' @param sequence vector with variable names that defines the axes sequence
#' @param scaling one of 'variable', 'data', 'observation' or 'none' to specify
#'   how the data is scaled. See Details and Examples for more information.
#' @param axesLayout either \code{"radial"} or \code{"parallel"}
#' @template param_by
#' @template param_on
#' @template param_layout
#' @param andrews Andrew's plot (a 'Fourier' transformation)
#' @param showAxes boolean to indicate whether axes should be shown or not
#' @param linewidth vector with line widths. Default is given by \code{\link{l_getOption}("linewidth")}.
#' @param color vector with line colors. Default is given by \code{\link{l_getOption}("color")}.
#' @template param_active
#' @template param_selected
#' @template param_parent
#' @template param_dots_state_args
#' @param ... named arguments to modify the serialaxes states or layouts, see details.
#'
#' @details \itemize{
#'   \item {
#'   The \code{scaling} state defines how the data is scaled. The axes
#'   display 0 at one end and 1 at the other. For the following explanation
#'   assume that the data is in a nxp dimensional matrix. The scaling options
#'   are then
#' \tabular{ll}{
#' variable \tab per column scaling\cr
#' observation \tab per row scaling\cr
#' data \tab whole matrix scaling\cr
#' none \tab do not scale
#' }
#'   }
#'   \item {Some arguments to modify layouts can be passed through,
#'   e.g. "separate", "byrow", etc.
#'   Check \code{\link{l_facet}} to see how these arguments work.
#'   }
#' }
#'
#' @seealso Turn interactive loon plot static \code{\link{loonGrob}}, \code{\link{grid.loon}}, \code{\link{plot.loon}}.
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' #######
#' #
#' # Effect of the choice of the argument "scaling"
#' #
#' # To illustrate we will look at the four measurements of
#' # 150 iris flowers from the iris data of Edgar Anderson made
#' # famous by R.A. Fisher.
#' #
#' # First separate the measurements
#' irisFlowers <- iris[, 1:4]
#' # from their species
#' species <- iris[,5]
#' # and get some identifiers for the individual flowers
#' flowerIDs <- paste(species, 1:50)
#' #
#' # Now create parallel axes plots of the measurements
#' # using different scaling values.
#'
#' #
#' # scaling = "variable"
#' #
#' # This is the standard scaling of most serial axes plots,
#' # scaling each axis from the minimum to the maximum of that variable.
#' # Hence it is the default scaling.
#' #
#' # More precisely, it maps the minimum value in each column (variable) to
#' # zero and the maximum to one.  The result is every parallel
#' # axis will have a point at 0 and a point at 1.
#' #
#' # This scaling highlights the relationships (e.g. correlations)
#' # between the variables (removes the effect of the location and scale of
#' # each variable).
#' #
#' # For the iris data, ignoring species we see for example that
#' # Sepal.Length and Sepal.Width are negatively correlated (lots of
#' # crossings) across species but more positively correlated (mostly
#' # parallel lines) within each species (colour).
#' #
#' sa_var <- l_serialaxes(irisFlowers,
#'                        scaling = "variable",    # scale within column
#'                        axesLayout = "parallel",
#'                        color = species,
#'                        linewidth = 2,
#'                        itemLabel = flowerIDs,
#'                        showItemLabels = TRUE,
#'                        title = "scaling = variable (initially)",
#'                        linkingGroup = "irisFlowers data")
#'
#' #
#' # scaling = "observation"
#' #
#' # This maps the minimum value in each row (observation) to
#' # zero and the maximum value in each row to one.
#' #
#' # The result is that every observation (curve in the parallel
#' # coordinate plot) will touch 0 on at least one axis and touch
#' # 1 on another.
#' #
#' # This scaling highlights the differences between observations (rows)
#' # in terms of the relative measurements across the variables for each
#' # observation.
#' #
#' # For example, for the iris data we can see that for every flower (row)
#' # the Sepal.Length is the largest measurement and the Petal.Width
#' # is the smallest.  Each curve gives some sense of the *shape* of each
#' # flower without regard to its size.  Two species (versicolor and
#' # virginica) have similar shaped flowers (relatively long but narrow
#' # sepals and petals), whereas the third (setosa) has relatively large
#' # sepals compared to small petals.
#' #
#' sa_obs <- l_serialaxes(irisFlowers,
#'                        scaling = "observation", # scale within row
#'                        axesLayout = "parallel",
#'                        color = species,
#'                        linewidth = 2,
#'                        itemLabel = flowerIDs,
#'                        showItemLabels = TRUE,
#'                        title = "scaling = observation (initially)",
#'                        linkingGroup = "irisFlowers data")
#'
#' #
#' # scaling = "data"
#' #
#' # This maps the minimum value in the whole dataset (over all elements)
#' # to zero and the maximum value in the whole dataset to one.
#' #
#' # The result is that every measurement is on the same numeric (if not
#' # measurement) scale.  Highlighting the relative magnitudes of all
#' # numerical values in the data set, each curve shows the relative magnitudes
#' # without rescaling by variable.
#' #
#' # This is most sensible data such as the iris flower where all four measurements
#' # appear to have been taken on the same measuring scale.
#' #
#' # For example, for the iris data full data scaling preserves the size
#' # and shape of each flower.  Again virginica is of roughly the same
#' # shape as versicolor but has distinctly larger petals.
#' # Setosa in contrast is quite differently shaped in both sepals and petals
#' # but with sepals more similar in size to the two other flowers and
#' # with significantly smaller petals.
#' sa_dat <- l_serialaxes(irisFlowers,
#'                        scaling = "data",        # scale using all data
#'                        axesLayout = "parallel",
#'                        color = species,
#'                        linewidth = 2,
#'                        itemLabel = flowerIDs,
#'                        showItemLabels = TRUE,
#'                        title = "scaling = data (initially)",
#'                        linkingGroup = "irisFlowers data")
#'
#' #
#' #  scaling = "none"
#' #
#' #  Sometimes we might wish to choose a min and max to use
#' #  for the whole data set; or perhaps a separate min and max
#' #  for each variable.
#'
#' #  This would be done outside of the construction of the plot
#' #  and displayed by having scaling = "none" in the plot.
#' #
#' #  For example, for the iris data, we might choose scales so that
#' #  the minimum and the maximum values within the data set do not
#' #  appear at the end points 0 and 1 of the axes but instead inside.
#' #
#' #  Suppose we choose the following limits for all variables
#' lower_lim <- -3 ; upper_lim <- max(irisFlowers) + 1
#'
#' #  These are the limits we want to use to define the end points of
#' #  the axes for all variables.
#' #  We need only scale the data as
#' irisFlowers_0_1 <- (irisFlowers - lower_lim)/(upper_lim - lower_lim)
#' #  Or alternatively using the built-in scale function
#' #  (which allows different scaling for each variable)
#' irisFlowers_0_1 <- scale(irisFlowers,
#'                          center = rep(lower_lim, 4),
#'                          scale = rep((upper_lim - lower_lim), 4))
#'
#' # Different scales for different
#' # And instruct the plot to not scale the data but plot it on the 0-1 scale
#' # for all axes.  (Note any rescaled date outside of [0,1] will not appear.)
#' #
#' sa_none <- l_serialaxes(irisFlowers_0_1,
#'                         scaling = "none",        # do not scale
#'                         axesLayout = "parallel",
#'                         color = species,
#'                         linewidth = 2,
#'                         itemLabel = flowerIDs,
#'                         showItemLabels = TRUE,
#'                         title = "scaling = none (initially)",
#'                         linkingGroup = "irisFlowers data")
#'
#' # This is particularly useful for "radial" axes to keep the polygons away from
#' # the centre of the display.
#' # For example
#' sa_none["axesLayout"] <- "radial"
#' # now displays each flower as a polygon where shapes and sizes are easily
#' # compared.
#' #
#' #  NOTE: rescaling the data so that all values are within [0,1] is perhaps
#' #        the best way to proceed (especially if there are natural lower and
#' #        upper limits for each variable).
#' #        Then scaling can always be changed via the inspector.
#'
#' }

l_serialaxes.default <- function(data,
                                 sequence,
                                 scaling="variable",
                                 axesLayout='radial',
                                 by = NULL,
                                 on,
                                 layout = c("grid", "wrap", "separate"),
                                 andrews = FALSE,
                                 showAxes=TRUE,
                                 color = l_getOption("color"),
                                 active = TRUE,
                                 selected = FALSE,
                                 linewidth = l_getOption("linewidth"),
                                 parent=NULL,
                                 ...) {

    dotArgs <- list(...)
    # set by dotArgs, used for facetting
    byArgs <- dotArgs[l_byArgs()]
    # dotArgs passed into loonPlotFactory
    dotArgs[l_byArgs()] <- NULL

    l_className <- "l_serialaxes"

    if(missing(data)) {
        plot <- do.call(
            loonPlotFactory,
            c(
                dotArgs,
                list(
                    factory_tclcmd = '::loon::serialaxes',
                    factory_path = 'serialaxes',
                    factory_window_title = 'loon serialaxes plot',
                    parent = parent,
                    showAxes = showAxes
                )
            )
        )

        class(plot) <- c(l_className, class(plot))
        return(plot)
    }

    data <- as.data.frame(data)

    if (missing(sequence)) {
        sequence <- names(data)
    }

    n <- dim(data)[1]

    call <- match.call()
    modifiedLinkedStates <- l_modifiedLinkedStates(l_className, names(call))

    color <- aes_settings(color, n, ifNoStop = FALSE)
    linewidth <- aes_settings(linewidth, n, ifNoStop = FALSE)
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
    dotArgs$data <- data
    dotArgs$color <- color
    dotArgs$linewidth <- linewidth
    dotArgs$active <- active
    dotArgs$selected <- selected

    if(is.null(by)) {
        dotArgs <- l_na_omit(l_className, dotArgs)
        dotArgs$data <- l_data(dotArgs$data)

        plot <- do.call(
            loonPlotFactory,
            c(
                dotArgs,
                list(
                    factory_tclcmd = '::loon::serialaxes',
                    factory_path = 'serialaxes',
                    factory_window_title = 'loon serialaxes plot',
                    parent = parent,
                    sequence = sequence,
                    showAxes = showAxes,
                    scaling = scaling,
                    axesLayout = axesLayout,
                    andrews = andrews
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
                            byArgs = Filter(Negate(is.null), byArgs),
                            linkingGroup = linkingGroup,
                            sync = sync,
                            parent = parent,
                            factory_tclcmd = '::loon::serialaxes',
                            factory_path = 'serialaxes',
                            factory_window_title = 'loon serialaxes plot',
                            modifiedLinkedStates = modifiedLinkedStates,
                            sequence = sequence,
                            showAxes = showAxes,
                            scaling = scaling,
                            axesLayout = axesLayout)

        return(plots)

    }
}
