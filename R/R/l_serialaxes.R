#' @title Create a Serialaxes Widget
#'
#' @description The serialaxes widget displays multivariate data either as a
#'   stacked star glyph plot, or as a parallel coordinate plot.
#'
#'
#' @param data a data frame with numerical data only
#' @param sequence vector with variable names that defines the axes sequence
#' @param scaling one of 'variable', 'data', 'observation' or 'none' to specify
#'   how the data is scaled. See Details and Examples for more information.
#' @param axesLayout either \code{"radial"} or \code{"parallel"}
#' @param by loon plot can be separated by some variables into mutiple panels.
#' This argument can take a \code{vector}, a \code{list} of same lengths or a \code{data.frame} as input.
#' @param layout layouts in a \code{'grid'} or a \code{'wrap'}
#' @param showAxes boolean to indicate whether axes should be shown or not
#' @param linewidth vector with line widths.
#' Default is given by \code{\link{l_getOption}("linewidth")}.
#' @param color vector with line colors.
#' Default is given by \code{\link{l_getOption}("color")}.
#' @param active a logical determining whether items appear or not
#' (default is \code{TRUE} for all items). If a logical vector is given of length
#' equal to the number of items, then it identifies which items appear (\code{TRUE})
#' and which do not (\code{FALSE}).
#' @param selected a logical determining whether items appear selected at first
#' (default is \code{FALSE} for all items). If a logical vector is given of length
#' equal to the number of items, then it identifies which items are (\code{TRUE})
#' and which are not (\code{FALSE}).
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
#'
#' @return plot handle object
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' s <- l_serialaxes(data=oliveAcids, color=olive$Area, title="olive data")
#' s['axesLayout'] <- 'parallel'
#' states <- l_info_states(s)
#' names(states)
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

l_serialaxes <- function(data,
                         sequence,
                         scaling="variable",
                         axesLayout='radial',
                         by = NULL,
                         layout = c("grid", "wrap", "separate"),
                         showAxes=TRUE,
                         linewidth = l_getOption("linewidth"),
                         color = l_getOption("color"),
                         active = TRUE,
                         selected = FALSE,
                         parent=NULL, ... ){

    args <- list(...)
    # set by args, used for facetting
    by_args <- args[l_byArgs()]
    # args passed into loonPlotFactory
    args[l_byArgs()] <- NULL

    data <- as.data.frame(data)

    if (missing(sequence)) {
        sequence <- names(data)
    }

    sync <- args$sync

    if(is.null(sync)) {
        sync <- "pull"
        if(length(color) > 1) {
            sync <- "push"
        } else {
            if(length(color) == 1 && !is.na(color) && color != "steelblue") sync <- "push"
        }

        if(length(linewidth) != 1) {
            sync <- "push"
        } else {
            if(length(linewidth) == 1 && !is.na(linewidth) && linewidth != 1) sync <- "push"
        }
    }

    n <- dim(data)[1]
    len_color <- length(color)
    if (len_color > 1) {
        if (len_color != n) {
            color <- rep_len(color, n)
        }
    } else {
        if(is.na(color)) color <- l_getOption("color")
    }

    len_linewidth <- length(linewidth)
    if (len_linewidth > 1) {
        if (len_linewidth != n) {
            linewidth <- rep_len(linewidth, n)
        }
    } else {
        if(is.na(linewidth)) linewidth <- 1
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

    linkingGroup <- args[["linkingGroup"]]
    args$linkingGroup <- NULL
    # n dimensional states NA check
    args$data <- data
    args$color <- color
    args$linewidth <- linewidth
    args$active <- active
    args$selected <- selected

    if(is.null(by)) {
        args <- l_na_omit("l_serialaxes", args)
        args$data <- l_data(args$data)

        plot <- do.call(
            loonPlotFactory,
            c(
                args,
                list(
                    factory_tclcmd = '::loon::serialaxes',
                    factory_path = 'serialaxes',
                    factory_window_title = 'loon serialaxes plot',
                    parent = parent,
                    sequence = sequence,
                    showAxes = showAxes,
                    scaling = scaling,
                    axesLayout = axesLayout
                )
            )
        )

        if(!is.null(linkingGroup)) {
            l_configure(plot,
                        linkingGroup = linkingGroup,
                        sync = sync)
        }

        class(plot) <- c("l_serialaxes", class(plot))
        return(plot)

    } else {

        # convert all types of 'by' to a data frame
        byDeparse <- deparse(substitute(by))

        if(is.atomic(by)) {
            if(length(by) == n) {
                by <- setNames(data.frame(by, stringsAsFactors = FALSE), byDeparse)
            } else {
                warning("Set 'by' as variables is not recommended")
                by <- data[by]
            }
        } else {

            if(is.null(names(by))) {

                by <- as.data.frame(by, stringsAsFactors = FALSE)
                names(by) <- NULL
            } else {
                by <- as.data.frame(by, stringsAsFactors = FALSE)
            }
        }

        plots <- loonFacets(type = "l_serialaxes",
                            by,
                            args,
                            byDeparse = byDeparse,
                            layout = match.arg(layout),
                            by_args = Filter(Negate(is.null), by_args),
                            linkingGroup = linkingGroup,
                            sync = sync,
                            parent = parent,
                            factory_tclcmd = '::loon::serialaxes',
                            factory_path = 'serialaxes',
                            factory_window_title = 'loon serialaxes plot',
                            sequence = sequence,
                            showAxes = showAxes,
                            scaling = scaling,
                            axesLayout = axesLayout)

        return(plots)

    }
}
