
#' @title Create an interactive histogram
#' @name l_hist
#' @description \code{l_hist} is a generic function for creating interactive histogram displays
#' that can be linked with loon's other displays.
#'
#' @family loon interactive states
#'
#' @param x vector with numerical data to perform the binning on x,
#' @param ... named arguments to modify the histogram plot states or layouts, see details.
#'
#' @templateVar page  learn_R_display_hist
#' @template see_l_help_page
#'
#' @template return_widget_handle
#'
#' @export
l_hist <- function(x, ...) {
    UseMethod("l_hist")
}

#' @rdname l_hist
#' @param yshows one of "frequency" (default) or  "density"
#' @template param_by
#' @template param_on
#' @template param_layout
#' @template param_connectedScales
#' @param origin numeric scalar to define the binning origin
#' @param binwidth a numeric scalar to specify the binwidth
#'   If NULL \code{binwidth} is set using David Scott's rule when \code{x} is numeric
#'   (namely 3.49 * sd(x)/(n ^(1/3)) if sd(x) > 0 and 1 if sd(x) == 0)
#'   and using the minumum numerical difference between factor levels when \code{x}
#'   is a factor or a character vector (coerced to factor).
#' @param showStackedColors  if TRUE (default) then bars will be coloured according to
#'    colours of the points; if FALSE, then the bars will be a uniform colour
#'    except for highlighted points.
#' @param showBinHandle If \code{TRUE}, then an interactive "bin handle" appears on the plot
#'   whose movement resets the \code{origin} and the \code{binwidth}.  Default is \code{FALSE}
#' @param color colour fills of bins; colours are repeated
#'  until matching the number x.
#'  Default is found using \code{\link{l_getOption}("color")}.
#' @template param_active
#' @template param_selected
#' @param xlabel label to be used on the horizontal axis. If NULL, an attempt at a meaningful label
#'   inferred from \code{x} will be made.
#' @template param_showLabels
#' @template param_showScales
#' @template param_showGuides
#' @template param_parent
#' @param ... named arguments to modify the histogram plot states or layouts, see details.
#'
#' @details \itemize{
#'   \item {
#'   Note that when changing the \code{yshows} state from
#'   \code{'frequency'} to \code{'density'} you might have to use
#'   \code{\link{l_scaleto_world}} to show the complete histogram in the plotting
#'   region.
#'   }
#'   \item {Some arguments to modify layouts can be passed through,
#'   e.g. "separate", "byrow", etc.
#'   Check \code{\link{l_facet}} to see how these arguments work.
#'   }
#' }
#'
#' @seealso Turn interactive loon plot static \code{\link{loonGrob}}, \code{\link{grid.loon}}, \code{\link{plot.loon}}.
#'
#' @export
#' @examples
#' if(interactive()){
#'
#' h <- l_hist(iris$Sepal.Length)
#'
#' names(h)
#' h["xlabel"] <- "Sepal length"
#' h["showOutlines"] <- FALSE
#'
#' h["yshows"]
#' h["yshows"] <- "density"
#' l_scaleto_plot(h)
#'
#' h["showStackedColors"] <- TRUE
#' h['color'] <- iris$Species
#' h["showStackedColors"] <- FALSE
#' h["showOutlines"] <- TRUE
#' h["showGuides"] <- FALSE
#'
#' # link another plot with the previous plot
#' h['linkingGroup'] <- "iris_data"
#' h2 <- with(iris, l_hist(Petal.Width,
#'                         linkingGroup="iris_data",
#'                         showStackedColors = TRUE))
#'
#'
#' # Get an R (grid) graphics plot of the current loon plot
#' plot(h)
#' # or with more control about grid parameters
#' grid.loon(h)
#' # or to save the grid data structure (grob) for later use
#' hg <- loonGrob(h)
#'
#'}
l_hist.default <-  function(x,
                            yshows = c("frequency", "density"),
                            by = NULL,
                            on,
                            layout = c("grid", "wrap", "separate"),
                            connectedScales = c("cross", "row", "column", "both", "x", "y", "none"),
                            origin = NULL,
                            binwidth = NULL,
                            showStackedColors = TRUE,
                            showBinHandle = FALSE,
                            color = l_getOption("color"),
                            active = TRUE,
                            selected = FALSE,
                            xlabel = NULL,
                            showLabels = TRUE,
                            showScales = FALSE,
                            showGuides = TRUE,
                            parent = NULL,
                            ...) {

    dotArgs <- list(...)
    # set by dotArgs, used for facetting
    byArgs <- dotArgs[l_byArgs()]
    # dotArgs passed into loonPlotFactory
    dotArgs[l_byArgs()] <- NULL

    l_className <- "l_hist"

    if(missing(x)) {

        yshows <- match.arg(yshows)
        if (is.null(origin) | !is.numeric(origin)){
            origin <- 0
        }
        if (is.null(binwidth)| !is.numeric(binwidth)) {
            binwidth <- 1
        }
        if (is.null(xlabel)| !is.character(xlabel)){
            xlabel <- gsub("\"", "", deparse(substitute(x)))
        }

        hist <- do.call(
            loonPlotFactory,
            c(
                dotArgs,
                list(
                    factory_tclcmd = '::loon::histogram',
                    factory_path = 'hist',
                    factory_window_title = 'loon histogram',
                    parent = parent,
                    yshows = yshows,
                    showStackedColors = showStackedColors,
                    origin = origin,
                    binwidth = binwidth,
                    showBinHandle = showBinHandle,
                    showLabels = showLabels,
                    showScales = showScales,
                    showGuides = showGuides,
                    xlabel = xlabel
                )
            )
        )

        class(hist) <- c(l_className, class(hist))
        return(hist)

    } else {

        # x should be a vector and a vector should return NULL when we call dim(x)
        dim_x <- dim(x)
        if(!is.null(dim_x))
            stop("Unkown data structure",
                 call. = FALSE)

        n <- length(x)

        call <- match.call()
        modifiedLinkedStates <- l_modifiedLinkedStates(l_className, names(call))

        color <- aes_settings(color, n, ifNoStop = FALSE)
        active <- aes_settings(active, n, ifNoStop = TRUE)
        selected <- aes_settings(selected, n, ifNoStop = TRUE)

        if (is.null(xlabel))
            xlabel <- gsub("\"", "", deparse(substitute(x)))

        yshows <- match.arg(yshows)
        ## ylabel will be overwritten in ...
        if (is.null(origin) | !is.numeric(origin)) {
            origin <- min(x, na.rm = TRUE)
        }

        if (is.null(binwidth) | !is.numeric(binwidth)) {
            # Sturges rule
            # binwidth <- diff(range(x))/(1 + 3.322 * (log(n, base = 10)))
            # David Scott's rule
            sd <- sd(x, na.rm = TRUE)
            binwidth <- if (sd == 0 || is.na(sd)) {1} else  {3.49 * sd/(n ^(1/3))}
        }


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
        dotArgs$color <- color
        dotArgs$active <- active
        dotArgs$selected <- selected

        if(is.null(by)) {

            dotArgs <- l_na_omit(l_className, dotArgs)

            hist <- do.call(
                loonPlotFactory,
                c(
                    dotArgs,
                    list(
                        factory_tclcmd = '::loon::histogram',
                        factory_path = 'hist',
                        factory_window_title = 'loon histogram',
                        parent = parent,
                        yshows = yshows,
                        showStackedColors = showStackedColors,
                        origin = origin,
                        binwidth=binwidth,
                        showBinHandle = showBinHandle,
                        showLabels = showLabels,
                        showScales = showScales,
                        showGuides = showGuides,
                        xlabel = xlabel
                    )
                )
            )

            if(!is.null(linkingGroup)) {

                syncTemp <- ifelse(length(modifiedLinkedStates) == 0,  sync, "pull")
                if(syncTemp == "push")
                    message("The modification of linked states is not detected",
                            " so that the default settings will be pushed to all plots")
                # configure hist (linking)
                l_configure(hist,
                            linkingGroup = linkingGroup,
                            sync = syncTemp)

                if(sync == "push" && length(modifiedLinkedStates) > 0) {

                    do.call(l_configure,
                            c(
                                list(
                                    target = hist,
                                    linkingGroup = linkingGroup,
                                    sync = sync
                                ),
                                dotArgs[modifiedLinkedStates]
                            )
                    )
                } else {
                    l_linkingWarning(hist, sync, args = dotArgs,
                                     modifiedLinkedStates = modifiedLinkedStates,
                                     l_className = l_className)
                }
            }

            class(hist) <- c(l_className, class(hist))
            return(hist)

        } else {

            hists <- loonFacets(type = l_className,
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
                                factory_tclcmd = '::loon::histogram',
                                factory_path = 'hist',
                                factory_window_title = 'loon histogram',
                                showLabels = showLabels,
                                showScales = showScales,
                                showGuides = showGuides,
                                modifiedLinkedStates = modifiedLinkedStates,
                                yshows = yshows,
                                showStackedColors = showStackedColors,
                                origin = origin,
                                binwidth=binwidth,
                                showBinHandle = showBinHandle,
                                xlabel = xlabel,
                                ylabel = yshows)

            return(hists)
        }
    }
}

#' @rdname l_hist
#' @param showFactors whether to show the factor levels as factor labels layered on the plot.
#'        If \code{FALSE}, the factor labels are hidden and can be turned on
#'        from the "layers" tab on the inspector.
#' @param factorLabelAngle is the angle of rotation (in degrees) for the factor labels.
#' If not specified, an angle of 0 is chosen if there are fewer than 10 labels; labels are
#' rotated 90 degrees if there are 10 or more. This can also be a numeric vector of length
#' equal to the number of factor levels in \code{x}.
#' @param factorLabelSize is the font size for the factor labels (default 12).
#' @param factorLabelColor is the colour to be used for the factor labels.
#' (default is \code{l_getOption("foreground")}). Can be a vector of length
#' equal to that of the number of factor levels in \code{x}.
#' @param factorLabelY either a single number (default 0), or a numeric vector of length
#' equal to that of the number of factor levels, determining the
#' y coordinate(s) for the factor labels.
#' @export
l_hist.factor <-  function(x,
                           showFactors = length(unique(x)) < 25L,
                           factorLabelAngle,
                           factorLabelSize = 12,
                           factorLabelColor = l_getOption("foreground"),
                           factorLabelY = 0,
                           ...) {

    if(missing(x))
        return(
            l_hist.default(x, ...)
        )

    dotArgs <- list(...)

    if (is.null(dotArgs$xlabel)) {
        dotArgs$xlabel <-  gsub("\"", "", deparse(substitute(x)))
    }

    if (!is.null(dotArgs$yshows)) {
        if(dotArgs$yshows == "density"){
            dotArgs$yshows <- "frequency"
            warning("For character or factor data, `yshows` cannot be `density`.",
                    "Switched `yshows` to ", dotArgs$yshows)
        }
    }

    x <- as.factor(x)

    levelNames <- levels(x)
    nlevels <- length(levelNames)
    if(missing(factorLabelAngle)){
        if(nlevels >= 10) {
            factorLabelAngle <- 90
        } else {
            factorLabelAngle <- 0
        }
    }
    if(!is.numeric(factorLabelY) | (length(factorLabelY) == 0)) {
        warning("factorLabelY must be numeric; using default -1")
        factorLabelY <- rep(-1, nlevels)
    } else {
        if(length(factorLabelY) != nlevels) {
            factorLabelY <- rep(factorLabelY,
                                length.out = nlevels)
            }
        }


    x <-  unclass(x)  # Get the level numbers as numeric values
    dotArgs$x <- x

    # check origin
    origin <- dotArgs$origin
    if (is.null(origin) || !is.numeric(origin)) {
        dotArgs$origin <- min(x, na.rm = TRUE)
    }

    # check binwidth
    binwidth <- dotArgs$binwidth
    if (is.null(binwidth) || !is.numeric(binwidth)) {
        uni_x <- unique(x)
        binwidth <- if(length(uni_x) == 1) {
            # This is a single bin histogram
            # the bin width can be set as any non-negative value
            0.1
        } else {
            min(diff(sort(uni_x)))
        }

        dotArgs$binwidth <- binwidth
    }

    hist <- do.call(l_hist.default, dotArgs)

    # Add level names to plot
    ## Adjust text coordinates
    ## The reason to do so is to make sure that
    ## `labels` always lay down the corresponding bins
    ##  no matter how origin shifts


    if(inherits(hist, "l_compound")) {

        lapply(hist,
               function(h) {
                   text_adjust <- h['origin']
                   if(text_adjust > 1 || text_adjust <= 0) {
                       text_adjust <- text_adjust - as.integer(text_adjust)
                       if(text_adjust <= 0) text_adjust <- text_adjust + 1
                   }

                   text_adjust <- text_adjust - 0.5

                   text_layer <- l_layer_texts(h,
                                               x = seq(nlevels) + text_adjust,
                                               y = factorLabelY,
                                               text = levelNames,
                                               label = "Factor levels",
                                               angle = factorLabelAngle,
                                               size = factorLabelSize,
                                               color = factorLabelColor)

                   if(!showFactors) l_layer_hide(h, text_layer)
               })

    } else {
        text_adjust <- hist['origin']
        if(text_adjust > 1 || text_adjust <= 0) {
            text_adjust <- text_adjust - as.integer(text_adjust)
            if(text_adjust <= 0) text_adjust <- text_adjust + 1
        }

        text_adjust <- text_adjust - 0.5

        text_layer <- l_layer_texts(hist,
                                    x = seq(nlevels) + text_adjust,
                                    y = factorLabelY,
                                    text = levelNames,
                                    label = "Factor levels",
                                    angle = factorLabelAngle,
                                    size = factorLabelSize,
                                    color = factorLabelColor)

        if(!showFactors) l_layer_hide(hist, text_layer)
    }

    hist
}

#' @rdname l_hist
#' @param showFactors whether to show the factor labels (unique strings in \code{x})
#'        as a layer on the plot.
#'        If \code{FALSE}, the factor labels are  hidden and can be turned on
#'        from the "layers" tab on the inspector.
#' @param factorLabelAngle is the angle of rotation (in degrees) for the factor labels.
#' If not specified, an angle of 0 is chosen if there are fewer than 10 labels; labels are
#' rotated 90 degrees if there are 10 or more.  This can also be a numeric vector of length
#' equal to the number of factor labels.
#' @param factorLabelSize is the font size for the factor labels (default 12).
#' @param factorLabelColor is the colour to be used for the factor labels.
#' (default is \code{l_getOption("foreground")}). Can also be a vector
#' equal to that of the number of factor labels.
#' @param factorLabelY either a single number, or a numeric vector of length
#' equal to the number of factor labels, determining the
#' y coordinate(s) for the factor labels.
#' @export
l_hist.character <- function(x,
                             showFactors = length(unique(x)) < 25L,
                             factorLabelAngle,
                             factorLabelSize = 12,
                             factorLabelColor = l_getOption("foreground"),
                             factorLabelY = 0,
                             ...) {

    if(missing(x))
        return(
            l_hist.default(x, ...)
        )

    nlevels <- length(unique(x))
    if(missing(factorLabelAngle)){
        if(nlevels >= 10) {
            factorLabelAngle <- 90
        } else {
            factorLabelAngle <- 0
        }
    }

    l_hist.factor(x,
                  showFactors = showFactors,
                  factorLabelAngle = factorLabelAngle,
                  factorLabelSize = factorLabelSize,
                  factorLabelColor = factorLabelColor,
                  factorLabelY = factorLabelY,
                  ...)
}

#' @rdname l_hist
#' @export
l_hist.data.frame <- function(x, ...) {

    if(missing(x))
        return(
            l_hist.default(x, ...)
        )

    dotArgs <- list(...)
    xlabel <- dotArgs$xlabel

    # get a relatively informative xlabel
    if (is.null(xlabel)){

        name <- colnames(x)
        if (is.null(name)) {
            name <- "column 1"
        }
        name <- name[1L]
        dataname <- gsub("\"", "", deparse(substitute(x)))
        dotArgs$xlabel <- paste(name, "from", dataname)
    }

    # the first column
    dotArgs$x <- x[, 1L]

    do.call(l_hist, dotArgs)
}

#' @rdname l_hist
#' @export
l_hist.matrix <- function(x, ...) {

    l_hist(c(x), ...)
}

#' @rdname l_hist
#' @export
l_hist.list <- function(x, ...) {

    dotArgs <- list(...)
    by <- dotArgs$by

    if(is.null(by)) {
        message("The default argument `by` is set based on the list")
        dotArgs$by <- rep(seq(length(x)), lengths(x))
    }

    dotArgs$x <- unlist(x)

    do.call(l_hist, dotArgs)
}

#' @rdname l_hist
#' @export
l_hist.table <- function(x, ...) {

    dim_x <- dim(x)
    if(length(dim_x) > 2L)
        stop(x,
             "should have at most two dimensions",
             call. = FALSE)

    dotArgs <- list(...)
    xlabel <- dotArgs$xlabel

    if (is.null(xlabel)){
        name <- colnames(x)
        if (is.null(name)) {
            name <- "column 1"
        }
        name <- name[1L]
        dataname <- gsub("\"", "", deparse(substitute(x)))
        dotArgs$xlabel <- paste(name, "from", dataname)
    }
    dotArgs$x <- x[, 1L]
    do.call(l_hist, dotArgs)
}

#' @rdname l_hist
#' @export
l_hist.array <- function(x, ...) {

    l_hist.table(x, ...)
}
