
#' @title Create an interactive histogram
#'
#' @description \code{l_hist} is a generic function for creating an interactive histogram display that can be linked with
#'   loon's other displays
#'
#' @param x vector with numerical data to perform the binning on x,
#' @param yshows one of "frequency" (default) or  "density"
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
#' @param showStackedColors  if TRUE (default) then bars will be coloured according to
#'    colours of the points; if FALSE, then the bars will be a uniform colour
#'    except for highlighted points.
#' @param origin numeric scalar to define the binning origin
#' @param binwidth a numeric scalar to specify the binwidth
#'   If NULL \code{binwidth} is set using David Scott's rule when \code{x} is numeric
#'   (namely 3.49 * sd(x)/(n ^(1/3)) if sd(x) > 0 and 1 if sd(x) == 0)
#'   and using the minumum numerical difference between factor levels when \code{x}
#'   is a factor or a character vector (coerced to factor).
#' @param showBinHandle If \code{TRUE}, then an interactive "bin handle" appears on the plot
#'   whose movement resets the \code{origin} and the \code{binwidth}.  Default is \code{FALSE}
#' @param color colour fills of bins; colours are repeated
#'  until matching the number x.
#'  Default is found using \code{\link{l_getOption}("color")}.
#' @param active a logical determining whether x appears or not
#' (default is \code{TRUE} for all x). If a logical vector is given of length
#' equal to the number of x, then it identifies which x appears (\code{TRUE})
#' and which does not (\code{FALSE}).
#' @param selected a logical determining whether x appears selected at first
#' (default is \code{FALSE} for all x). If a logical vector is given of length
#' equal to the number of x, then it identifies which x is (\code{TRUE})
#' and which is not (\code{FALSE}).
#' @param xlabel label to be used on the horizontal axis. If NULL, an attempt at a meaningful label
#'   inferred from \code{x} will be made.
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
#' @templateVar page  learn_R_display_hist
#' @template see_l_help_page
#'
#' @template return_widget_handle
#'
#' @seealso \code{\link{l_plot}}
#'
#' @export
#'
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
#' h["showGuides"] <- TRUE
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

l_hist <- function(x,
                   yshows = c("frequency", "density"),
                   by = NULL,
                   layout = c("grid", "wrap", "separate"),
                   connectedScales = c("cross", "row", "column", "both", "x", "y", "none"),
                   showStackedColors = TRUE,
                   origin = NULL,
                   binwidth=NULL,
                   showBinHandle = FALSE,
                   color = l_getOption("color"),
                   active = TRUE,
                   selected = FALSE,
                   xlabel = NULL,
                   parent=NULL, ...) {
    UseMethod("l_hist")
}

#' @export
l_hist.factor <-  function(x,
                           yshows = c("frequency", "density"),
                           by = NULL,
                           layout = c("grid", "wrap", "separate"),
                           connectedScales = c("cross", "row", "column", "both", "x", "y", "none"),
                           showStackedColors = TRUE,
                           origin = NULL,
                           binwidth=NULL,
                           showBinHandle = FALSE,
                           color = l_getOption("color"),
                           active = TRUE,
                           selected = FALSE,
                           xlabel = NULL,
                           parent=NULL, ...) {

    if(missing(x))
        return(
            l_hist.default(x,
                           by = by,
                           layout = match.arg(layout),
                           connectedScales = match.arg(connectedScales),
                           yshows = yshows,
                           showStackedColors = showStackedColors,
                           origin = origin,
                           binwidth=binwidth,
                           showBinHandle = showBinHandle,
                           color = color,
                           active = active,
                           selected = selected,
                           xlabel = xlabel,
                           parent=parent, ...)
        )

    if (is.null(xlabel)){
        xlabel <-  gsub("\"", "", deparse(substitute(x)))
    }

    levelNames <- levels(x)
    nlevels <- length(levelNames)
    x <-  unclass(x)  # Get the level numbers as numeric values
    if (is.null(origin) | !is.numeric(origin)) {
        origin <- min(x, na.rm = TRUE)
    }
    if (is.null(binwidth) | !is.numeric(binwidth)) {
        uni_x <- unique(x)
        binwidth <- if(length(uni_x) == 1) {
            # This is a single bin histogram
            # the binwidth can be set as any non-negtive value
            0.1
        } else {
            min(diff(sort(uni_x)))
        }
    }

    h <-  l_hist(x,
                 yshows = yshows,
                 by = by,
                 layout = match.arg(layout),
                 connectedScales = match.arg(connectedScales),
                 showStackedColors = showStackedColors,
                 origin = origin,
                 binwidth=binwidth,
                 showBinHandle = showBinHandle,
                 color = color,
                 active = active,
                 selected = selected,
                 xlabel = xlabel,
                 parent=parent, ...)

    # Add level names to plot
    ## Adjust text coords
    ## The reason to do so is to make sure that
    ## `labels` always lay down the corresponding bins no matter how origin shifts

    if(inherits(h, "l_compound")) {

        lapply(h,
               function(p) {
                   text_adjust <- p['origin']
                   if(text_adjust > 1 || text_adjust <= 0) {
                       text_adjust <- text_adjust - as.integer(text_adjust)
                       if(text_adjust <= 0) text_adjust <- text_adjust + 1
                   }

                   text_adjust <- text_adjust - 0.5

                   l_layer_texts(p, x = 1:nlevels + text_adjust, y = rep(-1, nlevels),
                                 text = levelNames, label = "Factor levels",
                                 angle = 0,
                                 size = 12, color = l_getOption("foreground"))
               })

    } else {
        text_adjust <- h['origin']
        if(text_adjust > 1 || text_adjust <= 0) {
            text_adjust <- text_adjust - as.integer(text_adjust)
            if(text_adjust <= 0) text_adjust <- text_adjust + 1
        }

        text_adjust <- text_adjust - 0.5

        l_layer_texts(h, x = 1:nlevels + text_adjust, y = rep(-1, nlevels),
                      text = levelNames, label = "Factor levels",
                      angle = 0,
                      size = 12, color = l_getOption("foreground"))
    }

    h
}


#' @export
l_hist.character <-  function(x,
                              yshows = c("frequency", "density"),
                              by = NULL,
                              layout = c("grid", "wrap", "separate"),
                              connectedScales = c("cross", "row", "column", "both", "x", "y", "none"),
                              showStackedColors = TRUE,
                              origin = NULL,
                              binwidth = NULL,
                              showBinHandle = FALSE,
                              color = l_getOption("color"),
                              active = TRUE,
                              selected = FALSE,
                              xlabel = NULL,
                              parent=NULL, ...) {

    if(missing(x))
        return(
            l_hist.default(x,
                           by = by,
                           layout = match.arg(layout),
                           connectedScales = match.arg(connectedScales),
                           yshows = yshows,
                           showStackedColors = showStackedColors,
                           origin = origin,
                           binwidth=binwidth,
                           showBinHandle = showBinHandle,
                           color = color,
                           active = active,
                           selected = selected,
                           xlabel = xlabel,
                           parent=parent, ...)
        )

    x <- factor(x)

    l_hist(x,
           by = by,
           layout = match.arg(layout),
           connectedScales = match.arg(connectedScales),
           yshows = yshows,
           showStackedColors = showStackedColors,
           origin = origin,
           binwidth=binwidth,
           showBinHandle = showBinHandle,
           color = color,
           active = active,
           selected = selected,
           xlabel = xlabel,
           parent=parent, ...)
}

#' @export
l_hist.default <-  function(x,
                            yshows = c("frequency", "density"),
                            by = NULL,
                            layout = c("grid", "wrap", "separate"),
                            connectedScales = c("cross", "row", "column", "both", "x", "y", "none"),
                            showStackedColors = TRUE,
                            origin = NULL,
                            binwidth = NULL,
                            showBinHandle = FALSE,
                            color = l_getOption("color"),
                            active = TRUE,
                            selected = FALSE,
                            xlabel = NULL,
                            parent = NULL,
                            ...) {

    args <- list(...)
    # set by args, used for facetting
    by_args <- args[l_byArgs()]
    # args passed into loonPlotFactory
    args[l_byArgs()] <- NULL

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

        plot <- do.call(
            loonPlotFactory,
            c(
                args,
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
                    xlabel = xlabel
                )
            )
        )

        class(plot) <- c("l_hist", class(plot))
        return(plot)

    } else {

        # x should be a vector and a vector should return NULL when we call dim(x)
        dim_x <- dim(x)
        if(!is.null(dim_x))
            stop("Unkown data structure",
                 call. = FALSE)
        sync <- args$sync

        if(is.null(sync)) {
            sync <- "pull"
            if(length(color) > 1) {
                sync <- "push"
            } else {
                if(length(color) == 1 && !is.na(color) && color != "grey60") sync <- "push"
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

        linkingGroup <- args[["linkingGroup"]]
        args$linkingGroup <- NULL
        # n dimensional states NA check
        args$x <- x
        args$color <- color
        args$active <- active
        args$selected <- selected

        if(is.null(by)) {

            args <- l_na_omit("l_hist", args)

            plot <- do.call(
                loonPlotFactory,
                c(
                    args,
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
                        xlabel = xlabel
                    )
                )
            )

            if(!is.null(linkingGroup)) {
                l_configure(plot,
                            linkingGroup = linkingGroup,
                            sync = sync)
            }

            class(plot) <- c("l_hist", class(plot))
            return(plot)

        } else {

            # convert all types of 'by' to a data frame
            byDeparse <- deparse(substitute(by))

            if(is.atomic(by)) {
                if(length(by) == n) {
                    by <- setNames(data.frame(by, stringsAsFactors = FALSE), byDeparse)
                } else {
                    stop("Unknown 'by' type")
                }
            } else {

                if(is.null(names(by))) {

                    by <- as.data.frame(by, stringsAsFactors = FALSE)
                    names(by) <- NULL
                } else {
                    by <- as.data.frame(by, stringsAsFactors = FALSE)
                }
            }

            plots <- loonFacets(type = "l_hist",
                                by,
                                args,
                                byDeparse = byDeparse,
                                layout = match.arg(layout),
                                connectedScales = match.arg(connectedScales),
                                by_args = Filter(Negate(is.null), by_args),
                                linkingGroup = linkingGroup,
                                sync = sync,
                                parent = parent,
                                factory_tclcmd = '::loon::histogram',
                                factory_path = 'hist',
                                factory_window_title = 'loon histogram',
                                yshows = yshows,
                                showStackedColors = showStackedColors,
                                origin = origin,
                                binwidth=binwidth,
                                showBinHandle = showBinHandle,
                                xlabel = xlabel,
                                ylabel = yshows)

            return(plots)

        }
    }
}

#' @export
l_hist.data.frame <- function(x,
                              yshows = c("frequency", "density"),
                              by = NULL,
                              layout = c("grid", "wrap", "separate"),
                              connectedScales = c("cross", "row", "column", "both", "x", "y", "none"),
                              showStackedColors = TRUE,
                              origin = NULL,
                              binwidth=NULL,
                              showBinHandle = FALSE,
                              color = "grey60",
                              active = TRUE,
                              selected = FALSE,
                              xlabel = NULL,
                              parent=NULL, ...) {

    if(missing(x))
        return(
            l_hist.default(x,
                           by = by,
                           layout = match.arg(layout),
                           connectedScales = match.arg(connectedScales),
                           yshows = yshows,
                           showStackedColors = showStackedColors,
                           origin = origin,
                           binwidth=binwidth,
                           showBinHandle = showBinHandle,
                           color = color,
                           active = active,
                           selected = selected,
                           xlabel = xlabel,
                           parent=parent, ...)
        )

    # get a relatively informative xlabel
    if (is.null(xlabel)){
        name <- colnames(x)
        if (is.null(name)) {
            name <- "column 1"
        }
        name <- name[1]
        dataname <- gsub("\"", "", deparse(substitute(x)))
        xlabel <- paste(name, "from", dataname)
    }
    x <- x[, 1]

    l_hist(x,
           by = by,
           layout = match.arg(layout),
           connectedScales = match.arg(connectedScales),
           yshows = yshows,
           showStackedColors = showStackedColors,
           origin = origin,
           binwidth=binwidth,
           showBinHandle = showBinHandle,
           color = color,
           active = active,
           selected = selected,
           xlabel = xlabel,
           parent=parent, ...)
}

#' @export
l_hist.matrix <- function(x,
                          yshows = c("frequency", "density"),
                          by = NULL,
                          layout = c("grid", "wrap", "separate"),
                          connectedScales = c("cross", "row", "column", "both", "x", "y", "none"),
                          showStackedColors = TRUE,
                          origin = NULL,
                          binwidth=NULL,
                          showBinHandle = FALSE,
                          color = l_getOption("color"),
                          active = TRUE,
                          selected = FALSE,
                          xlabel = NULL,
                          parent=NULL, ...) {

    l_hist(c(x),
           by = by,
           layout = match.arg(layout),
           connectedScales = match.arg(connectedScales),
           yshows = yshows,
           showStackedColors = showStackedColors,
           origin = origin,
           binwidth=binwidth,
           showBinHandle = showBinHandle,
           color = color,
           active = active,
           selected = selected,
           xlabel = xlabel,
           parent=parent, ...)
}

#' @export
l_hist.list <- function(x,
                        yshows = c("frequency", "density"),
                        by = NULL,
                        layout = c("grid", "wrap", "separate"),
                        connectedScales = c("cross", "row", "column", "both", "x", "y", "none"),
                        showStackedColors = TRUE,
                        origin = NULL,
                        binwidth=NULL,
                        showBinHandle = FALSE,
                        color = l_getOption("color"),
                        active = TRUE,
                        selected = FALSE,
                        xlabel = NULL,
                        parent=NULL, ...) {

    l_hist(unlist(x),
           by = by,
           layout = match.arg(layout),
           connectedScales = match.arg(connectedScales),
           yshows = yshows,
           showStackedColors = showStackedColors,
           origin = origin,
           binwidth=binwidth,
           showBinHandle = showBinHandle,
           color = color,
           active = active,
           selected = selected,
           xlabel = xlabel,
           parent=parent, ...)
}

#' @export
l_hist.table <- function(x,
                         yshows = c("frequency", "density"),
                         by = NULL,
                         layout = c("grid", "wrap", "separate"),
                         connectedScales = c("cross", "row", "column", "both", "x", "y", "none"),
                         showStackedColors = TRUE,
                         origin = NULL,
                         binwidth=NULL,
                         showBinHandle = FALSE,
                         color = l_getOption("color"),
                         active = TRUE,
                         selected = FALSE,
                         xlabel = NULL,
                         parent=NULL, ...) {

    dim_x <- dim(x)
    if(length(dim_x) > 2)
        stop(x,
             "should have at most two dimensions",
             call. = FALSE)
    else {
        if (is.null(xlabel)){
            name <- colnames(x)
            if (is.null(name)) {
                name <- "column 1"
            }
            name <- name[1]
            dataname <- gsub("\"", "", deparse(substitute(x)))
            xlabel <- paste(name, "from", dataname)
        }
        x <- x[,1]
    }

    l_hist(x,
           by = by,
           layout = match.arg(layout),
           connectedScales = match.arg(connectedScales),
           yshows = yshows,
           showStackedColors = showStackedColors,
           origin = origin,
           binwidth=binwidth,
           showBinHandle = showBinHandle,
           color = color,
           active = active,
           selected = selected,
           xlabel = xlabel,
           parent=parent, ...)
}

#' @export
l_hist.array <- function(x,
                         yshows = c("frequency", "density"),
                         by = NULL,
                         layout = c("grid", "wrap", "separate"),
                         connectedScales = c("cross", "row", "column", "both", "x", "y", "none"),
                         showStackedColors = TRUE,
                         origin = NULL,
                         binwidth=NULL,
                         showBinHandle = FALSE,
                         color = l_getOption("color"),
                         active = TRUE,
                         selected = FALSE,
                         xlabel = NULL,
                         parent=NULL, ...) {

    l_hist.table(x = x,
                 by = by,
                 layout = match.arg(layout),
                 connectedScales = match.arg(connectedScales),
                 yshows = yshows,
                 showStackedColors = showStackedColors,
                 origin = origin,
                 binwidth=binwidth,
                 showBinHandle = showBinHandle,
                 color = color,
                 active = active,
                 selected = selected,
                 xlabel = xlabel,
                 parent=parent, ...)
}
