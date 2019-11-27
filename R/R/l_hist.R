
#' @title Create an interactive histogram
#'
#' @description \code{l_hist} is a generic function for creating an interactive histogram display that can be linked with
#'   loon's other displays
#'
#' @param x vector with numerical data to perform the binning onx,
#' @param yshows one of "frequency" (default) or  "density"
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
#' @param xlabel label to be used on the horizontal axis. If NULL, an attempt at a meaningful label
#'   inferred from \code{x} will be made.
#' @template param_parent
#' @param ... named arguments to modify the histogram plot states
#'
#' @details Note that when changing the \code{yshows} state form
#'   \code{'frequency'} to \code{'density'} you might have to use
#'   \code{\link{l_scaleto_world}} to show the complete histogram in the plotting
#'   region.
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

l_hist <- function(x,
                   yshows = c("frequency", "density"),
                   showStackedColors = TRUE,
                   origin = NULL,
                   binwidth=NULL,
                   showBinHandle = FALSE,
                   xlabel = NULL,
                   parent=NULL, ...) {
    UseMethod("l_hist")
}

#' @export
l_hist.factor <-  function(x,
                           yshows = c("frequency", "density"),
                           showStackedColors = TRUE,
                           origin = NULL,
                           binwidth=NULL,
                           showBinHandle = FALSE,
                           xlabel = NULL,
                           parent=NULL, ...) {

    if (is.null(xlabel)){
        xlabel <-  gsub("\"", "", deparse(substitute(x)))
    }
    levelNames <- levels(x)
    nlevels <- length(levelNames)
    x <-  unclass(x)  # Get the level numbers as numeric values
    if (is.null(origin) | !is.numeric(origin)) {
        origin <- min(x)}
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
                 showStackedColors = showStackedColors,
                 origin = origin,
                 binwidth=binwidth,
                 showBinHandle = showBinHandle,
                 xlabel = xlabel,
                 parent=parent, ...)

    # Add level names to plot
    ## Adjust text coords
    ## The reason to do so is to make sure that
    ## `labels` always lay down the corresponding bins no matter how origin shifts
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
    h
}


#' @export
l_hist.character <-  function(x,
                              yshows = c("frequency", "density"),
                              showStackedColors = TRUE,
                              origin = NULL,
                              binwidth = NULL,
                              showBinHandle = FALSE,
                              xlabel = NULL,
                              parent=NULL, ...) {
    x <- factor(x)

    l_hist(x,
           yshows = yshows,
           showStackedColors = showStackedColors,
           origin = origin,
           binwidth=binwidth,
           showBinHandle = showBinHandle,
           xlabel = xlabel,
           parent=parent, ...)
}

#' @export
l_hist.default <-  function(x,
                            yshows = c("frequency", "density"),
                            showStackedColors = TRUE,
                            origin = NULL,
                            binwidth = NULL,
                            showBinHandle = FALSE,
                            xlabel = NULL,
                            parent = NULL,
                            ...) {


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
        plot <- loonPlotFactory('::loon::histogram', 'hist', 'loon histogram',
                                parent,
                                yshows = yshows,
                                showStackedColors = showStackedColors,
                                origin = origin,
                                binwidth = binwidth,
                                showBinHandle = showBinHandle,
                                xlabel = xlabel,
                                ...)
        class(plot) <- c("l_hist", class(plot))


    } else {
        ndims <- length(dim(x))
        if (ndims > 2) stop("x should have at most two dimensions")
        if (length(dim(x)) == 2) {
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
            x <- x[,1]

            plot <- l_hist(x,
                           yshows = yshows,
                           showStackedColors = showStackedColors,
                           origin = origin,
                           binwidth=binwidth,
                           showBinHandle = showBinHandle,
                           xlabel = xlabel,
                           parent=parent,
                           ...)
        } else {

            yshows <- match.arg(yshows)
            if (is.null(xlabel)){
                xlabel <- gsub("\"", "", deparse(substitute(x))) }
            ## ylabel will be overwritten in ...
            if (is.null(origin) | !is.numeric(origin)){
                origin <- min(x)
            }

            if (is.null(binwidth) | !is.numeric(binwidth)) {
                n <- length(x)
                # Sturges rule
                # binwidth <- diff(range(x))/(1 + 3.322 * (log(n, base = 10)))
                # David Scott's rule
                sd <- sd(x)
                binwidth <- if (sd == 0) {1} else  {3.49 * sd/(n ^(1/3))}

            }

            plot <- loonPlotFactory('::loon::histogram',
                                    'hist',
                                    'loon histogram',
                                    parent,
                                    x = x,
                                    yshows = yshows,
                                    showStackedColors = showStackedColors,
                                    origin = origin,
                                    binwidth=binwidth,
                                    showBinHandle = showBinHandle,
                                    xlabel = xlabel,
                                    ...)
            class(plot) <- c("l_hist", class(plot))

        }
    }
    return(plot)
}


