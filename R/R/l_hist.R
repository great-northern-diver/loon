
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
#'   If NULL \code{binwidth} is set using David Scott's rule when \code{x} is numeric and
#'   and using the minumum numerical difference between factor levels when \code{x}
#'   is a factor or a character vector.
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
    x <- as.numeric(x)
    if (is.null(origin) | !is.numeric(origin)) {
        origin <- min(x)}
    if (is.null(binwidth) | !is.numeric(binwidth)) {
        binwidth <- min(diff(sort(unique(x))))
    }
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


    } else
    {
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
    } else
    {

        yshows <- match.arg(yshows)
        if (is.null(xlabel)){
            xlabel <- gsub("\"", "", deparse(substitute(x)))
        }
        ## ylabel will be overwritten in ...
        if (is.null(origin) | !is.numeric(origin)){
            origin <- min(x)
            }

        if (is.null(binwidth) | !is.numeric(binwidth)) {
            n <- length(x)
            # Sturges rule
            # binwidth <- diff(range(x))/(1 + 3.322 * (log(n, base = 10)))
            # David Scott's rule
            binwidth <- 3.49 * sd(x)/(n ^(1/3))
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
