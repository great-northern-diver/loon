
#' @title Create an Interactive Histogram
#'
#' @description Create an interactive histogram display that can be linked with
#'   loon's other displays
#'
#' @param x vector with numerical data to perform the binning on
#' @param origin scalar to define the binning origin
#' @param binwidth scalar to specify the binwidth, if NULL then it is set to
#'   \code{diff(range(x))/30} if that value is >= 0.0001 or 0.0001 otherwise
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




l_hist <-  function(x, origin = min(x), binwidth=NULL, parent=NULL, ...) {


    if(missing(x)) {

        plot <- loonPlotFactory('::loon::histogram', 'hist', 'loon histogram', parent, ...)

    } else {

        if (!missing(x))
            xlabel <- gsub("\"", "", deparse(substitute(x)))

        ## xlab and ylab will be overwritten in they
        ## are defined in ...
        if (missing(origin))
            origin <- min(x)

        if (is.null(binwidth)) {
            binwidth <- diff(range(x))/30
            if (binwidth < 0.0001)
                binwidth <- 0.0001
        }

        plot <- loonPlotFactory('::loon::histogram', 'hist', 'loon histogram', parent,
                                x = x,
                                origin=origin,
                                binwidth=binwidth,
                                xlabel=xlabel,
                                ...)

        class(plot) <- c("l_hist", class(plot))
    }


    return(plot)
}
