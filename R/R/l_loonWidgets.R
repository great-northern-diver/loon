
#' @title Get the set of basic path types for loon plots.
#'
#' @description Loon's plots are constructed in TCL and identified with
#' a path string appearing in the window containing the plot.
#' The path string begins with a unique identifier for the plot
#' and ends with a suffix describing the type of loon plot being displayed.
#'
#' The path identifying the plot is the string concatenation of both the
#' identifier and the type.
#'
#' This function returns the set of the base (non-compound) loon
#' path types.
#'
#' @return character vector of the base path types.
#'
#' @seealso \code{\link{l_compoundPaths}} \code{\link{l_getFromPath}} \code{\link{l_loonWidgets}}
#'
#' @export
#'
l_basePaths <- function(){
    c("plot", "hist", "plot3D", "serialaxes", "graph")
    # Does not appear in window title ->, "navgraph")
}

#' @title Get the set of basic path types for loon plots.
#'
#' @description Loon's plots are constructed in TCL and identified with
#' a path string appearing in the window containing the plot.
#' The path string begins with a unique identifier for the plot
#' and ends with a suffix describing the type of loon plot being displayed.
#'
#' The path identifying the plot is the string concatenation of both the
#' identifier and the type.
#'
#' This function returns the set of the loon
#' path types for compound loon plots.
#'
#' @return character vector of the compound path types.
#'
#' @seealso \code{\link{l_basePaths}}\code{\link{l_loonWidgets}} \code{\link{l_getFromPath}}
#'
#' @export
#'
l_compoundPaths <- function(){
    c("facet", "pairs", "ts")
}

l_path2class <- function(path) {

    if (path %in% l_basePaths()) {
        class <- c(paste0("l_", path), "loon")

    } else if (path %in% l_compoundPaths()) {
        class <- c(paste0("l_", path),
                   "l_compound",
                   "loon")

    } else class <- c()
    #
    # return
    class
}
#' @title Get all active top level loon plots.
#'
#' @description Loon's plots are constructed in TCL and identified with
#' a path string appearing in the window containing the plot.
#'
#' If the plots were not saved on a variable, this function will
#' look for all loon plots displayed and return their values in a list whose
#' elements may then be assigned to R variables.
#'
#' @param pathTypes an optional argument identifying the collection of path types that
#' are to be returned (if displayed).
#'
#' This must be a subset of the union of
#' \code{\link{l_basePaths}()} and \code{\link{l_compoundPaths}()}.
#'
#' If it is missing,
#' all \code{\link{l_basePaths}()} and \code{\link{l_compoundPaths}()} will be returned.
#'
#'
#' @return list whose elements are named by, and contain the values of, the
#' loon plot widgets.  The list can be nested when loon plots (like \code{\link{l_pairs}})
#' are compound in that they consist of more than one base loon plot.
#'
#' @seealso \code{\link{l_basePaths}}\code{\link{l_compoundPaths}} \code{\link{l_getFromPath}}
#'
#' @export
#'
#' @examples
#' if(interactive()){
#' l_plot(iris)
#' l_hist(iris)
#' l_hist(mtcars)
#' l_pairs(iris)
#' # The following will not be loonWidgets (neither is the inspector)
#' tt <- tktoplevel()
#' tkpack(l1 <- tklabel(tt, text = "Heave"), l2<- tklabel(tt, text = "Ho"))
#' #
#' # This will return loon widgets corresponding to plots
#' loonPlots <- l_loonWidgets()
#' names(loonPlots)
#' firstPlot <- loonPlots[[1]]
#' firstPlot["color"] <- "red"
#' histograms <- l_loonWidgets("hist")
#' lapply(histograms,
#'        FUN  = function(hist) {
#'                 hist["binwidth"] <- hist["binwidth"]/2
#'                 l_scaleto_world(hist)
#'              }
#'              )
#' }
#'
l_loonWidgets <- function(pathTypes) {

    #
    # Internal helper function
    #
    create_handles <- function(target, plots, type) {
        i <- 0
        hasRecognized <- TRUE
        while(hasRecognized) {

            path <- compound_path(target = target, type = type, index = i)

            hasRecognized <- l_isLoonWidget(path)
            i <- i + 1
            if(hasRecognized) {
                plots[[length(plots) + 1]] <- l_create_handle(path)
            }
        }
        return(plots)
    }

    #
    # Legitimate loon paths to look over
    #
    loonBasePaths <- l_basePaths()
    loonCompoundPaths <-  l_compoundPaths()

    #
    # Make sure pathTypes are legit
    #
    if (!missing(pathTypes)) {
        loonBasePaths <- intersect(loonBasePaths, pathTypes)
        loonCompoundPaths <- intersect(loonCompoundPaths, pathTypes)

        nonLoonPaths <- setdiff(pathTypes, c(loonBasePaths, loonCompoundPaths))
        #
        # Warn user about unknown path types
        #
        if (length(nonLoonPaths) > 0) {
            warning(paste0(nonLoonPaths,
                           " is not a path extension in loonPaths(). \n"))
        }
    }

    #
    # Get all the top most widgets from tcl
    #
    widgets <-  as.character(tcl("loon::listTopWidgets"))
    #
    # Collect up the loon plots
    #
    loonPlots <- c()
    #
    # Easy ones
    #
    for (path in loonBasePaths) {
        for (w in widgets) {
            w_loon <- paste0(w, ".", path)
            if (l_isLoonWidget(w_loon)) {
                class(w_loon)  <- l_path2class(path)
                loonPlots[[w_loon]] <- w_loon
            }
        }
    }
    #
    # Compound ones are more complicated since they
    # are really a list of a lot of separate base plots
    #
    # Haven't really considered the case where
    # one of the list might itself be a compound plot.
    #
    for (c_path in loonCompoundPaths) {
        for (w in widgets) {
            root_path <- paste0(w, ".", c_path)
            plotList <- list()
            for (plot_path in loonBasePaths) {
                plotList <- create_handles(target = root_path,
                                           plots = plotList,
                                           type = plot_path)
            }
            #
            # Compound widgets contain lots of plots
            #
            nPlots <- length(plotList)
            #
            if (nPlots > 0){
                if (nPlots == 1) {
                    loonPlot <- plotList[[1]]
                } else {
                    loonPlot <- plotList
                    class(loonPlot)  <- l_path2class(c_path)
                }
                loonPlots[[root_path]] <- loonPlot
            }


        }
    }
    loonPlots
}
