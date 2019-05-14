
#' @title A generic function to transfer the values of the states of one `loon` structure to another.
#'
#' @description \code{l_copyStates} reads the values of the states of the `source` and
#' assigns them to the states of the same name on the `target`.
#'
#' @param source the `loon` object providing the values of the states.
#' @param target the `loon` object whose states are assigned the
#' values of the `sources` states of the same name.
#' @param states a character vector of the states to be copied. If `NULL` (the default),
#' then all states in common (excluding those identified by exclusion parameters)
#' are copied from the `source` to the `target`.
#' @param exclude a character vector naming those common states to be excluded
#' from copying.  Default is NULL.
#' @param excludeBasicStates  a logical indicating whether certain basic states
#' are to be excluded from the copy (if `TRUE`, the default).
#' These states include those derived from data variables (like
#' "x", "xTemp", "zoomX", "panX", "deltaX", "xlabel", and the "y" counterparts)
#' since these values determine coordinates in the plot and so are typically not to be copied.
#' Similarly "swapAxes" is one of these basic states because in \code{l_compound} plots such
#' as \code{l_pairs()} swapping axes can wreak havoc if unintended.
#' Finally, an important pair of basic states to exclude are
#' "linkingKey" and "linkingGroup" since such changes  require proper
#' synchronization.
#'
#' Setting `excludeBasicStates = TRUE` is a simple way to avoid copying the values of these
#' basic states.
#' Setting `excludeBasicStates = FALSE` will allow these to be copied as well.
#' @param returnNames a logical to indicate whether to return the names
#' of all states successfully copied for all plots.  Default is `FALSE`
#'
#' @return a character vector of the names of the states successfully copied
#' (for each plot whose states were affected), or
#' NULL if none were copied or `returnNames == FALSE`.
#'
#' @seealso \code{\link{l_saveStatesRDS}} \code{\link{l_info_states}} \code{\link{saveRDS}}
#'
#' @export
#'
#' @examples
#' if(interactive()){
#' # Source and target are `l_plots`
#'    p <- with(iris,
#'          l_plot(x = Sepal.Width, y = Petal.Width,
#'                 color = Species, glyph = "ccircle",
#'                 size = 10, showGuides = TRUE,
#'                 title = "Edgar Anderson's Iris data"
#'                )
#'            )
#'
#'    p2 <- with(iris,
#'           l_plot(x = Sepal.Length, y = Petal.Length,
#'                  title = "Fisher's Iris data"
#'                  )
#'               )
#' # Copy the states of p to p2
#' # First just the size and title
#'    l_copyStates(source = p, target = p2,
#'                 states = c("size", "title")
#'                 )
#' # Copy all but those associated with the variables
#'    l_copyStates(source = p, target = p2)
#'
#' # Suppose p had a linkingGroup, say "Edgar"
#'    l_configure(p, linkingGroup = "Edgar", sync = "push")
#'
#' # To force this linkingGroup to be copied to a new plot
#'    p3 <- with(iris,
#'           l_plot(x = Sepal.Length, y = Petal.Length,
#'                  title = "Fisher's Iris data"
#'                  )
#'               )
#'    l_copyStates(source = p, target = p3,
#'                 states = c("linkingGroup"),
#'                 # To allow this to happen:
#'                 excludeBasicStates = FALSE
#'                 )
#'
#'    h <- with(iris,
#'              l_hist((Petal.Width * Petal.Length),
#'                     showStackedColors = TRUE,
#'                     yshows = "density")
#'                     )
#'    l_copyStates(source = p, target = h)
#'
#'    sa <- l_serialaxes(iris, axes = "parallel")
#'    l_copyStates(p, sa)
#'
#'    pp <- l_pairs(iris, showHistograms = TRUE)
#'    l_copyStates(p, pp)  # makes no copy (not one to one)
#'
#'    pp2 <- l_pairs(iris,
#'                   color = iris$Species,
#'                   showGuides = TRUE,
#'                   title ="Iris data",
#'                   glyph = "ctriangle")
#'    l_copyStates(pp2, pp)
#'    l_copyStates(pp2, p)
#' }
#'
l_copyStates <- function(source, target,
                         states = NULL,
                         exclude = NULL,
                         excludeBasicStates = TRUE,
                         returnNames = FALSE) {
    UseMethod("l_copyStates")
}


#' @export
l_copyStates.l_savedStates <- function(source,
                                       target,
                                       states = NULL,
                                       exclude = NULL,
                                       excludeBasicStates = TRUE,
                                       returnNames = FALSE) {
    if (identical(attr(source, "l_plot_class"),
                  class(target))) {
        if (is(target, "l_compound")) {
            Map(
                function(source_i, target_i){
                    l_copyStates(source_i,
                                 target_i,
                                 states = states,
                                 exclude = exclude,
                                 excludeBasicStates = excludeBasicStates,
                                 returnNames = returnNames)
                },
                source,
                l_getPlots(target)
            )
        } else {
            common_states <- intersect(names(source),
                                       names(target))
            if (is.null(states)) {
                states <- common_states
            } else {
                states <- intersect(states,
                                    common_states)
            }
            if(excludeBasicStates) {
                exclude <- union(exclude,
                                 c("x", "xTemp", "zoomX", "panX", "deltaX", "xlabel",
                                   "y", "yTemp", "zoomY", "panY", "deltaY", "ylabel",
                                   "linkingKey", "linkingGroup", "swapAxes")
                )
            }
            if (!is.null(exclude)) {
                states <- setdiff(states, exclude)
            }

            # Check on linking info.
            # If present, then have to use l_configure() with "sync" as well
            if ("linkingKey" %in% states) {
                states <- setdiff(states, "linkingKey")
                l_configure(target, linkingKey = source["linkingKey"], sync = "pull")
            }
            if ("linkingGroup" %in% states) {
                states <- setdiff(states, "linkingGroup")
                l_configure(target, linkingGroup = source["linkingGroup"], sync = "pull")
            }
            # Now do the rest of the states
            lapply(states,
                   function (state){
                       target[state] <- source[[state]]
                   })
        }
    } else{
        stop("l_plot_class of l_savedStates `source` does not match `class(target)`")
    }
    if (!returnNames |(length(states) == 0)) NULL else states
}


#' @export
l_copyStates.l_compound <- function(source,
                                    target,
                                    states = NULL,
                                    exclude = NULL,
                                    excludeBasicStates = TRUE,
                                    returnNames = FALSE) {

    if (is(target, "l_compound")){
        if (identical(class(source), class(target))) {
            source_plots <- l_getPlots(source)
            target_plots <- l_getPlots(target)
            common_plot_names <- intersect(names(source_plots), names(target_plots))
            returnList <- Map(
                function(plotname){
                    l_copyStates(source_plots[plotname][[1]],
                                 target_plots[plotname][[1]],
                                 states = states,
                                 exclude = exclude,
                                 excludeBasicStates = excludeBasicStates,
                                 returnNames = returnNames)
                },
                common_plot_names
            )
            if (!returnNames) NULL else returnList

        } else {
            # copy the states of the source l_compound
            # to each of the plots in the target l_compound
            returnList <- Map(
                function(target_i){
                    l_copyStates(source,
                                 target_i,
                                 states = states,
                                 exclude = exclude,
                                 excludeBasicStates = excludeBasicStates,
                                 returnNames = returnNames)
                },
                l_getPlots(target)
            )
            if (!returnNames) NULL else returnList
        }
    } else {
        # target is not an l_compound
        # Want to map all named states from every plot of
        # the source l_compound
        # to the target
        returnList <- Map(
            function(source_i){
                l_copyStates(source_i,
                             target,
                             states = states,
                             exclude = exclude,
                             excludeBasicStates = excludeBasicStates,
                             returnNames = returnNames)
            },
            l_getPlots(source)
        )
        if (!returnNames) NULL else returnList
    }
}



#' @export
l_copyStates.loon <- function(source,
                              target,
                              states = NULL,
                              exclude = NULL,
                              excludeBasicStates = TRUE,
                              returnNames = FALSE) {
    if (is(target, "l_compound")){
        # copy the states of the source
        # to each of the plots in the target l_compound
        returnList <- Map(
            function(target_i){
                l_copyStates(source,
                             target_i,
                             states = states,
                             exclude = exclude,
                             excludeBasicStates = excludeBasicStates,
                             returnNames = returnNames)
            },
            l_getPlots(target)
        )
        if (!returnNames) NULL else returnList
    } else {

        if (is(target, "loon")){
            if (excludeBasicStates) {
                exclude <- union(exclude,
                                 c("x", "xTemp", "zoomX", "panX", "deltaX", "xlabel",
                                   "y", "yTemp", "zoomY", "panY", "deltaY", "ylabel",
                                   "linkingKey", "linkingGroup", "swapAxes")
                )
            }

            common_states <- intersect(names(source), names(target))
            if (is.null(states)) {
                states <- common_states
            } else {
                states <- intersect(states, common_states)
            }
            if (!is.null(states)){
                if (!is.null(exclude)) {
                    states <- setdiff(states, exclude)
                }
                # Check on linking info.
                # If present, then have to use l_configure() with "sync" as well
                if ("linkingKey" %in% states) {
                    states <- setdiff(states, "linkingKey")
                    l_configure(target, linkingKey = source["linkingKey"], sync = "pull")
                }
                if ("linkingGroup" %in% states) {
                    states <- setdiff(states, "linkingGroup")
                    l_configure(target, linkingGroup = source["linkingGroup"], sync = "pull")
                }
                # Now do the rest of the states
                lapply(states,
                       function (state){
                           target[state] <- source[state]
                       })
            } else warning("No effect since no states are shared")
        } else {
            stop(paste0("`target` must also be of class `loon`",
                        "\n Not class: ",
                        class(target)))
        }
    }
    if (!returnNames |(length(states) == 0)) NULL else states
}

