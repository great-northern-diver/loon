from functools import singledispatch
from sys import exit 
import numpy as np
from .loon_class import loon, loon_l_compound
from .l_isLoonWidget import l_isLoonWidget
from .l_throwErrorIfNotLoonWidget import l_throwErrorIfNotLoonWidget

@singledispatch
def l_getPlots(target):
    '''For the target compound loon plot, determines all the loon plots in that compound plot.

    Args:
        param: target the (compound) loon  plot to be laid out.

    Returns: 
        a list of the named arguments and their values

    '''
    exit("l_getPlots.default has no valid inheritance; not an l_compound or loon plot")

@l_getPlots.register
def _(target: loon):
    if(l_isLoonWidget(target)):
        return [target]
    else:
        l_throwErrorIfNotLoonWidget(target)

@l_getPlots.register
def _(target: loon_l_compound):
    exit("l_getPlots.l_compound has no valid inheritance; needs to be specialized for each type of l_compound")


#' For the target compound loon plot, determines location (only and excluding the grobs)
#' arguments to pass to `gridExtra::arrangeGrob()`
#'
#' @param target the (compound) loon  plot whose locations are needed lay it out.
#' @return a list of an appropriate subset of the named location arguments
#' `c("ncol", "nrow", "layout_matrix", "heights", "widths")`.  There are as many heights and
#'  widths as there are plots returned by \code{l_getPlots()}; these specify
#'  the relative height and width of each plot in the display.  \code{layout_matrix}
#'  is an \code{nrow} by \code{ncol} matrix whose entries identify the location
#'  of each plot in \code{l_getPlots()} by their index.
#'
#' @examples
#'
#' if(interactive()) {
#'
#' pp <- l_pairs(iris, showHistograms = TRUE)
#' ll <- l_getLocations(pp)
#' nplots <- length(l_getPlots(pp))
#' # the plots returned by l_getPlots(pp) are positioned
#' # in order by the layout_matrix
#' # ll$layout_matrix TODO
#' }
#'
#' @export
@singledispatch
def l_getLocations(target):
    '''For the target compound loon plot, determines location (only and excluding the grobs) arguments to pass to `gridExtra::arrangeGrob()`
    
    '''
    exit("l_getLocations.default has no valid inheritance; not an l_compound or loon plot")

@l_getLocations.register
def _(target: loon_l_compound):
    exit("l_getLocations.l_compound  has no valid inheritance; needs to be specialized for each type of l_compound")

@l_getLocations.register
def _(target: loon):
    if(l_isLoonWidget(target)):
        np.matrix(1)
    else:
        l_throwErrorIfNotLoonWidget(target)


# #' @export
# l_cget.l_compound <- function(target, state) {

#     plotNames <- names(target)
#     plots <- lapply(plotNames,
#                     function(plotName) {
#                         target[[plotName]]

#                     })
#     values <- lapply(plots,
#                      function(plot, s) {
#                          if(s %in% l_state_names(plot)){
#                              l_cget(plot, s)
#                          } else {
#                              NA
#                          }
#                          }, state)
#     names(values) <- plotNames
#     values

# }


# #' @export
# l_configure.l_compound <- function(target, ...) {

#     args <- list(...)
#     states <- names(args)
#     if (is.null(states) || any("" %in% states))
#         stop("configuration needs key=value pairs")

#     plotNames <- names(target)
#     plots <- lapply(plotNames,
#                     function(plotName) {
#                         target[[plotName]]

#                     })
#     for (state in states) {

#         switch(
#             state,
#             linkingGroup = lapply(plots, l_configure,
#                                   linkingGroup = args$linkingGroup, sync = "pull"),
#             selected = stop("not implemented yet"),
#             stop("state ", state, " not implemented")
#         )
#     }

#     target
# }

# #' @export
# l_info_states.l_compound <- function(target, states = "all") {

#     plots <- l_getPlots(target)
#     plotNames <- names(plots)
#     values <- lapply(plots, l_info_states, states)
#     names(values) <- plotNames
#     values

# }
#' @export
# l_getPlots.l_navgraph <- function(target){
#     list(graph = target$graph, plot = target$plot)
# }