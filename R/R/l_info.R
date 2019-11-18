
#' @title Retrieve Information about the States of a Loon Widget
#' @family loon interactive states
#' @description Loon's built-in object documentation. Can be used with every
#'   loon object that has plot states including plots, layers, navigators,
#'   contexts.  This is a generic function.
#'
#' @template param_target
#' @param states vector with names of states. \code{'all'} is treated as a
#'   keyword and results in returning information on all plot states
#'
#' @return a named nested list with one element per state. The list elements are
#'   also named lists with \code{type}, \code{dimension}, \code{defaultvalue},
#'   and \code{description} elements containing the respective information.
#'
#' @export
#'
#' @examples
#' p <- l_plot(iris, linkingGroup="iris")
#' i <- l_info_states(p)
#' names(p)
#' names(i)
#' i$selectBy
#'
#' l <- l_layer_rectangle(p, x=range(iris[,1]), y=range(iris[,2]), color="")
#' l_info_states(l)
#'
#'
#' h <- l_hist(iris$Sepal.Length, linkingGroup="iris")
#' l_info_states(h)
l_info_states <- function(target, states='all') {
    UseMethod("l_info_states", target)
}


#' @export
l_info_states.loon <- function(target, states='all') {
    obj_eval <- .loonobject(target, function(tclvalue) {
        sapply(as.character(tcl('dict', 'keys', tclvalue)),
               FUN=function(var){
                   sapply(c("type", "dimension", "defaultvalue", "description"),
                          function(field) {
                              v <- as.character(tcl('dict','get', tclvalue, var, field))
                              if (length(v) == 0)
                                  v <- ""

                              if (field=="description")
                                  v <- paste(v, collapse=' ')

                              v
                          }, simplify=FALSE)
               }, simplify=FALSE, USE.NAMES=TRUE)
    })

    if (length(states) == 1 && states == 'all') {
        result <- obj_eval('info', 'states')
    } else {
        result <-  obj_eval('info', 'states', states)
    }

    return(result)
}

