#' @title Generic funtction to create an interactive graph display
#' @name l_graph
#' @description Interactive graphs in loon are currently most often used for
#'   navigation graphs.
#'
#' @param nodes object for method dispatch
#' @param ... arguments passed on to methods
#'
#' @return graph handle
#'
#' @templateVar page  learn_R_display_graph
#' @templateVar section graph
#' @template see_l_help
#'
#' @seealso Other related graph objects, \code{\link{loongraph}},
#' \code{\link{completegraph}}, \code{\link{linegraph}},
#' \code{\link{complement}}, \code{\link{as.graph}}
#'
#' @export
#'
l_graph <- function(nodes, ...) {
    UseMethod("l_graph")
}


#' @rdname l_graph
#' @export
l_graph.graph <- function(nodes, ...) {
    l_graph.loongraph(as.loongraph(nodes), ...)
}


#' @rdname l_graph
#' @export
l_graph.loongraph <- function(nodes,...) {

    graph <- nodes

    dotArgs <- list(...)
    dotArgs$nodes <- graph$nodes

    if(is.null(dotArgs$isDirected)) {
        dotArgs$isDirected <- graph$isDirected
    }
    if(is.null(dotArgs$from)) {
        dotArgs$from <- graph$from
    }
    if(is.null(dotArgs$to)) {
        dotArgs$to <- graph$to
    }

    do.call(l_graph.default, dotArgs)
}


#' @rdname l_graph
#' @param from vector with node names of the from-to pairs for edges
#' @param to vector with node names of the from-to pairs for edges
#' @param isDirected a boolean state to specify whether these edges have directions
#' @param parent parent widget of graph display
#' @export
#'
#' @seealso Advanced usage \code{\link{l_navgraph}},
#' \code{\link{l_ng_plots}}, \code{\link{l_ng_ranges}}
#'
#' @examples
#' if(interactive()) {
#'  G <- completegraph(nodes=names(iris))
#'  LG <- linegraph(G, sep=":")
#'  g <- l_graph(LG)
#' }

l_graph.default <- function(nodes="", from="", to="",  isDirected = FALSE,
                            parent=NULL, ...) {

    dotArgs <- list(...)

    l_className <- "l_graph"
    call <- match.call()
    modifiedLinkedStates <- l_modifiedLinkedStates(l_className, names(call))

    # `sync` and `linkingGroup` are set after the plot is created
    # reason: set aesthetics first, then pull aesthetics from other plots (if they exist)
    linkingGroup <- dotArgs[["linkingGroup"]]
    dotArgs$linkingGroup <- NULL
    sync <- dotArgs[["sync"]]
    # if null, it is always **pull**
    if(is.null(sync)) sync <- "pull"
    dotArgs$sync <- NULL

    plot <- do.call(
        loonPlotFactory,
        c(
            dotArgs,
            list(factory_tclcmd = '::loon::graph',
                 factory_path = 'graph',
                 factory_window_title = 'loon graph',
                 parent = parent,
                 nodes = na.omit(as.character(nodes)),
                 from = na.omit(as.character(from)),
                 to = na.omit(as.character(to)),
                 isDirected = isDirected)
        )
    )

    if(!is.null(linkingGroup)) {

        syncTemp <- ifelse(length(modifiedLinkedStates) == 0,  sync, "pull")
        if(syncTemp == "push")
            message("The modification of linked states is not detected",
                    " so that the default settings will be pushed to all plots")
        # configure plot (linking)
        l_configure(plot,
                    linkingGroup = linkingGroup,
                    sync = syncTemp)

        if(sync == "push" && length(modifiedLinkedStates) > 0) {

            do.call(l_configure,
                    c(
                        list(
                            target = plot,
                            linkingGroup = linkingGroup,
                            sync = sync
                        ),
                        dotArgs[modifiedLinkedStates]
                    )
            )
        } else {
            l_linkingWarning(plot, sync, args = dotArgs,
                             modifiedLinkedStates = modifiedLinkedStates,
                             l_className = l_className)
        }
    }

    class(plot) <- c(l_className, class(plot))

    plot
}

