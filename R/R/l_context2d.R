

l_context <- function(navigator, ...) {
    obj_eval <- .loonobject(navigator)
    obj_eval('context', ...)
}

#' @title Delete a context from a navigator
#'
#' @description Navigators can have multiple contexts. This function removes a
#'   context from a navigator.
#'
#' @param navigator navigator hanlde
#' @param id context id
#'
#' @templateVar page  learn_R_display_graph
#' @templateVar section contexts
#' @template see_l_help
#'
#' @seealso \code{\link{l_context_ids}}, \code{\link{l_context_add_context2d}},
#'   \code{\link{l_context_add_geodesic2d}},
#'   \code{\link{l_context_add_slicing2d}}, \code{\link{l_context_getLabel}},
#'   \code{\link{l_context_relabel}}
#'
#' @export
l_context_delete <- function(navigator, id) {
    l_navigator(navigator, "delete", id)
}

#' @title List context ids of a navigator
#'
#' @description Navigators can have multiple contexts. This function list the
#'   context ids of a navigator.
#'
#' @inheritParams l_context_delete
#'
#' @templateVar page  learn_R_display_graph
#' @templateVar section contexts
#' @template see_l_help
#'
#' @seealso \code{\link{l_context_delete}},
#'   \code{\link{l_context_add_context2d}},
#'   \code{\link{l_context_add_geodesic2d}},
#'   \code{\link{l_context_add_slicing2d}}, \code{\link{l_context_getLabel}},
#'   \code{\link{l_context_relabel}}
#'
#'
#' @export
l_context_ids <- function(navigator) {
    l_navigator(navigator, "ids")
}

#' @title Change the label of a context
#'
#' @description Context labels are eventually used in the context inspector.
#'   This function relabels a context.
#'
#' @inheritParams l_context_delete
#' @param label context label shown
#'
#' @templateVar page  learn_R_display_graph
#' @templateVar section contexts
#' @template see_l_help
#'
#' @seealso \code{\link{l_context_getLabel}},
#'   \code{\link{l_context_add_context2d}},
#'   \code{\link{l_context_add_geodesic2d}}, \code{\link{l_context_add_slicing2d}},
#'   \code{\link{l_context_delete}}
#'
#' @export
l_context_relabel <- function(navigator, id, label) {
    l_navigator(navigator, "relabel", id, label)
    invisible()
}

#' @title Query the label of a context
#'
#' @description Context labels are eventually used in the context inspector.
#'   This function queries the label of a context.
#'
#' @inheritParams l_context_delete
#'
#' @templateVar page  learn_R_display_graph
#' @templateVar section contexts
#' @template see_l_help
#'
#' @seealso \code{\link{l_context_getLabel}},
#'   \code{\link{l_context_add_context2d}},
#'   \code{\link{l_context_add_geodesic2d}},
#'   \code{\link{l_context_add_slicing2d}}, \code{\link{l_context_delete}}
#'
#' @export
l_context_getLabel <- function(navigator, id) {
    paste(l_navigator(navigator, "getLabel", id), collapse=' ')
}

l_context_add <- function(navigator, type, ...) {

    obj_eval <- .loonobject(navigator)

    structure(obj_eval('context', 'add', type, ...),
              widget=attr(navigator,'widget'),
              navigator=as.vector(navigator),
              class=c('l_context', 'loon'))
}

#' @title Create a context2d navigator context
#'
#' @description A context2d maps every location on a 2d space graph to a list of
#'   xvars and a list of yvars such that, while moving the navigator along the
#'   graph, as few changes as possible take place in xvars and yvars.
#'
#' @template details_contexts
#'
#' @param navigator navigator handle object
#' @param ... arguments passed on to modify context states
#'
#' @return context handle
#'
#' @seealso \code{\link{l_info_states}}, \code{\link{l_context_ids}},
#'   \code{\link{l_context_add_geodesic2d}},
#'   \code{\link{l_context_add_slicing2d}}, \code{\link{l_context_getLabel}},
#'   \code{\link{l_context_relabel}}
#'
#'
#' @export
l_context_add_context2d <- function(navigator, ...) {
    l_context_add(navigator, 'context2d', ...)
}

#' @title Create a geodesic2d navigator context
#'
#' @description Geodesic2d maps every location on the graph as an orthogonal
#'   projection of the data onto a two-dimensional subspace. The nodes then
#'   represent the sub-space spanned by a pair of variates and the edges either
#'   a 3d- or 4d-transition of one scatterplot into another, depending on how
#'   many variates the two nodes connected by the edge share (see Hurley and
#'   Oldford 2011). The geodesic2d context inherits from the context2d context.
#'
#' @template details_contexts
#'
#' @inheritParams l_context_add_context2d
#'
#' @return context handle
#'
#' @seealso \code{\link{l_info_states}}, \code{\link{l_context_ids}},
#'   \code{\link{l_context_add_context2d}},
#'   \code{\link{l_context_add_slicing2d}}, \code{\link{l_context_getLabel}},
#'   \code{\link{l_context_relabel}}
#'
#'
#' @export
l_context_add_geodesic2d <- function(navigator, ...) {

    args <- list(navigator, "geodesic2d", ...)

    if (!is.null(args[['data']])) {
        args$data <- l_data(args$data)
    }

    g2d <- do.call('l_context_add', args)

    # add plot handle
    if(is.null(args[['command']])) {
        attr(g2d, 'plot') <- structure(
            strsplit(g2d['command'],' ')[[1]][1],
            class = 'loon')
    }

    g2d
}


#' @title Create a slicind2d navigator context
#'
#' @description The slicing2d context implements slicing using navigation graphs
#'   and a scatterplot to condition on one or two variables.
#'
#' @template details_contexts
#'
#' @inheritParams l_context_add_context2d
#'
#' @return context handle
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' names(oliveAcids) <- c('p','p1','s','o','l','l1','a','e')
#' nodes <- apply(combn(names(oliveAcids),2),2,
#'               function(x)paste(x, collapse=':'))
#' G <- completegraph(nodes)
#' g <- l_graph(G)
#' nav <- l_navigator_add(g)
#' con <- l_context_add_slicing2d(nav, data=oliveAcids)
#'
#' # symmetric range proportion around nav['proportion']
#' con['proportion'] <- 0.2
#'
#' con['conditioning4d'] <- "union"
#' con['conditioning4d'] <- "intersection"
#' }
#'
l_context_add_slicing2d <- function(navigator, ...) {

    args <- list(navigator, "slicing2d", ...)

    if (!is.null(args[['data']])) {
        args$data <- l_data(args$data)
    }

    con <- do.call(`l_context_add`, args)

    attr(con, 'plot_xy')  <- structure(con['plot_xy'], class='loon')
    attr(con, 'plot_uv')  <- structure(con['plot_uv'], class='loon')


    con
}

