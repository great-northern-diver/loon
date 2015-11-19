#' @export
l_context <- function(navigator, ...) {
    obj_eval <- .loonobject(navigator)
    obj_eval('context', ...)
}

#' @export
l_context_delete <- function(navigator, id) {
    l_navigator(navigator, "delete", id)
}

#' @export
l_context_ids <- function(navigator) {
    l_navigator(navigator, "ids")
}

#' @export
l_context_relabel <- function(navigator, id, label) {
    l_navigator(navigator, "relabel", id, label)
    invisible()
}

#' @export
l_context_getLabel <- function(navigator, id) {
    paste(l_navigator(navigator, "getLabel", id), collapse=' ')
}


#' @export
l_context_add <- function(navigator, type, ...) {

    obj_eval <- .loonobject(navigator)
    
    structure(obj_eval('context', 'add', type, ...),
              widget=attr(navigator,'widget'),
              navigator=as.vector(navigator),
              class=c('loon','l_context'))
}

#' @export
l_context_add_context2d <- function(navigator, ...) {
    l_context_add(navigator, 'context2d', ...)
}

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


#' Slicing Context
#' @export
#' 
#' @examples 
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
#' 
l_context_add_slicing2d <- function(navigator, ...) {
    
    args <- list(navigator, "slicing2d", ...)
    
    if (!is.null(args[['data']])) {
        args$data <- l_data(args$data)
    }
    
    con <- do.call('l_context_add', args)

    attr(con, 'plot_xy')  <- structure(con['plot_xy'], class='loon')
    attr(con, 'plot_uv')  <- structure(con['plot_uv'], class='loon')
    
        
    con    
}

