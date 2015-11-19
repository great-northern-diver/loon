
#' @export
l_graphswitch <- function(activewidget="", parent=NULL, ...) {

    new.toplevel <- FALSE
    if(is.null(parent)) {
        new.toplevel <- TRUE
        parent <- l_toplevel()
    }

    child <- l_subwin(parent, 'graphswitch')

            
    widget <- try(tcl('::loon::graphswitch', child,
                      activewidget=activewidget, ...))
      
    
    if(is(widget, 'try-error')) {
        if(new.toplevel) tkdestroy(parent)
        stop("graphswitch could not be created.")
    }
    
    widget <- as.character(widget)
    
    if(new.toplevel) {
        tkpack(widget, fill="both", expand=TRUE)
        tktitle(parent) <- paste("loon graphswitch", widget)
    }
    
    class(widget) <- "loon"

    return(widget)
}

#' @export
l_graphswitch_add <- function(widget, graph, ...) {
    UseMethod("l_graphswitch_add", graph)
} 

.index4R <- function(index) {
    if (is.numeric(index)) {
        index <- index - 1
    }
    return(index)
}


#' @export
l_graphswitch_add.default <- function(widget, graph, from, to, isDirected,
                                      label="", index="end") {
    nodes <- graph
    #print("add graph to switch")
    #print(paste( widget, 'add', label, nodes, from, to, isDirected, pos))

    if (length(from) == 0) {
        from <- vector()
        to <- vector()
    }
    
    call <- paste0(tcl('info', 'object', 'namespace', widget),'::my')
    
    as.character(tcl(call, 'AddExternal', nodes, from, to, isDirected,
                     label, .index4R(index)))
} 


#' @export
l_graphswitch_add.loongraph <- function(widget, graph, label="", index='end') {
    l_graphswitch_add.default(widget, graph$nodes, label=label, 
                              graph$from, graph$to, graph$isDirected, .index4R(index))    
}

#' @export
l_graphswitch_add.graph <- function(widget, graph, label="", index='end') {
    l_graphswitch_add.loongraph(widget, as.loongraph(graph), label, .index4R(index))
}

#' @export
l_graphswitch_ids <- function(widget) {
    as.character(tcl(widget, 'ids'))
}

#' @export
l_graphswitch_delete <- function(widget, id) {
    as.character(tcl(widget, 'delete', id))
}

#' @export
l_graphswitch_relabel <- function(widget, id, label) {
    tcl(widget, 'relabel', id, label)
}

#' @export
l_graphswitch_getLabel <- function(widget, id) {
    paste(as.character(tcl(widget, 'getLabel', id)), collapse=' ')
}

#' @export
l_graphswitch_move <- function(widget, id, index) {
    tcl(widget, 'move', id, .index4R(index))
}

#' @export
l_graphswitch_reorder <- function(widget, ids) {
    as.character(tcl(widget, 'reorder', ids))
}

#' @export
l_graphswitch_set <- function(widget, id) {
    as.character(tcl(widget, 'set', id))
}

#' @export
l_graphswitch_get <- function(widget, id) {
    tclgraph <- tcl(widget, 'get', id)
    
    loongraph(nodes=as.character(tcl('lindex', tclgraph, 0)),
              from=as.character(tcl('lindex', tclgraph, 1)),
              to=as.character(tcl('lindex', tclgraph, 2)),
              isDirected=as.logical(tcl('lindex', tclgraph, 3)))
}



