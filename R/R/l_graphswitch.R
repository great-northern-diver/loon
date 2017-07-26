#' @title Create a graphswitch widget
#'   
#' @description The graphswitch provides a graphical user interface for changing
#'   the graph in a graph display interactively.
#'   
#' @param activewidget widget handle of a graph display
#' @inheritParams l_subwin
#' @param ... widget states
#'   
#'   
#' @templateVar page learn_R_display_graph
#' @templateVar section graph-switch-widget
#' @template see_l_help
#'   
#'   
#' @seealso \code{\link{l_graphswitch_add}}, \code{\link{l_graphswitch_ids}},
#'   \code{\link{l_graphswitch_delete}}, \code{\link{l_graphswitch_relabel}},
#'   \code{\link{l_graphswitch_getLabel}}, \code{\link{l_graphswitch_move}}, 
#'   \code{\link{l_graphswitch_reorder}}, \code{\link{l_graphswitch_set}},
#'   \code{\link{l_graphswitch_get}}
#'   
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


#' @title Add a graph to a graphswitch widget
#'   
#' @description This is a generic function to add a graph to a graphswitch
#'   widget.
#'   
#' @template param_widget
#' @param graph a graph or a loongraph object
#' @param ... arguments passed on to method
#' 
#' @templateVar page learn_R_display_graph
#' @templateVar section graph-switch-widget
#' @template see_l_help
#'
#' @template return_l_graphswitch_add
#' 
#' @seealso \code{\link{l_graphswitch}}
#'     
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


#' @title Add a graph that is defined by node names and a from-to edges list
#'   
#' @description This default method uses the loongraph display states as 
#'   arguments to add a graph to the graphswitch widget.
#'   
#' @inheritParams l_graph.default
#' @param widget graphswitch widget handle (or widget path)
#' @param graph a vector with the node names, i.e. this argument gets passed on
#'   as the nodes argument to creat a \code{\link{loongraph}} like object
#' @param label string with label for graph
#' @param index position of graph in the graph list
#' @param isDirected boolean to indicate whether the from-to-list defines 
#'   directed or undirected edges
#' @template param_dots_method_not_used
#'   
#' @template return_l_graphswitch_add
#'   
#' @seealso \code{\link{l_graphswitch}}
#'   
#'   
#' @export
l_graphswitch_add.default <- function(widget, graph, from, to, isDirected,
                                      label="", index="end", ...) {
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


#' @title Add a graph to the graphswitch widget using a loongraph object
#' 
#' @description Loongraphs can be created with the \code{\link{loongraph}}
#'   function.
#'   
#' @inheritParams l_graphswitch_add.default
#' @param graph a loongraph object
#' @template param_dots_method_not_used
#' 
#' @template return_l_graphswitch_add
#' 
#' @seealso \code{\link{l_graphswitch}}
#' 
#' @export
l_graphswitch_add.loongraph <- function(widget, graph, label="", index='end', ...) {
    l_graphswitch_add.default(widget, graph$nodes, label=label, 
                              graph$from, graph$to, graph$isDirected, .index4R(index))    
}


#' @title Add a graph to the graphswitch widget using a graph object
#' 
#' @description Graph objects are defined in the graph \R package.
#'   
#' @inheritParams l_graphswitch_add.default
#' @param graph a graph object created with the functions in the \code{graph} \R
#'   package.
#' @template param_dots_method_not_used
#' 
#' @template return_l_graphswitch_add
#' 
#' @seealso \code{\link{l_graphswitch}}
#' 
#' @export
l_graphswitch_add.graph <- function(widget, graph, label="", index='end', ...) {
    l_graphswitch_add.loongraph(widget, as.loongraph(graph), label, .index4R(index))
}

#' @title List the ids of the graphs in the graphswitch widget
#'   
#' @description Every graph in the graphswitch widget has an id. This function 
#'   returns these ids preserving the oder of how the graphs are listed in the
#'   graphswitch.
#' 
#' @inheritParams l_graphswitch_add.default
#' 
#' @export
l_graphswitch_ids <- function(widget) {
    as.character(tcl(widget, 'ids'))
}

#' @title Delete a graph from the graphswitch widget
#' 
#' @description Remove a a graph from the graphswitch widget
#' 
#' @inheritParams l_graphswitch_add.default
#' @param id of the graph
#' 
#' @seealso \code{\link{l_graphswitch}}
#' 
#' @export
l_graphswitch_delete <- function(widget, id) {
    as.character(tcl(widget, 'delete', id))
}

#' @title Relabel a Graph in the Graphswitch Widget
#'   
#' @description The graphs in the graphswitch widgets have labels. Use this
#'   function the relabel a graph.
#'   
#' @inheritParams l_graphswitch_delete
#' @param label string with label of graph
#' 
#' @seealso \code{\link{l_graphswitch}}
#' 
#' @export
l_graphswitch_relabel <- function(widget, id, label) {
    tcl(widget, 'relabel', id, label)
}


#' @title Query Label of a Graph in the Graphswitch Widget
#'   
#' @description The graphs in the graphswitch widgets have labels. Use this 
#'   function to query the label of a graph.
#' 
#' @inheritParams l_graphswitch_delete
#'   
#' @seealso \code{\link{l_graphswitch}}
#'   
#' @export
l_graphswitch_getLabel <- function(widget, id) {
    paste(as.character(tcl(widget, 'getLabel', id)), collapse=' ')
}


#' @title Move a Graph in the Graph List
#'   
#' @description Change the postion in of a graph in the graphswitch widget.
#'   
#' @inheritParams l_graphswitch_delete
#' @param index position of the graph as a positive integer, \code{"start"} and
#'   \code{"end"} are also valid keywords.
#' 
#'   
#' @seealso \code{\link{l_graphswitch}}
#'   
#' @export
l_graphswitch_move <- function(widget, id, index) {
    tcl(widget, 'move', id, .index4R(index))
}


#' @title Reorder the Positions of the Graphs in the Graph List
#'   
#' @description Define a new graph order in the graph list.
#'   
#' @inheritParams l_graphswitch_move
#' @param ids vector with all graph ids from the graph widget. Use
#'   \code{\link{l_graphswitch_ids}} to query the ids.
#'   
#' @seealso \code{\link{l_graphswitch}}
#'   
#' @export
l_graphswitch_reorder <- function(widget, ids) {
    as.character(tcl(widget, 'reorder', ids))
}

#' @title Change the Graph shown in the Active Graph Widget
#'   
#' @description The \code{activewidget} state holds the widget handle of a graph
#'   display. This function replaces the graph in the \code{activewidget} with
#'   one of the graphs in the graphswitch widget.
#'   
#' @inheritParams l_graphswitch_delete
#'   
#' @seealso \code{\link{l_graphswitch}}
#'   
#' @export
l_graphswitch_set <- function(widget, id) {
    as.character(tcl(widget, 'set', id))
}

#' @title Return a Graph as a loongraph Object
#'   
#' @description Graphs can be extracted from the graphswitch widget as loongraph
#'   objects.
#'   
#' @inheritParams l_graphswitch_delete
#'   
#' @seealso \code{\link{l_graphswitch}}, \code{\link{loongraph}}
#'   
#' @export
l_graphswitch_get <- function(widget, id) {
    tclgraph <- tcl(widget, 'get', id)
    
    loongraph(nodes=as.character(tcl('lindex', tclgraph, 0)),
              from=as.character(tcl('lindex', tclgraph, 1)),
              to=as.character(tcl('lindex', tclgraph, 2)),
              isDirected=as.logical(tcl('lindex', tclgraph, 3)))
}

