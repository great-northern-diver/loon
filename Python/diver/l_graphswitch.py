from .l_toplevel import l_toplevel
from .l_subwin import l_subwin
from .tk import tk
from .helper import opts_to_list
from .loon_class import loon_l_graphswitch,loon,loon_loongraph
from .graphutils import loongraph
def l_graphswitch(activewidget="", parent=None, **options):
    '''Create a graphswitch widget

    Description: 
        The graphswitch provides a graphical user interface for changing
        the graph in a graph display interactively.

    Args:
        activewidget: widget handle of a graph display

    SeeAlso:
        `l_graphswitch_add`, `l_graphswitch_ids`,
        `l_graphswitch_delete`, `l_graphswitch_relabel`,
        `l_graphswitch_getLabel`, `l_graphswitch_move`, 
        `l_graphswitch_reorder`, `l_graphswitch_set`,
        `l_graphswitch_get`
    '''
    if(isinstance(activewidget,loon)):
        activewidget = activewidget.plot
    new_toplevel = False
    if(parent == None):
        new_toplevel = True
        parent = l_toplevel()
    
    child = l_subwin(parent, 'graphswitch')
    opts = opts_to_list(options)
    widget = str(tk.tk.call('::loon::graphswitch',child,'-activewidget',activewidget,*opts))
    
    # if(is(widget, 'try-error')) {
    #     if(new.toplevel) tkdestroy(parent)
    #     stop("graphswitch could not be created.")
    # }
    
    if(new_toplevel):
        tk.tk.call("pack",widget,'-fill','both','-expand',True)
        tk.tk.call('wm','title',widget,"loon graphswitch"+widget)
    
    res = loon_l_graphswitch(widget)
    return res


def l_graphswitch_add(widget, graph, label="", index='end', **options):
    if(not isinstance(graph,loon_loongraph)):
        exit('graph should be a loon_loongraph class object.')
    if(isinstance(widget,loon)):
        widget = widget.plot
    nodes = graph.nodes
    From = graph.From
    to = graph.to
    isDirected = graph.isDirected
    temp = tk.tk.call('info', 'object', 'namespace', widget)
    call = str(tk.tk.call('info', 'object', 'namespace', widget))+'::my'
    res = str(tk.tk.call(call,'AddExternal', nodes, From, to, isDirected,
                     label, index))
    return(res)
# #' @title Add a graph to a graphswitch widget
# #'   
# #' @description This is a generic function to add a graph to a graphswitch
# #'   widget.
# #'   
# #' @template param_widget
# #' @param graph a graph or a loongraph object
# #' @param ... arguments passed on to method
# #' 
# #' @templateVar page learn_R_display_graph
# #' @templateVar section graph-switch-widget
# #' @template see_l_help
# #'
# #' @template return_l_graphswitch_add
# #' 
# #' @seealso \code{\link{l_graphswitch}}
# #'     
# #' @export
# l_graphswitch_add <- function(widget, graph, ...) {
#     UseMethod("l_graphswitch_add", graph)
# } 

# .index4R <- function(index) {
#     if (is.numeric(index)) {
#         index <- index - 1
#     }
#     index
# }


# #' @title Add a graph that is defined by node names and a from-to edges list
# #'   
# #' @description This default method uses the loongraph display states as 
# #'   arguments to add a graph to the graphswitch widget.
# #'    
# #' @inheritParams l_graph.default
# #' @param widget graphswitch widget handle (or widget path)
# #' @param graph a vector with the node names, i.e. this argument gets passed on
# #'   as the nodes argument to creat a \code{\link{loongraph}} like object
# #' @param label string with label for graph
# #' @param index position of graph in the graph list
# #' @param isDirected boolean to indicate whether the from-to-list defines 
# #'   directed or undirected edges
# #' @template param_dots_method_not_used
# #'   
# #' @template return_l_graphswitch_add
# #'   
# #' @seealso \code{\link{l_graphswitch}}
# #'   
# #'   
# #' @export
# l_graphswitch_add.default <- function(widget, graph, from, to, isDirected,
#                                       label="", index="end", ...) {
#     nodes <- graph
#     #print("add graph to switch")
#     #print(paste( widget, 'add', label, nodes, from, to, isDirected, pos))

#     if (length(from) == 0) {
#         from <- vector()
#         to <- vector()
#     }
    
#     call <- paste0(tcl('info', 'object', 'namespace', widget),'::my')
    
#     as.character(tcl(call, 'AddExternal', nodes, from, to, isDirected,
#                      label, .index4R(index)))
# } 


# #' @title Add a graph to the graphswitch widget using a loongraph object
# #' 
# #' @description Loongraphs can be created with the \code{\link{loongraph}}
# #'   function.
# #'   
# #' @inheritParams l_graphswitch_add.default
# #' @param graph a loongraph object
# #' @template param_dots_method_not_used
# #' 
# #' @template return_l_graphswitch_add
# #' 
# #' @seealso \code{\link{l_graphswitch}}
# #' 
# #' @export
# l_graphswitch_add.loongraph <- function(widget, graph, label="", index='end', ...) {
#     l_graphswitch_add.default(widget, graph$nodes, label=label, 
#                               graph$from, graph$to, graph$isDirected, .index4R(index))    
# }


# #' @title Add a graph to the graphswitch widget using a graph object
# #' 
# #' @description Graph objects are defined in the graph \R package.
# #'   
# #' @inheritParams l_graphswitch_add.default
# #' @param graph a graph object created with the functions in the \code{graph} \R
# #'   package.
# #' @template param_dots_method_not_used
# #' 
# #' @template return_l_graphswitch_add
# #' 
# #' @seealso \code{\link{l_graphswitch}}
# #' 
# #' @export
# l_graphswitch_add.graph <- function(widget, graph, label="", index='end', ...) {
#     l_graphswitch_add.loongraph(widget, as.loongraph(graph), label, .index4R(index))
# }

#' @title List the ids of the graphs in the graphswitch widget
#'   
#' @description Every graph in the graphswitch widget has an id. This function 
#'   returns these ids preserving the oder of how the graphs are listed in the
#'   graphswitch.
#' 
#' @inheritParams l_graphswitch_add.default
#' 
#' @export
def l_graphswitch_ids(widget):
    if(isinstance(widget,loon)):
        widget = widget.plot
    res = tk.tk.call(widget, 'ids')
    return list(res) 


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
def l_graphswitch_delete(widget, id):
    if(isinstance(widget,loon)):
        widget = widget.plot
    if(isinstance(id,loon_l_graphswitch)):
        id = id.id 

    res = tk.tk.call(widget,'delete',id)
    return res 
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
def l_graphswitch_relabel(widget, id, label):
    if(isinstance(widget,loon)):
        widget = widget.plot
    if(isinstance(id,loon_l_graphswitch)):
        id = id.id 
    tk.tk.call(widget, 'relabel', id, label)


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
def l_graphswitch_getLabel(widget, id):
    if(isinstance(widget,loon)):
        widget = widget.plot
    if(isinstance(id,loon_l_graphswitch)):
        id = id.id 
    res = tk.tk.call(widget,'getLabel',id)
    return res
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
def l_graphswitch_move(widget, id, index):
    if(isinstance(widget,loon)):
        widget = widget.plot
    if(isinstance(id,loon_l_graphswitch)):
        id = id.id 
    tk.tk.call(widget,'move',id,index)



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
def l_graphswitch_reorder(widget, ids):
    if(isinstance(widget,loon)):
        widget = widget.plot
    new_ids = []
    for x in ids:
        if(isinstance(x,loon_l_graphswitch)):
            new_ids.append(x.id)
        else:
            new_ids.append(x)
    #as.character(tcl(widget, 'reorder', ids))
    res = tk.tk.call(widget, 'reorder',new_ids)
    return res 

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
def l_graphswitch_set(widget, id):
    if(isinstance(widget,loon)):
        widget = widget.plot
    if(isinstance(id,loon_l_graphswitch)):
        id = id.id 
    res = tk.tk.call(widget,'set',id)
    return res


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
def l_graphswitch_get(widget, id):
    if(isinstance(widget,loon)):
        widget = widget.plot
    if(isinstance(id,loon_l_graphswitch)):
        id = id.id 
    tclgraph = tk.tk.call(widget, 'get', id)
    
    res = loongraph(nodes=tk.tk.call('lindex', tclgraph, 0),
              f=tk.tk.call('lindex', tclgraph, 1),
              t=tk.tk.call('lindex', tclgraph, 2),
              isDirected=tk.tk.call('lindex', tclgraph, 3))
    return res 

