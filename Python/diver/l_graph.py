from .loonPlotFactory import *
from .loon_class import loon_l_graph,loon_loongraph
import numpy as np
from multipledispatch import dispatch

@dispatch(object,object,object,object)
def l_graph(nodes='', f='', t='',  parent=None,**options):
    """Create a graph display based on node names and from-to edges list
        
    This default method uses the loongraph display states as arguments to create a graph display.
    
    Args:
        nodes: vector with nodenames
        f:  vector with node names of the from-to pairs for edges
        t: vector with node names of the from-to pairs for edges
        parent: parent widget of graph display

    Returns:
        graph handle

    See Also: 
        `loongraph`, `l_graph`, `l_info_states`, `l_graph.graph`
    """
    kwargs = {'nodes':nodes, 'from':f, 'to': t}
    kwargs.update(options)
    plot = loonPlotFactory('::loon::graph', 'graph', 'loon graph', parent,**kwargs)
    plot = loon_l_graph(plot)
    return(plot)

@dispatch(loon_loongraph)
def l_graph(graph,**options):
    
    options["isDirected"] = graph.isDirected
    if('parent' in options.keys()):
        parent = options['parent']
    else:
        parent = None
    plot = l_graph(graph.nodes,graph.From,
                    graph.to,parent, **options)
    return(plot)

@dispatch(loon_loongraph,object)
def l_graph(nodes,parent = None,**options):
    graph = nodes 
    options["isDirected"] = graph.isDirected
    plot = l_graph(graph.nodes,graph.From,
                    graph.to, parent, **options)
    return(plot)