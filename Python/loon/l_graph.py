from .loonPlotFactory import *
from .loon_class import *
import numpy as np
def l_graph(nodes='', f='', t='',  parent=None,**options):
    """    
    Create a graph display based on node names and from-to edges list
       
       This default method uses the loongraph display states as arguments to create a graph display.
    
    Args:
        nodes: vector with nodenames
        f:  vector with node names of the from-to pairs for edges
        t: vector with node names of the from-to pairs for edges
        parent: parent widget of graph display
    Returns:
        graph handle
    @see loongraph, l_graph, l_info_states, l_graph.graph
    @namespace loon.l_graph
    """
    kwargs = {'nodes':nodes, 'from':f, 'to': t}
    kwargs.update(options)
    plot = loonPlotFactory('::loon::graph', 'graph', 'loon graph', parent,**kwargs)
    #class(plot) <- c("l_graph", class(plot))
    plot = loon(plot,'l_graph')
    return(plot)


