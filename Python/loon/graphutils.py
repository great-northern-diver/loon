import numpy as np
import warnings
from itertools import compress,combinations

def loongraph(nodes, f = '', t='', isDirected=False):
    """    
    Create a graph object of class loongraph
       
       The loongraph class provides a simple alternative to the graph 
       class to create common graphs that are useful for use as navigation graphs.
    
    Args:
        nodes:  a character vector with node names, each element defines a node
                hence the elements need to be unique
        f: a character vector with node names, each element defines an edge
        t: a character vector with node names, each element defines an edge
        isDirected: boolean scalar, defines whether from and to define directed edges
    @note loongraph objects can be converted to graph objects (i.e. objects of
            class graph which is defined in the graph package) with the as.graph 
            function.
    Returns:
        a loon gragh object 
    Examples:
        @code 
            >>>g = loongraph(nodes = ["A", "B", "C", "D"],
                            f    = ["A", "A", "B", "Bx", "C"],
                            t    = ["B", "C", "C", "D", "D"])
            >>># create a loon graph plot
            >>>p = l_graph(**g)
        @endcode
    """
    if (len(nodes) != len(np.unique(nodes))):
        warnings.warn("node names are not unique")
    if (len(f) != len(t)):
        exit("'from' and 'to' differ in length: " + str(len(f)) +' vs. '+ str(len(t)))
    if (len(f) != 0  and not all([x in nodes for x in f])):
        exit("the following nodes in 'from' do not exist:"+ ','.join(list(compress(f, [x not in nodes for x in f]))))
    if (len(t) != 0  and not all([x in nodes for x in t])):
        exit("the following nodes in 'to' do not exist:"+ ','.join(list(compress(t, [x not in nodes for x in t]))))
    graph = {"nodes": nodes, "f": f, "t": t, "isDirected":isDirected}
    #class(graph) <- "loongraph"
    return(graph)

def completegraph(nodes, isDirected=False):
    """    
    Create a complete graph or digraph with a set of nodes
       
        From Wikipedia: "a complete graph is a simple undirected graph 
        in which every pair of distinct vertices is connected by a unique edge. A 
        complete digraph is a directed graph in which every pair of distinct 
        vertices is connected by a pair of unique edges (one in each direction
    
    Args:
        nodes:  a character vector with node names, each element defines a node
                hence the elements need to be unique
        isDirected: a boolean scalar to indicate wheter the returned object is 
                    a complete graph (undirected) or a complete digraph (directed).
    @note Note that this function masks the completegraph function of the
             graph package. Hence it is a good idead to specify the package namespace
             with ::, i.e. loon::completegraph and graph::completegraph.
    Returns:
        a loon gragh object 
    Examples:
        @code 
            >>>l = ['a','b','c','d','e']
            >>>g = completegraph(l)
            >>>l_graph(**g)
        @endcode
    """
    n = len(nodes)
    if(n < 2):
        exit("number of nodes must be > 2.")
    if (isDirected):
        co =  np.array(np.meshgrid(nodes,nodes)).reshape(2,n*n)
        rm_index = list(range(0,n*n,n+1))
        mask = np.ones(co.shape[1], np.bool)
        co = co[:,mask]
        f = list(co[0,:])
        t = list(co[1,:])
    else:
        co = np.array(list(combinations(nodes, 2)))
        f = list(co[:,0])
        t = list(co[:,1])
    return(loongraph(nodes=nodes, f=f, t=t, isDirected=isDirected))

