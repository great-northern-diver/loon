import numpy as np
import warnings
from sys import exit 
import operator
from itertools import compress,combinations
from .loon_class import loon_loongraph
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
    Note: 
        loongraph objects can be converted to graph objects (i.e. objects of
        class graph which is defined in the graph package) with the as.graph 
        function.
    Returns:
        a loon gragh object 
    Examples:
            >>>g = loongraph(nodes = ["A", "B", "C", "D"],
                            f    = ["A", "A", "B", "B", "C"],
                            t    = ["B", "C", "C", "D", "D"])
            >>># create a loon graph plot
            >>>p = l_graph(g)
    @namespace loon.loongraph
    """
    if (len(nodes) != len(np.unique(nodes))):
        warnings.warn("node names are not unique")
    if (len(f) != len(t)):
        exit("'from' and 'to' differ in length: " + str(len(f)) +' vs. '+ str(len(t)))
    if (len(f) != 0  and not all([x in nodes for x in f])):
        exit("the following nodes in 'from' do not exist:"+ ','.join(list(compress(f, [x not in nodes for x in f]))))
    if (len(t) != 0  and not all([x in nodes for x in t])):
        exit("the following nodes in 'to' do not exist:"+ ','.join(list(compress(t, [x not in nodes for x in t]))))
    #graph = {"nodes": nodes, "f": f, "t": t, "isDirected":isDirected}
    #class(graph) <- "loongraph"
    graph = loon_loongraph(nodes= nodes, From= f, to= t, isDirected= isDirected)
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
            >>>l = ['a','b','c','d','e']
            >>>g = completegraph(l)
            >>>l_graph(g)
    @namespace loon.completegraph
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

def linegraph(x, separator=":", **options):
    '''Create a linegraph of a graph
    
    Description:
        Create a lingraph of a loongraph
    
    Args:
        x: loon_loongraph class object
        separator: one character - node names in x get concatenated with this character
    
    Details:
        linegraph needs the code part for directed graphs (i.e.
        isDirected=TRUE)
    
    Examples:
        g = loongraph(['a','b','c','d'], ['a','b','c'], ['b','c','d'], False)
        linegraph(g)
    '''
    if(not isinstance(x,loon_loongraph)):
        exit('x should be loon_loongraph class object')
    nodes = x.nodes
    From = x.From
    to = x.to
    
    n = len(nodes) 
    p = len(From)
    if(x.isDirected):
        exit('not implemented for directed graphs yet.')
    else:
        newfrom = []
        newto = []
        for i in range(p):
            for j in range(i,p):
                if(i != j and not(From[i] == From[j] and to[i] == to[j])):
                    ## Do they share a node?
                    if(From[i] == From[j] or From[i] == to[j] or to[i] == From[j] or to[i] == to[j]):
                        newfrom.append(From[i] + separator + to[i])
                        newto.append(From[j] + separator + to[j])
    
    newnodes = list(set(newfrom + newto))

    G = loongraph(nodes=newnodes, f=newfrom,
                   t=newto, isDirected=x.isDirected)
    G.set_separator(separator)    
    return(G)

def complement(x):
    '''Create the Complement Graph of a loon Graph
    
    Description:
        Creates a complement graph of a graph
        
    Details:
        This method is currently only implemented for undirected graphs.
        
    Args:
        x: loongraph object
    '''
    if(not isinstance(x,loon_loongraph)):
        exit('x should be loon_loongraph class object')
    nodes = x.nodes
    From = x.From
    to = x.to
    n = len(nodes)

    newfrom = []
    newto = []
    if (x.isDirected):
        exit('not implemented for directed graphs yet.')
    else:
        if(n > 0):
            for i in range(n):
                for j in range(i,n):
                    a1 = [temp == nodes[i] for temp in From]
                    a2 = [temp == nodes[j] for temp in to]
                    b1 = [temp == nodes[j] for temp in From]
                    b2 = [temp == nodes[i] for temp in to]
                    if(i != j and not(np.any(list(map(operator.and_,a1,a2))) or np.any(list(map(operator.and_,b1,b2))))):
                        newfrom.append(nodes[i])
                        newto.append(nodes[j])    
    
    G = loongraph(nodes=nodes, f=newfrom, t=newto,
                   isDirected=x.isDirected)
    G.separator = x.separator
    return(G)
