from .l_toplevel import l_toplevel
from .graphutils import completegraph,linegraph,complement
from .l_graphswitch import l_graphswitch,l_graphswitch_add
from .l_graph import l_graph
from .l_navigator import l_navigator_add
from .l_context2d import l_context_add_geodesic2d
from .loon_class import loon_l_plot, loon_l_navgraph
from .l_configure import l_configure
from .tk import tk
import pandas as pd 
from itertools import combinations
from sys import exit 
def l_navgraph(data, separator=":", graph=None,**options):
    '''Explore a dataset with the canonical 2d navigation graph setting
    
    Creates a navigation graph, a graphswitch, a navigator and a 
    geodesic2d context added, and a scatterplot.

    Args:
        data: a pandas.DataFrame with numeric variables only
        graph: optional, graph or loongraph object with navigation graph. If 
                the graph argument is not used then a 3d and 4d transition graph and a 
                complete transition graph is added.
        **options: arguments passed on to modify the scatterplot plot states
    
    Returns:
        return a loon_l_navgraph object, which include the information of
        `graph` handle, `plot` handle, `graphswitch` handle, `navigator`
        handle, and `context` handle.

    Examples:
        >>> ng = l_navgraph(oliveAcids, color=olive.Area)
        >>> ng2 = l_navgraph(oliveAcids, separator='-', color=olive.Area)
    '''
    if(not isinstance(data,pd.DataFrame)):
        data = pd.DataFrame(data)
    
    tt = l_toplevel()
    if(graph == None):
        G = completegraph(nodes=list(data.columns))
        LG = linegraph(G, separator=separator)
        LGnot = complement(LG)        
        cmb  = list(combinations(list(data.columns),2))
        CompG = completegraph(nodes= [separator.join(x) for x in cmb])
        
        g = l_graph(LG, tt)
    else:
        g = l_graph(graph, tt)
        
    tk.tk.call('wm','title',tt,"loon navigation graph: " + g.plot)
    gs = l_graphswitch(activewidget=g, parent=tt)
    if (graph == None):
        l_graphswitch_add(gs, graph=LG, label="3d transition")
        l_graphswitch_add(gs, graph=LGnot, label="4d transition")
        l_graphswitch_add(gs, graph=CompG, label="3d and 4d transition")
    else:
        gid = l_graphswitch_add(gs, graph=graph, label="user specified")
    tk.tk.call('pack',gs.plot,'-side','right','-fill','y')
    tk.tk.call('pack',g.plot,'-side','right','-fill','both','-expand',True)
    navigator = l_navigator_add(g, label="default navgraph")
    context = l_context_add_geodesic2d(navigator, data=data, separator=separator)  
    plot = context['command'].split(' ')[0]
    plot = loon_l_plot(plot)
    if(len(options) > 0):
        l_configure(plot,**options)

    navgraph = loon_l_navgraph({'graph':g,'plot':plot,'graphswitch':gs,
                                'navigator':navigator,'context':context})

    return navgraph

