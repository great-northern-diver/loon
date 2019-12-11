from .loonobject import *
from .tk import tk

def l_info_states(target, states=['all']):
    """Retrieve Information about the States of a Loon Widget
    
    Description:
        Loon's built-in object documentation. Can be used with every
        loon object that has plot states including plots, layers, navigators,
        contexts.  This is a generic function.
    
    Args:
        target: target loon class
        states: list with names of states. ['all'] is treated as a
        keyword and results in returning information on all plot states
    Returns:
        a named nested list with one element per state. The list elements are
        also named lists with type, dimension, defaultvalue,
        and description elements containing the respective information.

    Examples:
        p = l_plot(iris, linkingGroup="iris")
        i = l_info_states(p)
        p["names"]
        i["selectBy"]
        #l = l_layer_rectangle(p, x=range(iris[,1]), y=range(iris[,2]), color="")
        l_info_states(l)
        h = l_hist(iris["Sepal.Length"], linkingGroup="iris")
        l_info_states(h)
    @namespace loon.l_info_states
    """

    def trans(tclvalue):
        names = tk.tk.call('dict','keys',tclvalue)
        res = {}
        def ff(var):
            temp = {field:tk.tk.call('dict','get',tclvalue,var,field) for field in ["type", "dimension", "defaultvalue", "description"]}
            return temp
        res = {var:ff(var) for var in names}
        return res
        
    obj_eval = loonobject(target,convert=trans)

    if (len(states) == 1 and states[0] == 'all'):
        opt = ['info', 'states']
        result = obj_eval(*opt)
    else:
        opt = ['info', 'states',states]
        result = obj_eval(*opt)
    return(result)



