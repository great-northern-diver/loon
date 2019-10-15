from l_toplevel import *
from l_subwin import *
from tk import tk 

#def loonPlotFactory(factory_tclcmd,factory_path,factory_window_title="loon plot", parent=None, options=None):
def loonPlotFactory(factory_tclcmd,factory_path,factory_window_title="loon plot", parent=None, **kwargs):
    """Documentation for a function.
    More details.
    """
    new_toplevel = False
    if(parent == None):
        new_toplevel = True
        parent = l_toplevel()
    child = l_subwin(parent, factory_path)
    if(len(kwargs) == 0):
        plot = tk.tk.call(factory_tclcmd, child)
    else:
        opt = []
        for key, value in kwargs.items():
            opt.append('-' + key)
            opt.append(value)
        plot = tk.tk.call(factory_tclcmd,child,*opt)
    if(new_toplevel):
        tk.tk.call('pack', plot,'-expand',1,'-fill','both')
        tk.tk.call('wm','title',parent, factory_window_title +" "+ str(plot))
    plot = str(plot)
    return(plot)