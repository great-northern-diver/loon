from .l_toplevel import *
from .l_subwin import *
from .tk import tk 
from .helper import opts_to_list
import threading

def loonPlotFactory(factory_tclcmd,factory_path,factory_window_title="loon plot", parent=None, **kwargs):
    """Documentation for a function.
    More details.
    """
    # print('parent:',parent)
    new_toplevel = False
    if(parent == None):
        new_toplevel = True
        parent = l_toplevel()
    child = l_subwin(parent, factory_path)
    # print('child:',child)

    if(len(kwargs) == 0):
        plot = tk.tk.call(factory_tclcmd, child)
    else:
        opt = opts_to_list(kwargs)
        plot = tk.tk.call(factory_tclcmd,child,*opt)

    if(new_toplevel):
        tk.tk.call('pack', plot,'-expand',1,'-fill','both')
        tk.tk.call('wm','title',parent, factory_window_title +" "+ str(plot))
        ## Bind the toplevel <FocusIn> event to update the loon inspector
        tk.tk.call("bind", parent, "<FocusIn>","+::loon::setLoonInspectorActiveWidget "+ str(plot))
        def temp():
                exportImageDialog(plot)
        ## Bind Ctrl-P to export image
        tk.tk.call("bind", parent, "<Control-KeyPress-p>",temp)

        # tcl("bind", parent, "<Control-KeyPress-P>",
        #     function()exportImageDialog(plot))
    plot = str(plot)
    return(plot)