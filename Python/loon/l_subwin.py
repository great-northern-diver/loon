from loonPlotFactory import *
from tk import tk 

def l_subwin(parent, name="w"):
    """
    Create a child widget path 
    
        This function is similar to Tk.subwin except that does
        not the environment of the "tkwin" object to keep track of numbering the
        subwidgets. Instead it creates a widget path (parent).looni, where i is the
        smallest integer for which no widget exists yet.

    Args:
        parent: parent widget path
        name: child name
    Returns:
        widget path name as a string
    """
    if(parent == "tkwin"): 
        parent = parent.winfo_id()    
    i = 0
    child = parent + '.'+ name
    while(int(tk.eval('winfo ' + 'exists '+ child))):       
        i = i + 1
        child = parent+ '.'+ name+ i        
    return(child)
