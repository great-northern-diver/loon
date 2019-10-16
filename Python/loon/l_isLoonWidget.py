from tk import tk
def l_isLoonWidget(widget):
    """
    Check if a widget path is a valid loon widget
    
        This function can be useful to check whether a loon widget is
        has been closed by the user.

    Args:
        widget: widget path name as a string
    Returns:
        boolean, True if the argument is a valid loon widget path, False 
        otherwise
    """
    if(isinstance(type(widget), list)):
        return False
    else:
        isLoon = tk.tk.call('::loon::isKnownWidget', widget)
        if (isLoon): return True
        else: return False
