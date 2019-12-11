from .tk import tk

def l_size(widget):
    '''Query Size of a Plot Display

    Get the width and height of a plot in pixels
    
    Returns:
        list of width and height in pixels

    See Also: 
        `l_resize`
    '''
    if(not isinstance(widget,str)):
        widget = widget.plot
    width = tk.tk.call('winfo','width',widget)
    height = tk.tk.call('winfo','height',widget)
    return([width,height])
