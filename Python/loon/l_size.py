from tk import tk

def l_size(widget):
    '''
    Query Size of a Plot Display

    Description:
        Get the width and height of a plot in pixels
    
    Returns:
        list of width and height in pixels

    @see l_resize

    @namespace loon.l_size 
    '''
    if(not isinstance(widget,str)):
        widget = widget.plot
    width = tk.tk.call('winfo','width',widget)
    height = tk.tk.call('winfo','height',widget)
    return([width,height])
