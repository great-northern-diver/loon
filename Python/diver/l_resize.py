from .tk import tk

def l_resize(widget, width, height):
    '''Resize Plot Widget

    Resizes the toplevel widget to a specific size.

    Args:
        widget: target widget name 
        width: width in pixels
        height: in pixels
    
    See Also: 
        `l_size`

    Examples:
        >>> p = l_plot(iris)
        >>> l_resize(p,300,300)
        >>> l_size(p)
    '''
    if(not isinstance(widget,str)):
        widget = widget.plot
    tk.tk.call("::loon::resize", widget, width, height)

