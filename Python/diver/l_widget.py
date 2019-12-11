def l_widget(widget):
    """Dummy function to be used in the Roxygen documentation
    
    Description:
        Helper function to ensure that a widget path is associated with a
        loon widget.

    Args:
        widget: widget path name as a string
    
    Returns:
        widget path name as a string
    
    @namespace loon.l_widget
    """
    if(not isinstance(widget, str)):
        widget = widget.plot
    return(widget)
