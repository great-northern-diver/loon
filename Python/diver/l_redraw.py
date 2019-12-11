from .tk import tk

def l_redraw(widget):
    '''Force a Content Redraw of a Plot

    Description:
        Force redraw the plot to make sure that all the visual elements 
        are placed correctly.

    Details:
        Note that this function is intended for debugging. If you find that 
        the display does not display the data according to its plot states then
        please contact loon's package maintainer.
    Examples:
        p = l_plot(iris)
        l_redraw(p)

    @namespace loon.l_redraw
    '''
    tk.tk.call(widget, "redraw")

