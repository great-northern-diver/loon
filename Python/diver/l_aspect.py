from .tk import tk
from .helper import widget_to_string
from .l_throwErrorIfNotLoonWidget import l_throwErrorIfNotLoonWidget
from sys import exit 

def l_aspect(widget):
    '''Query the aspect ratio of a plot

    Returns: 
        aspect ratio

    Examples: 
            p = l_plot(iris['Sepal.Length'],iris['Sepal.Width'], color=iris['Species']) 
            l_aspect(p)
            l_setAspect(p,ratio = 1)
    
    @namespace loon.l_aspect
    '''
    widget = widget_to_string(widget)
    l_throwErrorIfNotLoonWidget(widget)
    return(tk.tk.call(widget, 'aspect'))

 
def l_setAspect(widget, ratio = None,x = None , y = None):
    '''Set the aspect ratio of a plot  

    Args:
        ratio: aspect ratio, optional, if omitted then the `x` and 
                `y` arguments have to be specified.
        x: optional, if the `ratio` argument is missing then `x` and
           `y` can be specified and the aspect ratio is calculted usding `y/x`.
        y: see description for `x` argument above
    
    Examples:
        p = l_plot(iris['Sepal.Length'],iris['Sepal.Width'], color=iris['Species']) 
        l_aspect(p)
        l_setAspect(p, x = 1, y = 2) 

    @namespace loon.l_setAspect
    '''
    if(ratio == x == y == None):
        exit('ratio, x, and y can not both be empty.')
    if(ratio == None and (x == None or y == None)):
        exit('when ratio is empty, x and y should be assigned')
    if(ratio == None):
        if(not isinstance(x,(int,float)) or not isinstance(y,(int,float))):
            exit('x and y should both be numeric')
        ratio = y/x
    if(not isinstance(ratio, (int,float)) or ratio <= 0):
        exit("aspect ractio needs to be a value > 0.")
    widget = widget_to_string(widget)
    l_throwErrorIfNotLoonWidget(widget)
    tk.tk.call(widget, 'aspect', ratio)    
