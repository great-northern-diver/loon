from .l_throwErrorIfNotLoonWidget import *
from .tk import tk
from .loon_class import *
def l_scaleto(widget, *opt):
    if(isinstance(widget, loon_class.loon)):
        widget = widget.plot
    l_throwErrorIfNotLoonWidget(widget)
    tk.tk.call(widget, 'scaleto', *opt)



def l_scaleto_world(widget):
    '''Change Plot Region to Display All Plot Data

    Description: 
        The function modifies the `zoomX`, `zoomY`, 
        panX, and `panY` so that all elements in the plot are displayed.
    '''
    l_scaleto(widget, "world")


def l_scaleto_plot(widget):
    '''Change Plot Region to Display the All Data of the Model Layer

    Description: 
        The function modifies the `zoomX`, `zoomY`, 
        `panX`, and `panY` so that all elements in the model layer of the
        plot are displayed
    '''
    l_scaleto(widget, "plot")


def l_scaleto_selected(widget):
    '''Change Plot Region to Display All Selected Data

    Description: 
        The function modifies the `zoomX`, `zoomY`, 
        `panX`, and `panY` so that all selected data points are displayed. 
    '''
    l_scaleto(widget, "selected")



def l_scaleto_active(widget):
    '''Change Plot Region to Display All Active Data

    Description:
        The function modifies the `zoomX`, `zoomY`, 
        `panX`, and `panY` so that all active data points are displayed.    
    '''
    l_scaleto(widget, "active")

def l_scaleto_layer(layer):
    '''Change Plot Region to Display All Elements of a Particular Layer

    Description: 
        The function modifies the zoomX, zoomY, 
        `panX`, and `panY` so that all elements of a particular layer are displayed.    

    SeeAlso: 
        `l_layer_ids`
    '''
    target = layer.widget
    l_scaleto(target, "layer", layer.id)


