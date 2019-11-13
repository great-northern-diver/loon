from .loon_class import *
def l_zoom(widget, factor=1.1):
    '''
    Zoom from and towards the center

    Description:
        This function changes the plot states `panX`, `panY`, 
        `zoomX`, `zoomY` to zoom towards or away from the center of
        the current view.

    Args:
        widget: target widget 
        factor: a zoom factor
    
    @namespace loon.l_zoom
    '''
    if(not isinstance(widget,loon)):
        widget = loon(widget)
    zoomX = widget['zoomX']
    zoomY = widget['zoomY']
    panX = widget['panX']
    panY = widget['panY']
    deltaX =  widget['deltaX']
    deltaY = widget['deltaY']
    
    fp = (1/factor-1)/2
    new_panX = panX - deltaX/zoomX * fp
    new_panY = panY - deltaY/zoomY * fp
    
    l_configure(widget, panX=new_panX, panY=new_panY,
                zoomX=zoomX*factor, zoomY=zoomY*factor)
