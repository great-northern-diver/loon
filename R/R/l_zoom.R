
#' Zoom from and towards the center
#' @export
l_zoom <- function(widget, factor=1.1) {
    zoomX <- widget['zoomX']
    zoomY <- widget['zoomY']
    panX <- widget['panX']
    panY <- widget['panY']
    deltaX <-  widget['deltaX']
    deltaY <- widget['deltaY']
    
    fp <- (1/factor-1)/2
    new_panX <- panX - deltaX/zoomX * fp
    new_panY <- panY - deltaY/zoomY * fp
    
    l_configure(widget, panX=new_panX, panY=new_panY,
                zoomX=zoomX*factor, zoomY=zoomY*factor)
}
