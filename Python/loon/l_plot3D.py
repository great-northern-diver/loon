from loonPlotFactory import *
import pandas as pd
from retrieve_name import *
from loon_class import loon_l_plot3D

def l_plot3D(x,  y = None, z = None,
                axisScaleFactor = 1, color = "grey60",
                glyph = "ccircle", size = 4, active = True, selected = False,
                xlabel = None, ylabel = None, zlabel = None,
                title = '', showLabels = True, showScales = False,
                showGuides = True, guidelines = "white",
                guidesBackground = "grey92", foreground = "black",
                background = "white", parent = None, **options):
    """    
    Create an interactive loon 3d plot widget
       
        l_plot3D is a generic function for creating interactive
        visualization environments for python objects.

    Args:
        x:  the x, y and z arguments provide the x, y and z coordinates for the plot.
            Any reasonable way of defining the coordinates is acceptable.
            See the function xyz.coords for details.
            If supplied separately, they must be of the same length.  
        y:  the y coordinates of points in the plot,
            optional if x is an appropriate structure.
        z:  the z coordinates of points in the plot,
            optional if x is an appropriate structure.
        axisScaleFactor: the amount to scale the axes at the centre of the rotation. Default is 1.
                         All numerical values are acceptable (0 removes the axes, < 0 reverses their direction.)
        color: colours of points; colours are repeated until matching the number points
        glyph:  shape of point; must be one of the primitive glyphs
                "circle", "ccircle", "ocircle", "square", "csquare", "osquare",
                "triangle", "ctriangle", "otriangle", "diamond", "cdiamond", or "odiamond".
                Prefixes "c" and "o" mean closed and open, respectively.
                Default is "ccircle" meaning a closed circle glyph.
                Non-primitive glyphs such as polygons, images, text, point ranges, and even interactive glyphs like
                serial axes glyphs may be added, but only after the plot has been created.
        size: size of the symbol (roughly in terms of area)
        active: a logical determining whether points appear or not
                (default is TRUE for all points). If a logical vector is given of length
                equal to the number of points, then it identifies which points appear (TRUE)
                and which do not (FALSE).
        selected: a logical determining whether points appear selected at first
                  (default is FALSE for all points). If a logical vector is given of length
                  equal to the number of points, then it identifies which points are (TRUE)
                  and which are not (FALSE).
        xlabel: Label for the horizontal (x) axis. If missing,
                one will be inferred from x if possible.
        ylabel: Label for the vertical (y) axis. If missing,
                one will be inferred from y (or x) if possible.
        zlabel: Label for the third (perpendicular to the screen) (z) axis. If missing,
                one will be inferred from z (or x) if possible.
        title: Title for the plot, default is an empty string.
        showLabels: logical to determine whether axes label (and title) should be presented.
        showScales: logical to determine whether numerical scales should
                    be presented on both axes.
        showGuides: logical to determine whether to present background guidelines
                    to help determine locations.
        guidelines: colour of the guidelines shown when showGuides = TRUE (default "white").
        guidesBackground: colour of the background to the guidelines shown when
                          showGuides = TRUE (default "grey92").
        foreground: foreground colour used by all other drawing (default "black").
        background: background colour used for the plot (default "white")
        parent: a valid Tk parent widget path. When the parent widget is
                specified (i.e. not None) then the plot widget needs to be placed using
                some geometry manager like tkpack or tkplace in
                order to be displayed. See the examples below.
        **options: named arguments to modify plot states.
    @note  The scatterplot displays a number of direct interactions with the
           mouse and keyboard, these include: rotating, zooming towards the mouse cursor using
           the mouse wheel, panning by right-click dragging and various selection
           methods using the left mouse button such as sweeping, brushing and
           individual point selection.
    Returns:
        widget handle
    Examples:
        @code
            height = [1,2,1]
            width = [1,2,3]    
            length = [1,2,3] 
            l_plot3D(height,width,length)
        @endcode
    """
    if(isinstance(x,pd.core.series.Series)):
        xlabel = x.name
        x = list(x)

    if(isinstance(y,pd.core.series.Series)):
        ylabel = y.name
        y = list(y)
    if(isinstance(z,pd.core.series.Series)):
        zlabel = z.name
        z = list(z)
    
    if(x == None):
        kwargs = {"axisScaleFactor": 1, "color": "grey60",
                "glyph": "ccircle", "size": 4, "active": True, "selected": False,
                "xlabel": None, "ylabel": None, "zlabel": None,
                "title": title, "showLabels": True, "showScales": False,
                "showGuides": True, "guidelines": "white",
                "guidesBackground": "grey92", "foreground": "black",
                "background": "white"}
        plot = loonPlotFactory('::loon::plot3D', 'plot3D', 'loon scatterplot3D', parent, **kwargs)
    else:

        ## Get x, y, z, xlab, ylab, zlab
        ## similar as in plot.default use xyz.coords
        if (xlabel == None):
            if (x != None):
                xlabel = retrieve_name(x)
            else:
                xlabel = ''
            
        if (ylabel == None):
            if (y != None):
                ylabel = retrieve_name(y)
            else:
                ylabel = ''
        if (zlabel == None):
            if (z != None):
                zlabel = retrieve_name(z)
            else:
                zlabel = ''

        kwargs = {"x": x, "y": y, "z": z, "axisScaleFactor": 1, "color": "grey60",
                "glyph": "ccircle", "size": 4, "active": True, "selected": False,
                "xlabel": xlabel, "ylabel": ylabel, "zlabel": zlabel,
                "title": title, "showLabels": True, "showScales": False,
                "showGuides": True, "guidelines": "white",
                "guidesBackground": "grey92", "foreground": "black",
                "background": "white"}
        kwargs.update(options)
        plot = loonPlotFactory('::loon::plot3D', 'plot3D', 'loon scatterplot3D', parent,**kwargs)
    #plot = loon(plot,"l_plot3D")
    plot = loon_l_plot3D(plot)
    return(plot)



