from .loonPlotFactory import *
from .loon_class import loon_l_plot
from .retrieve_name import *
import numpy as np
import pandas as pd
from tkinter import *

def l_plot(x=None, y=None,xlabel=None, ylabel=None, title=None,color = ["grey60"],glyph = ["ccircle"],
            size = 4,active = [True],selected = [False],showLabels = True,showScales = False,
            showGuides = True, guidelines = "white",guidesBackground = "grey92",foreground = "black",
            background = "white",parent = None,**options):
    """l_plot is a generic function for creating interactive

    Description:
       visualization environments for R objects.
    
    Args:
        x:  the x coordinates of points in the l_plot.
            Alternatively, a single plotting structure, function, or any R object having an l_plot
            method can be provided.  
        y:  the y coordinates of points in the l_plot, optional if x is an
            appropriate structure.
    Returns:
        widget handle
    Examples:
        p1 = l_plot(iris['Sepal.Length'],iris['Sepal.Width'],color = iris['Species'],title = "First plot")
        # The names of the info states that can be
        # accessed or set.  They can also be given values as    
        # arguments to l_plot.default()
        p1.names
        p1["size"] <- 10
    @namespace loon.l_plot
    """
    if(isinstance(x,pd.core.frame.DataFrame)):
        if(x.shape[1] > 1):
            y = x.iloc[:,1]
        x = x.iloc[:,0]      
    
    if(isinstance(x,pd.core.series.Series)):
        xlabel = x.name
        x = list(x)
    elif(isinstance(x,range)):
        xlabel = retrieve_name(x)
        x = list(x)
    
    if(isinstance(y,pd.core.series.Series)):
        ylabel = y.name
        y = list(y)
    elif(isinstance(y,range)):
        ylabel = retrieve_name(y)
        y = list(y)
    
    if(isinstance(color,pd.core.series.Series)):
        color = list(color)

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
    if (title == None): 
        title = ''
    if(x == None):      
        #kwargs = {"color": color, "glyph": glyph, "size": size, "active": active,"xlabel": xlabel,
        #        "ylabel": ylabel,"title": title,"selected": selected,"showLabels": showLabels,
        #        "showScales": showScales,"showGuides": showGuides, "guidelines": guidelines,
        #        "guidesBackground": guidesBackground,"foreground": foreground,"background": background}
        plot = loonPlotFactory('::loon::plot', 'plot', 'loon scatterplot',parent)
    else:        
        ## Get x, y, xlab and ylab
        if(len(color) > 1):
            if(len(color) != len(x)):
                color = np.resize(color, len(x))
        if(len(active) > 1):
            if (len(active) != len(x)):
                exit("When more than length 1, length of active must match number of points:"+str(len(x)))
        if(len(selected) > 1):
            if(len(selected) != len(x)):
                exit("When more than length 1, length of selected must match number of points:"+str(len(x)))
        if(len(glyph) > 1):
            if(len(glyph) != len(x)):
                exit("When more than length 1, length of glyph must match number of points:"+str(len(x)))
        if(y == None):
            y = list(range(1,len(x)+1))
        kwargs = {"x":x,"y":y,"color": color, "glyph": glyph, "size": size, "active": active,"xlabel": xlabel,
                "ylabel": ylabel,"title": title,"selected": selected,"showLabels": showLabels,
                "showScales": showScales,"showGuides": showGuides, "guidelines": guidelines,
                "guidesBackground": guidesBackground,"foreground": foreground,"background": background,"parent":parent}
        kwargs.update(options)
        plot = loonPlotFactory('::loon::plot', 'plot', 'loon scatterplot',**kwargs)

    plot = loon_l_plot(plot)
    return(plot)


    
