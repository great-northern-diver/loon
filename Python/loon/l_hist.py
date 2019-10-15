from loonPlotFactory import *
from loon_class import *
from retrieve_name import *
import pandas as pd
from functools import singledispatch
import numpy as np
def l_hist(x = None,
         yshows = ["frequency", "density"],
         showStackedColors = True,
         origin = None,
         binwidth = None,
         showBinHandle = False,
         xlabel = None,
         parent = None,
         **options):
    """
    Create an interactive histogram
    
        l_hist is a generic function for creating an interactive histogram display that can be linked with
        loon's other displays
    Args:
        x: vector with numerical data to perform the binning onx
        yshows: one of "frequency" (default) or  "density"
        showStackedColors:  if TRUE (default) then bars will be coloured according to
                            colours of the po\ints; if FALSE, then the bars will be a uniform colour
                            except for highlighted points.
        origin: numeric scalar to define the binning origin
        binwidth: a numeric scalar to specify the binwidth
                  If NULL binwidth is set using David Scott's rule 
                  when x is numeric and using the minumum numerical difference
                  between factor levels when x is a factor or a character vector.
        showBinHandle: If True, then an interactive "bin handle" appears on the plot
                       whose movement resets the origin and the binwidth. Default is False
        xlabel: label to be used on the horizontal axis. If NULL, an attempt at a meaningful label
                inferred from x will be made.
    Returns:
        widget handle
   Examples:
        @code
            h = l_hist(iris["Sepal.Length"])
            h.names
            h["xlabel"] = "Sepal length"
            h["showOutlines"] = False
            h["yshows"]
            h["yshows"] = "density"
        @endcode
    """
    if(isinstance(x,pd.core.series.Series)):
        xlabel = x.name
        x = list(x)
    if(isinstance(x,type(None))):
        #yshows <- match.arg(yshows)
        yshows = yshows[0]
        if (origin == None or not isinstance(origin,[int,float])):
            origin = 0
        if (binwidth == None or not isinstance(binwidth,[int, float])):
            binwidth = 1
        xlabel = ''
        kwargs = {"parent":parent, "yshows":yshows,"showStackedColors": showStackedColors,
                    "xlabel":xlabel}

        plot = loonPlotFactory('::loon::histogram', 'hist', 'loon histogram', **kwargs)
        plot = loon(plot)
    else:
        ndims = len(np.asarray(x).shape)
        if (ndims > 2): exit("x should have at most two dimensions")
        if (ndims == 2):
            # get a relatively informative xlabel
            #if (xlabel == None):
            #    xlabel = retrieve_name(x)
            #if(xlabel == ''):
            #    xlabel = 'column 1'
            #x = x[:,0]
            if(isinstance(x,pd.core.frame.DataFrame)):
                x = x.iloc()[:,0]
                xlabel = x.name
                x = list(x)
            plot = l_hist(x,
                           yshows = yshows,
                           showStackedColors = showStackedColors,
                           origin = origin,
                           binwidth=binwidth,
                           showBinHandle = showBinHandle,
                           xlabel = xlabel,
                           parent=parent,**options)
        else:
        #yshows <- match.arg(yshows)
            yshows = yshows[0]
            if (xlabel == None):
                xlabel = retrieve_name(x)
        ## ylabel will be overwritten in ...
            if (origin == None or not isinstance(origin,[int,float])):
                origin = min(x)
            if (binwidth == None or not isinstance(binwidth,[int, float])):
                n = len(x)
                binwidth = 3.49 * np.std(x)/(n **(1/3))
            plot = loonPlotFactory('::loon::histogram',
                                'hist',
                                'loon histogram',
                                parent,
                                x = x,
                                yshows = yshows,
                                showStackedColors = showStackedColors,
                                origin = origin,
                                binwidth=binwidth,
                                showBinHandle = showBinHandle,
                                xlabel = xlabel,
                                **options)
        plot = loon(plot,'l_hist')
    return(plot)

# def l_hist(x=None, origin=None, binwidth=None, parent=None,**options):
#     if(isinstance(x,pd.core.series.Series)):
#         xlabel = x.name
#         x = list(x)

#     if(x == None):
#         plot = loonPlotFactory('::loon::histogram', 'hist', 'loon histogram', parent,**options)
#     else:
#         if(xlabel == None):
#             xlabel = retrieve_name(x)        
#         if(origin == None):
#             origin = min(x)
#         if(binwidth == None):
#             binwidth = (max(x) - min(x))/30
#             if(binwidth < 0.0001):
#                 binwidth = 0.00005
#         kwargs = {'x':x, 'origin':origin, 'binwidth':binwidth,'xlabel':xlabel}
#         kwargs.update(options)
#         plot = loonPlotFactory('::loon::histogram', 'hist', 'loon histogram', parent,**kwargs)
#     plot = loon_obj(plot,'l_hist')
#     return(plot)

