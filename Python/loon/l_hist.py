from .loonPlotFactory import *
from .loon_class import loon_l_hist
from .retrieve_name import *
import pandas as pd
from functools import singledispatch
import numpy as np
#from genetic_func.UseMethod import *
#from genetic_func.getclass import *
from .helper import match_arg
# def l_hist(x = None,
#          yshows = ["frequency", "density"],
#          showStackedColors = True,
#          origin = None,
#          binwidth = None,
#          showBinHandle = False,
#          xlabel = None,
#          parent = None,
#          **options):
#     """
#     Create an interactive histogram
    
#         l_hist is a generic function for creating an interactive histogram display that can be linked with
#         loon's other displays
#     Args:
#         x: vector with numerical data to perform the binning onx
#         yshows: one of "frequency" (default) or  "density"
#         showStackedColors:  if TRUE (default) then bars will be coloured according to
#                             colours of the po\ints; if FALSE, then the bars will be a uniform colour
#                             except for highlighted points.
#         origin: numeric scalar to define the binning origin
#         binwidth: a numeric scalar to specify the binwidth
#                   If NULL binwidth is set using David Scott's rule 
#                   when x is numeric and using the minumum numerical difference
#                   between factor levels when x is a factor or a character vector.
#         showBinHandle: If True, then an interactive "bin handle" appears on the plot
#                        whose movement resets the origin and the binwidth. Default is False
#         xlabel: label to be used on the horizontal axis. If NULL, an attempt at a meaningful label
#                 inferred from x will be made.
#     Returns:
#         widget handle
#    Examples:
#         @code
#             h = l_hist(iris["Sepal.Length"])
#             h.names
#             h["xlabel"] = "Sepal length"
#             h["showOutlines"] = False
#             h["yshows"]
#             h["yshows"] = "density"
#         @endcode
#     """
#     if(isinstance(x,pd.core.series.Series)):
#         xlabel = x.name
#         x = list(x)
#     if(isinstance(x,type(None))):
#         #yshows <- match.arg(yshows)
#         yshows = yshows[0]
#         if (origin == None or not isinstance(origin,[int,float])):
#             origin = 0
#         if (binwidth == None or not isinstance(binwidth,[int, float])):
#             binwidth = 1
#         xlabel = ''
#         kwargs = {"parent":parent, "yshows":yshows,"showStackedColors": showStackedColors,
#                     "xlabel":xlabel}

#         plot = loonPlotFactory('::loon::histogram', 'hist', 'loon histogram', **kwargs)
#         plot = loon(plot)
#     else:
#         ndims = len(np.asarray(x).shape)
#         if (ndims > 2): exit("x should have at most two dimensions")
#         if (ndims == 2):
#             # get a relatively informative xlabel
#             #if (xlabel == None):
#             #    xlabel = retrieve_name(x)
#             #if(xlabel == ''):
#             #    xlabel = 'column 1'
#             #x = x[:,0]
#             if(isinstance(x,pd.core.frame.DataFrame)):
#                 x = x.iloc()[:,0]
#                 xlabel = x.name
#                 x = list(x)
#             plot = l_hist(x,
#                            yshows = yshows,
#                            showStackedColors = showStackedColors,
#                            origin = origin,
#                            binwidth=binwidth,
#                            showBinHandle = showBinHandle,
#                            xlabel = xlabel,
#                            parent=parent,**options)
#         else:
#         #yshows <- match.arg(yshows)
#             yshows = yshows[0]
#             if (xlabel == None):
#                 xlabel = retrieve_name(x)
#         ## ylabel will be overwritten in ...
#             if (origin == None or not isinstance(origin,[int,float])):
#                 origin = min(x)
#             if (binwidth == None or not isinstance(binwidth,[int, float])):
#                 n = len(x)
#                 binwidth = 3.49 * np.std(x)/(n **(1/3))
#             plot = loonPlotFactory('::loon::histogram',
#                                 'hist',
#                                 'loon histogram',
#                                 parent,
#                                 x = x,
#                                 yshows = yshows,
#                                 showStackedColors = showStackedColors,
#                                 origin = origin,
#                                 binwidth=binwidth,
#                                 showBinHandle = showBinHandle,
#                                 xlabel = xlabel,
#                                 **options)
#         plot = loon(plot,'l_hist')
#     return(plot)

def l_hist(x = None,
         yshows = 'frequency',
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
            l_scaleto(h)
            h["showStackedColors"] = True
            h['color'] = list(iris["Species"])
            h["showStackedColors"] = False
            h["showOutlines"] = True
            h["showGuides"] = True
            # link another plot with the previous plot
            h['linkingGroup'] = "iris_data" 
            h2 = l_hist(iris["Petal.Width"],
                        linkingGroup="iris_data",
                        showStackedColors = True)
        @endcode
    @namespace loon.l_hist
    """
    if(yshows != 'frequency'):
        yshow_opt = ["frequency", "density"]
        yshows = match_arg(yshows,yshow_opt,'yshows')
    
    if(isinstance(x,pd.core.series.Series)):
        xlabel = x.name
        x = list(x)
    if(not isinstance(x[0], (int,float))):
        x = pd.Series(x,dtype='category')
        x = list(x.cat.codes + 1)
        if (binwidth == None or not isinstance(binwidth,(int, float))):
                binwidth = min(np.diff(np.sort(np.unique(x))))
        if (origin == None):
            origin = min(x)
    if(isinstance(x,type(None))):
        #yshows <- match.arg(yshows)
        
        if (origin == None or not isinstance(origin,(int,float))):
            origin = 0
        if (binwidth == None or not isinstance(binwidth,(int, float))):
            binwidth = 1
        xlabel = ''
        kwargs = {"parent":parent, "yshows":yshows,"showStackedColors": showStackedColors,
                    "xlabel":xlabel}

        plot = loonPlotFactory('::loon::histogram', 'hist', 'loon histogram', **kwargs)
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
            if (xlabel == None):
                xlabel = retrieve_name(x)
        ## ylabel will be overwritten in ...
            if (origin == None or not isinstance(origin,(int,float))):
                origin = min(x)

            if (binwidth == None):
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
    #plot = loon(plot,'l_hist')
    plot = loon_l_hist(plot)
    return(plot)

# def l_hist(x = None,
#          yshows = ["frequency", "density"],
#          showStackedColors = True,
#          origin = None,
#          binwidth = None,
#          showBinHandle = False,
#          xlabel = None,
#          parent = None,
#          **options):
#     args = {"x":x,"yshow":  yshows, 'showStackedColors': showStackedColors, 'origin': origin,
#             "bindwidth": binwidth, "showBinHandle": showBinHandle,"xlabel":xlabel,
#             "parent": parent}
#     args.update(options)
#     classname  = getclass(x)
#     plot = UseMethod("l_hist", classname,**args)
#     return(plot)

