def retrieve_name(var):
    temp = inspect.currentframe().f_back.f_globals.items()
    return [var_name for var_name, var_val in temp if var_val is var]

def l_plot(x=None, y=None, parent=None, options = None):
    """Documentation for a function.
    More details.
    """
    if(x == None):      
        plot = loonPlotFactory('::loon::plot', 'plot', 'loon scatterplot', parent)
    else:        
        ## Get x, y, xlab and ylab
        ## similar as in plot.default use xy.coords
        xlabel = retrieve_name(x) 
        ylabel = retrieve_name(y) 
        plot = loonPlotFactory('::loon::plot', 'plot', 'loon scatterplot', parent,
                                options= {'x':x,'y':y,'xlabel':xlabel,'ylabel':ylabel})
        plot = loon_obj(plot)
    return(plot)
