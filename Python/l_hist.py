def l_hist(x=None, origin=None, binwidth=None, parent=None, options = None):
    """Documentation for a function.
    More details.
    """
    if(x == None):
        plot = loonPlotFactory('::loon::histogram', 'hist', 'loon histogram', parent, options)
    else:
        xlabel = retrieve_name(x)        
        if(origin == None):
            origin = min(x)
        if(binwidth == None):
            binwidth = (max(x) - min(x))/30
            if(binwidth < 0.0001):
                binwidth = 0.00005
        plot = loonPlotFactory('::loon::histogram', 'hist', 'loon histogram', parent,
                                options= {'x':x, 'origin':origin, 'binwidth':binwidth,'xlabel':xlabel})
        plot = loon_obj(plot)
    return(plot)
