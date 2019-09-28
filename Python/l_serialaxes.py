def l_serialaxes(data, sequence=None, scaling="variable", axesLayout='radial',showAxes=TRUE, parent=None,options =  None):
    data = pd.DataFrame(data)                            
    if(sequence == None):
        sequence = list(data.columns)
    plot = loonPlotFactory('::loon::serialaxes', 'serialaxes', 'loon serialaxes plot', parent,
                    {"data":l_data(data),
                    "sequence":sequence,
                    "showAxes":showAxes,
                    "scaling":scaling,
                    "axesLayout":axesLayout,
                    "options": options})
    plot = loon_obj(plot)
    return(plot)