import pandas as pd
from loonPlotFactory import *
from loon_class import loon_l_serialaxes
from l_data import *

def l_serialaxes(data, sequence=None, scaling="variable", axesLayout='radial',showAxes=True, parent=None,**options):
    """
    Create a Serialaxes Widget
    Description:
       The seerialaxes widget displays multivariate data either as a stacked star glyph plot, or as a parallel coordinate plot.    
    
    Args:
       data: a data frame with numerical data only
       sequence: vector with variable names that defines the axes sequence
       scaling: one of 'variable', 'data', 'observation' or 'none' to specify how the data is scaled. See Details for more information
       axesLayout: either "radial" or "parallel"
       showAxes: boolean to indicate whether axes should be shown or not
       parent: parent widget path
       options: state arguments, see l_info_states.
    Returns:
        plot handle object
    Details: 
        The scaling state defines how the data is scaled. The axes display 0 at 
        one end and 1 at the other. For the following explanation assume that the 
        data is in a nxp dimensional matrix. The scaling options are then
        <table>
        <tr><td>variable    <td>per column scaling
        <tr><td>observation <td>per row scaling
        <tr><td>data        <td>whole matrix scaling
        <tr><td>none        <td>do not scale
        </table>
    Examples:
        @code
            s = l_serialaxes(data=oliveAcids, color=olive$Area, title="olive data")
        @endcode
    @namespace loon.l_serialaxes
    """
    data = pd.DataFrame(data)                            
    if(sequence == None):
        sequence = list(data.columns)
    kwargs = {"data":l_data(data),"sequence":sequence,"showAxes":showAxes,
                "scaling":scaling,"axesLayout":axesLayout}
    kwargs.update(options)
    plot = loonPlotFactory('::loon::serialaxes', 'serialaxes', 'loon serialaxes plot', parent,**kwargs)
    #plot = loon(plot,'l_serialaxes')
    plot = loon_l_serialaxes(plot)
    return(plot)   
