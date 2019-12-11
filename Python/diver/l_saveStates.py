import pickle 
from .loon_class import loon_l_savedStates, loon, loon_l_compound
from .l_compound import l_getPlots
def l_saveStates(p, states = ["color", "active", "selected","linkingKey", "linkingGroup"],
                        file = None,**options):
    '''Save the info states of a loon plot widget as an RDS file

    `l_saveStatesRDS` uses `pickle` module to save the info states 
    of a loon plot as an python class object to the named file.
    This is helpful, for example, when using some notebooking
    facility to recreate an earlier saved loon plot so as to present it
    in the document.

    Args:
        p: the plot object whose info states are to be saved.
        states: either the logical `True` or a character list of info states to be saved.
                Default value `["color", "active", "selected", "linkingKey", "linkingGroup"]`
                consists of `n` dimensional states that are common to many `l_plot`s
                and which are most important to reconstruct the plot's display in any summary.
                If `states` is the logical `True`, by `p.names` are saved.
        file: is a string giving the file name where the saved information'
              will be written (custom suggests this file name end in the suffix `.pkl`.
    
    See Also: 
        `l_copyStates`, `l_info_states, `pickle.dump`, `pickle.load`

    Examples:
        >>> # Suppose you some plot that you created like
        >>> p = l_plot(iris, showGuides = True)
        >>> # and coloured groups by hand (using the mouse and inspector)
        >>> # so that you ended up with these colours:
        >>> p["color"] = ["lightgreen", "firebrick","skyblue"]*50
        >>> # Having determined the colours you could save them (and other states)
        >>> # in a file of your choice, here some tempfile:
        >>> import tempfile
        >>> myFileName = tempfile.NamedTemporaryFile(suffix = '.pkl').name
        >>> # Save the names states of p
        >>> l_saveStates(p,
                        states = ["color", "active", "selected"],
                        file = myFileName)
        >>> # These can later be retrieved and used on a new plot
        >>> # (say in RMarkdown) to set the new plot's values to those
        >>> # previously determined interactively.
        >>> p_new = l_plot(iris, showGuides = True)
        >>> import pickle
        >>> p_saved_info = pickle.load(open(myFileName,'rb'))
        >>> p_new["color"] = p_saved_info['color']
        >>> # The result is that p_new looks like p did
        >>> # (after your interactive exploration)
    '''
    if(file == None):
        exit('missing name of file')
    if(not file.endswith('.pkl')):
        exit('must save into a pkl file, file name should ends with .pkl')
    if(isinstance(states,bool)):
        if(states == True):
            states = ['all']
    if(isinstance(states,str)):
        states = [states]
    if(isinstance(p,loon_l_compound)):
        result = {}
        plots = l_getPlots(p)
        for p_name,p in plots.items():
            if('all' in states):
                plots_states = p.names
            else:
                plots_states = list(set(p.names) & set(states))
            
            result[p_name] = {x:p[x] for x in plots_states}
    else:
        if('all' in states):
            states = p.names
        result = {x:p[x] for x in states}
    result = loon_l_savedStates(result,type(p))
    pickle.dump(result,open(file,'wb'),pickle.HIGHEST_PROTOCOL)

