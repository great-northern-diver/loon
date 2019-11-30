from .tk import tk
def l_setTitleFont(size='16', weight="bold", family="Helvetica"):
    '''
    Set the title font of all loon displays
    
    Description:
        All of loon's displays access a set of common options.  
        This function sets the font for the title bar of the displays.

    Args:
        size: the font size, default is 16
        weight: the font size, default is 'bold'
        family: the font family, default is 'Helvetica'

    @see `l_getOptionNames`, `l_userOptions`, `l_userOptionDefault, `l_setOption`   

    Examples:
            l_setTitleFont(14)
    
    @namespace loon.l_setTitleFont
    '''
    res = tk.tk.call('font', 'configure',tk.tk.call('set', '::loon::Options(font-title)'),
                    '-weight',weight, '-size', size, '-family', family)
    return res


