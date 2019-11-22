from .tk import tk
from sys import exit

def l_getOption(option):
    """ 
    Get the value of a loon display option

    Description:
        All of loon's displays access a set of common options.  
        This function accesses and returns the current value of the named option.

    Args:
        option: the name of the option being queried.
    
    Returns:
        the value of the named option.

    @see `l_getOptionNames`, `l_userOptions`,`l_userOptionDefault`, `l_setOption`

    Examples: 
            l_getOption("background")
    
    @namespace loon.l_getOption
    """
    if(not isinstance(option,str)):
        exit("option must be a single string")
    
    if (option not in l_getOptionNames()): 
        exit("option ", option, " is not valid")
    
    return tk.tk.eval("set ::loon::Options("+ option+ ")")

def l_getOptionNames():
    ''' Get the names of all loon display options

    Description:
        All of loon's displays access a set of common options.  
        This function accesses and returns the names of all loon options.
      
    Returns:
        a list of all loon display option names.

    @see `l_getOption`, `l_userOptions`, `l_userOptionDefault`, `l_setOption`

    Examples: 
            l_getOptionNames()
    
    @namespace loon.l_getOptionNames
    '''
    return str(tk.tk.eval('array names ::loon::Options')).split(' ')


def l_userOptions():
    '''
    Get the names of all loon display options that can be set by the user.

    Description:
        All of loon's displays access a set of common options.  
        This function accesses and returns the names of the subset of loon options which
        can be changed by the user.
      
    Returns:
        a list of all user settable option names.

    @see `l_getOptionNames`, `l_getOption`, `l_userOptionDefault`, `l_setOption`

    Examples: 
            l_userOptions()

    @namespace loon.l_userOptions
    '''
    return ["select-color","background","foreground","guidesBackground",
            "guidelines","brush_color","brush_color_handle"]


def l_userOptionDefault(option):
    '''
    Get loon's system default value for the named display option.

    Description:
        All of loon's displays access a set of common options.  
        This function accesses and returns the default value for the named option.
      
    Args:
        option: the name of the user changeable loon display option 
                whose default value is to be determined.
   
    Returns:
        the default value for the named option

    @see `l_getOptionNames`, `l_getOption`, `l_userOptionDefault`, `l_userOptions`

    Examples: 
            l_userOptionDefault("background")

    @namespace loon.l_userOptionDefault
    '''
    switcher = {
                "select-color":"magenta",
                "background" : "white",
                "foreground" :"black",
                "guidesBackground" : "gray92",
                "guidelines" : "white",
                "brush_color" : "lightgray",
                "brush_color_handle" : "gray",
                }
    return switcher.get(option,"ERROR: option "+ option+ " is not settable by user, so no default value.")

def l_setOption(option, value):
    '''
    Set the value of a loon display option

    Description:
        All of loon's displays access a set of common options.  
        This function assigns the value to the named option.
    
    Args:
        option: the name of the option being set
        value: the value to be assigned to the option.  If value == "default", 
               then the option is set to loon's default value for it.
    
    Returns:
        the new value

    @see `l_getOption`, `l_getOptionNames`, `l_userOptions`,`l_userOptionDefault`

    Examples: 
            l_setOption("select-color", "red")
            l_setOption("select-color", "default")

    @namespace loon.l_setOption
    '''
    if (value == "default"):
        value =  l_userOptionDefault(option)
        l_setOption(option, value)
    
    if (option not in  l_getOptionNames()):
        exit("option "+ option+ " is not valid")
    if (option not in l_userOptions()):
        exit("option "+ option+ " is not settable by user.")
    tk.tk.call("set", "::loon::Options("+ option+ ")", value)
    return value
