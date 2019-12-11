from .loon_class import * 
from .l_info import *

def l_state_names(target):
    """Get State Names of Loon Object
    
    Description:
        States of loon objects can be accessed by '[]', '.'and l_cget
        and modified with l_configure.
    
    Args:
        target: target loon class

    Returns:
        state names
    
    Details:
        In order to access values of a states use l_info_states.
    
    SeeAlso: 
        l_info_states, l_cget, l_configure
    
    @namespace loon.l_state_names
    """
    # if(not isinstance(target,loon_class.loon)):
    #     exit('target needs to be of class loon')
    return list(l_info_states(target).keys())
    

