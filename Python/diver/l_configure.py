from .loonobject import *
from .l_data import *
import numpy as np
def l_configure(target,**kwargs):
    """
    Modify one or multiple plot states
    
    Description:
        All of loon's displays have plot states. Plot states specify 
        what is displayed, how it is displayed and if and how the plot is linked 
        with other loon plots. Layers, glyphs, navigators and contexts have states 
        too (also refered to as plot states). This function modifies one or 
        multiple plot states.
    @see l_cget, l_info_states, l_create_handle
    Args:
        target: target loon object 
        **kwargs: configuration states and values(state=value)
    Examples:
        @code
            p = l_plot(iris, color = iris['Species'])
            l_configure(p, color='red')
            p['size'] = [2 if x == 'versicolor' else 8 for x in iris['Species']]
        @endcode
    @namespace loon.l_configure
    """
    obj_eval = loonobject(target)
    #args <- list('configure', ...)
    if('data' in kwargs.keys()):
        kwargs['data'] = l_data(kwargs['data'])
    # if('command' in kwargs.keys()):
    #     if(isinstance(kwargs['command'],function)):
    #   if (!is.null(args[['command']])) {
    #     if (is.function(args[['command']])) {
    #         callback <- .Tcl.callback(args[['command']])
    #         callbackFunctions$general[[paste0(paste(environment(obj_eval)$specifier,
    #                                                 collapse='.'),
    #                                           '.command')]] <- args[['command']]
    #         args[['command']] <- callback
    #     }
    # }
    # kwargsnames = kwargs.keys()
    # if (any(substr(argnames,0,5) == 'which')) {
    #     argwhich <- argnames[substr(names(args),0,5) == 'which']
        
    #     for (w in argwhich) {
    #         if(!is.logical(args[[w]]) && !is.character(args[[w]])) {
    #             args[[w]] <- args[[w]]-1
    #         }
    #     }
    # }

    ## nested lists in polygons layer or point glyphs
    # if(any(environment(obj_eval)$type %in% c("layer", 'glyph'))) {
    #     if (!is.null(args[['x']])) {
    #         if (is.list(args[['x']])) {
    #             args[['x']] <- l_Rlist2nestedTclList(args[['x']])
    #         }
    #         if (is.list(args[['y']])) {
    #             args[['y']] <- l_Rlist2nestedTclList(args[['y']])
    #         }            
    #     }
    # }
    opt = []
    for key, value in kwargs.items():
        opt.append('-' + key)
        if(isinstance(value,np.ndarray)):
            value = list(value)
        opt.append(value)
    obj_eval('configure',*opt)
    

# #' @export
# l_configure.character <- function(target, ...) {
#     widget <- try(l_create_handle(target), silent = TRUE)
#     if ("try-error" %in% class(widget)) {
#         stop(paste0(target, " is not configurable via l_configure()"))
#     }
#     else {
#         l_configure(widget, ...)
#     }
# }
