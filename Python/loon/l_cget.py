from tk import tk 
from functools import singledispatch
from loon_class import *
from loonobject import *

# @singledispatch
def l_cget(target, state):
    """
    Query a Plot State
    
    Description:
        All of loon's displays have plot states. Plot states specify 
        what is displayed, how it is displayed and if and how the plot is linked 
        with other loon plots. Layers, glyphs, navigators and contexts have states 
        too (also refered to as plot states). This function queries a single plot 
        state.

    @see l_cget, l_info_states, l_create_handle
    Args:
        target: target loon object 
        state: query state
    Examples:
        @code
            p = l_plot(iris, color = iris.Species)
            l_cget(p, "color")
            p['selected']
        @endcode
    @namespace loon.l_cget
    """
#    exit('target should be a loon claas or string')

# @l_cget.register(loon)
# def _(target,state):
    obj_eval = loonobject(target, str)
    
    if(state[0] != "-"):
        dash_state = "-" + state
    else:
        dash_state = state
        state = state[1:]
    # Type = obj_eval('info', 'stateType', state)
    
    # if (Tyoe %in% c("double", "positive_double", "integer",
    #                 "positive_integer", "tempcoords", "in_unit_interval")) {
    #     environment(obj_eval)$convert <- function(x) {as.numeric(as.character(x))}
    # } else if (type == "boolean") {
    #     environment(obj_eval)$convert <- function(x) {as.logical(as.character(x))}
    # } else if (type == "data") {
    #     environment(obj_eval)$convert <- function(result) { 
    #         ## create a data.frame with all characters
    #         vars <- tcl('dict', 'keys', result)
    #         l <- sapply(as.character(tcl('dict','keys',result)),
    #                     FUN=function(var){
    #                         as.character(tcl('dict','get', result, var))
    #                     }, simplify=FALSE, USE.NAMES=TRUE)
            
    #         l[['stringsAsFactors']] <- FALSE
            
    #         do.call(data.frame, l)
    #     }          
    # } else if (type == "nested_double") {
    #     environment(obj_eval)$convert <- function(result) {
    #         dim <- as.numeric(tcl('llength', result))
    #         out <- vector(mode='list', length=dim)
    #         for (i in 1:dim) {
    #            out[[i]] <- as.numeric(tcl('lindex', result, i-1)) 
    #         }
    #         out
    #     }
    # } else if (state %in% c("n","p")) {
    #     environment(obj_eval)$convert <- function(x) {as.numeric(as.character(x))}
    # } else if (type == "nested_double") {
    #     environment(obj_eval)$convert <- l_nestedTclList2Rlist
    # } else {
    #     dim <- obj_eval('info', 'stateDimension', state)
    #     if (dim == "1") {
    #         environment(obj_eval)$convert <- function(x) {
    #             paste(as.character(x), collapse=' ')
    #         }
    #     }
    #  }
    return obj_eval('cget', dash_state)    

# @l_cget.register(str)
# def _(target, state):
#     widget <- try(l_create_handle(target), silent = TRUE)
#     if ("try-error" %in% class(widget)) {
#         stop(paste0(state, " is not accessible from", target, "via l_cget"))
#     }
#     else {
#         l_cget(widget, state)
#     }
# }
