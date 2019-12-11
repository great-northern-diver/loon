from .tk import tk 
from .loonobject import loonobject
from .l_throwErrorIfNotLoonWidget import l_throwErrorIfNotLoonWidget
from .l_cget import l_cget
from .loon_class import loon_l_navigator
from .helper import opts_to_list
# #' @export
def l_navigator(widget,*opt_list,**opt_dict):
    obj_eval = loonobject(widget)
    options = list(opt_list) + opts_to_list(opt_dict)
    res = str(obj_eval("navigator", *options))
    return res 

#' @title Add a Navigator to a Graph
#'   
#' @description To turn a graph into a navigation graph you need to add one or 
#'   more navigators. Navigator have their own set of states that can be queried
#'   and modified.
#'   
#' @param widget graph widget
#' @param from The position of the navigator on the graph is defined by the
#'   states \code{from}, \code{to} and \code{proportion}. The states \code{from}
#'   and \code{to} hold vectors of node names of the graph. The
#'   \code{proportion} state is a number between and including \code{0} and 
#'   \code{1} and defines how far the navigator is between the last element of 
#'   \code{from} and the first element of \code{to}. The \code{to} state can 
#'   also be an empty string \code{''} if there is no further node to go to. 
#'   Hence, the concatenation of \code{from} and \code{to} define a path on the 
#'   graph.
#' @param to see descriptoin above for \code{from}
#' @param proportion see descriptoin above for \code{from}
#' @param color of navigator
#' @param ... named arguments passed on to modify navigator states
#'   
#' @templateVar page  learn_R_display_graph
#' @templateVar section navigators
#' @template see_l_help
#'   
#' @return navigator handle with navigator id
#'   
#' @seealso \code{\link{l_navigator_delete}}, \code{\link{l_navigator_ids}},
#'   \code{\link{l_navigator_walk_path}},
#'   \code{\link{l_navigator_walk_forward}},
#'   \code{\link{l_navigator_walk_backward}}, \code{\link{l_navigator_relabel}},
#'   \code{\link{l_navigator_getLabel}}
#'   
#' @export

def l_navigator_add(widget, From="", to="", proportion=0, color='orange', **options):

    l_throwErrorIfNotLoonWidget(widget)

    if (From == ""):
        nodes = l_cget(widget, "nodes")
        if ( len(nodes)>0 ):
            From = nodes[0]
        
    res = l_navigator(widget, "add", From=From, to=to,
                          proportion=proportion, color=color, **options)
    res = loon_l_navigator(res,widget.plot)
    return res 

#' @title Delete a Navigator
#' 
#' @description Removes a navigator from a graph widget
#' 
#' @inheritParams l_navigator_add
#' @param id navigator handle or navigator id
#' 
#' @seealso \code{\link{l_navigator_add}}
#' 
#' @export
def l_navigator_delete(widget, id):
    if(isinstance(id,loon_l_navigator)):
        id = id.id 
    l_navigator(widget, "delete", id)

#' @title List Navigators
#' 
#' @description Lists all navigators that belong to a graph
#' 
#' @inheritParams l_navigator_delete
#' 
#' @seealso \code{\link{l_navigator_add}}
#' 
#' @export
def l_navigator_ids(widget):
    res = l_navigator(widget, "ids")
    return res 


#' @title Have the Navigator Walk a Path on the Graph
#'  
#' @description Animate a navigator by having it walk on a path on the graph
#'  
#' @param navigator navigator handle
#' @param path vector with node names of the host graph that form a valid path
#'   on that graph
#'   
#' @seealso \code{\link{l_navigator_add}}
#'   
#' @export
def l_navigator_walk_path(navigator, path):

    obj_eval = loonobject(navigator)
    # if(length(navigator)==2) {
    #     navigator <- vapply(navigator, as.vector, character(1))
    #     obj_eval <- .loonobject(navigator[1], navigator[2])
    # } else {
    #     obj_eval <- .loonobject(navigator)
    # }
    obj_eval('walk', 'path', path)



#' @title Have the Navigator Walk Forward on the Current Path
#'   
#' @description Animate a navigator by having it walk on a path on the graph
#' 
#' @inheritParams l_navigator_walk_path
#' @param to node name that is part of the active path forward where the
#'   navigator should stop.
#'   
#' @details Note that navigators have the states \code{animationPause} and 
#'   \code{animationProportionIncrement} to control the animation speed. 
#'   Further, you can stop the animation when clicking somewhere on the graph 
#'   display or by using the mouse scroll wheel.
#'   
#' @seealso \code{\link{l_navigator_add}}
#'   
#' @export
def l_navigator_walk_forward(navigator, to=''):
    # if(length(navigator)==2) {
    #     navigator <- vapply(navigator, as.vector, character(1))
    #     obj_eval <- .loonobject(navigator[1], navigator[2])
    # } else {
    #     obj_eval <- .loonobject(navigator)
    # }
    obj_eval = loonobject(navigator)

    if (to == ''):
        obj_eval('walk', 'forward')
    else:
        obj_eval('walk', 'forward', to)

#' @title Have the Navigator Walk Backward on the Current Path
#' 
#' @description Animate a navigator by having it walk on a path on the graph
#' 
#' @inheritParams l_navigator_walk_forward
#' @param to node name that is part of the active path backward where the 
#'   navigator should stop.
#'   
#' @details Note that navigators have the states \code{animationPause} and 
#'   \code{animationProportionIncrement} to control the animation speed. 
#'   Further, you can stop the animation when clicking somewhere on the graph 
#'   display or by using the mouse scroll wheel.
#'   
#' @seealso \code{\link{l_navigator_add}}
#'   
#' @export
def l_navigator_walk_backward(navigator, to=''):

    # if(length(navigator)==2) {
    #     navigator <- vapply(navigator, as.vector, character(1))
    #     obj_eval <- .loonobject(navigator[1], navigator[2])
    # } else {
    #     obj_eval <- .loonobject(navigator)
    # }
    obj_eval = loonobject(navigator)

    if (to == ''):
        obj_eval('walk', 'backward')
    else:
        obj_eval('walk', 'backward', to)


#' @title Modify the Label of a Navigator
#' 
#' @description Change the navigator label
#' 
#' @param widget graph widget handle
#' @param id navigator id
#' @param label new label of navigator
#' 
#' @seealso \code{\link{l_navigator_add}}
#' 
#' @export
def l_navigator_relabel(widget, id, label):
    l_navigator(widget, "relabel", id, label)

#' @title Query the Label of a Navigator
#' 
#' @description Returns the label of a navigator
#' 
#' @inheritParams l_navigator_relabel
#' 
#' @seealso \code{\link{l_navigator_add}}
#' 
#' @export
def l_navigator_getLabel(widget, id):
    res = l_navigator(widget, "getLabel", id)
    return res 


#' @title Get the sequence of nodes of a navigator's current path
#' 
#' @description Determines and returns the current path of the navigator.  
#' 
#' @param navigator navigator handle
#' @return a vector of node names for the current path of the navigator
#'   
#' @export
def l_navigator_getPath(navigator):
    if(not isinstance(navigator,loon_l_navigator)):
        exit('navigator shouls be a loon_l_navigator class object.')
    res = [navigator.From,navigator.to]
    return res 