
# #' @export
l_navigator <- function(widget, ...) {

    obj_eval <- .loonobject(widget)
    
    as.character(obj_eval("navigator", ...))
}

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




l_navigator_add <- function(widget, from="", to="", proportion=0, color='orange', ...) {

    l_throwErrorIfNotLoonWidget(widget)

    if (from == "") {
        nodes <- l_cget(widget, "nodes")
        if ( length(nodes)>0 ) {
            from <- nodes[1]
        }
    }

    structure(l_navigator(widget, "add", from=from, to=to,
                          proportion=proportion, color=color, ...),
              widget = as.vector(widget),
              class = c("l_navigator", "loon")
    )
}

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
l_navigator_delete <- function(widget, id) {
    l_navigator(widget, "delete", id)
}

#' @title List Navigators
#' 
#' @description Lists all navigators that belong to a graph
#' 
#' @inheritParams l_navigator_delete
#' 
#' @seealso \code{\link{l_navigator_add}}
#' 
#' @export
l_navigator_ids <- function(widget) {
    l_navigator(widget, "ids")
}


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
l_navigator_walk_path <- function(navigator, path) {

    if(length(navigator)==2) {
        navigator <- vapply(navigator, as.vector, character(1))
        obj_eval <- .loonobject(navigator[1], navigator[2])
    } else {
        obj_eval <- .loonobject(navigator)
    }

    obj_eval('walk', 'path', path)

    invisible()
}

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
l_navigator_walk_forward <- function(navigator, to='') {

    if(length(navigator)==2) {
        navigator <- vapply(navigator, as.vector, character(1))
        obj_eval <- .loonobject(navigator[1], navigator[2])
    } else {
        obj_eval <- .loonobject(navigator)
    }

    if ( to == '') {
        obj_eval('walk', 'forward')
    } else {
        obj_eval('walk', 'forward', to)
    }
    invisible()
}


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
l_navigator_walk_backward <- function(navigator, to='') {

    if(length(navigator)==2) {
        navigator <- vapply(navigator, as.vector, character(1))
        obj_eval <- .loonobject(navigator[1], navigator[2])
    } else {
        obj_eval <- .loonobject(navigator)
    }

    if ( to == '') {
        obj_eval('walk', 'backward')
    } else {
        obj_eval('walk', 'backward', to)
    }

    invisible()
}


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
l_navigator_relabel <- function(widget, id, label) {
    l_navigator(widget, "relabel", id, label)
    invisible()
}

#' @title Query the Label of a Navigator
#' 
#' @description Returns the label of a navigator
#' 
#' @inheritParams l_navigator_relabel
#' 
#' @seealso \code{\link{l_navigator_add}}
#' 
#' @export
l_navigator_getLabel <- function(widget, id) {
    paste(l_navigator(widget, "getLabel", id), collapse=' ')
}


#' @title Get the sequence of nodes of a navigator's current path
#' 
#' @description Determines and returns the current path of the navigator.  
#' 
#' @param navigator navigator handle
#' @return a vector of node names for the current path of the navigator
#'   
#' @export
l_navigator_getPath <- function(navigator) {
    c(navigator["from"], navigator["to"])
}
