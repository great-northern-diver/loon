#' @export
l_navigator <- function(widget, ...) {

    obj_eval <- .loonobject(widget)
    
    as.character(obj_eval("navigator", ...))
}

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
              class=c("loon", "l_navigator"),
              widget=as.vector(widget))
}

#' @export
l_navigator_delete <- function(widget, id) {
    l_navigator(widget, "delete", id)
}

#' @export
l_navigator_ids <- function(widget) {
    l_navigator(widget, "ids")
}


#' @export
l_navigator_walk_path<- function(navigator, path) {

    if(length(navigator)==2) {
        navigator <- vapply(navigator, as.vector, character(1))
        obj_eval <- .loonobject(navigator[1], navigator[2])
    } else {
        obj_eval <- .loonobject(navigator)
    }

    obj_eval('walk', 'path', path)

    invisible()
}

#' @export
l_navigator_walk_forward<- function(navigator, to='') {

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


#' @export
l_navigator_walk_backward<- function(navigator, to='') {

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


#' @export
l_navigator_relabel <- function(widget, id, label) {
    l_navigator(widget, "relabel", id, label)
    invisible()
}

#' @export
l_navigator_getLabel <- function(widget, id) {
    paste(l_navigator(widget, "getLabel", id), collapse=' ')
}

