

## I need to have a pointer to all the callback functions
## such that they do not get garbage collected.
callbackFunctions$canvas <- list()
callbackFunctions$state <- list()
callbackFunctions$item <- list()
callbackFunctions$layer <- list()
callbackFunctions$glyph <- list()
callbackFunctions$navigator <- list()
callbackFunctions$context <- list()
callbackFunctions$general <- list()


## Callback id for environment
.cbid <- function(widget, id) {
    paste(widget,id, sep="_")
}

l_bind <- function(widget, ...) {
    l_throwErrorIfNotLoonWidget(widget)
    as.character(tcl(widget, "bind", ...))
}



#### Canvas Bindings

#' @title Create a Canvas Binding
#' 
#' @description Canvas bindings are triggered by a mouse/keyboard gesture over
#'   the plot as a whole.
#' 
#' @template param_widget
#' @param event event patterns as defined for Tk canvas widget
#'   \url{http://www.tcl.tk/man/tcl8.6/TkCmd/bind.htm#M5}.
#' @template param_callback
#' 
#' @details 
#' Canvas bindings are used to evaluate callbacks at certain X events on the 
#' canvas widget (underlying widget for all of loon's plot widgets). Such X
#' events include re-sizing of the canvas and entering the canvas with the
#' mouse.
#' 
#' Bindings, callbacks, and binding substitutions are described in detail in
#' loon's documentation webpage, i.e. run \code{l_help("learn_R_bind")}
#' 
#'  
#' @return canvas binding id
#' 
#' @seealso \code{\link{l_bind_canvas_ids}}, \code{\link{l_bind_canvas_get}},
#'   \code{\link{l_bind_canvas_delete}}, \code{\link{l_bind_canvas_reorder}}
#'
#' @export
#' 
#' @template examples_bind_canvas
l_bind_canvas <- function(widget, event, callback) {    
    id <- l_bind(widget, "canvas", "add", event, callback)
    callbackFunctions$canvas[[.cbid(widget,id)]] <- callback
    id
}


#' @templateVar type canvas
#' @template templ_bind_ids
#' 
#' @template param_widget
#' 
#' @export
#' 
#' @template examples_bind_canvas
l_bind_canvas_ids <- function(widget) {
    l_bind(widget, "canvas", "get")
}


#' @templateVar type canvas
#' @template templ_bind_get
#' 
#' @template param_widget
#' 
#' @export
#' 
#' @template examples_bind_canvas
l_bind_canvas_get <- function(widget, id) {
    l_bind(widget, "canvas", "get", id)
}


#' @templateVar type canvas
#' @template templ_bind_delete
#' 
#' @template param_widget
#' 
#' @export
l_bind_canvas_delete <- function(widget, id) {
    l_bind(widget, "canvas", "delete", id)
    callbackFunctions$canvas[[.cbid(widget,id)]] <- NULL
    invisible(NULL)
}

#' @templateVar type canvas
#' @template templ_bind_reorder
#'  
#' @template param_widget
#'  
#' @export
l_bind_canvas_reorder <- function(widget, ids) {
    warning('canvas binding order has currently no effect.')    
    l_bind(widget, "canvas", "reorder", ids)
}

#### Layer Bindings


#' @templateVar type layer
#' @template templ_bind_collection
#'
#' @template param_widget
#'
#' @param event a vector with one or more of the following evnets: \code{'add'},
#'   \code{'delete'}, \code{'move'}, \code{'hide'}, \code{'show'},
#'   \code{'relabel'}
#'     
#' @export
l_bind_layer <- function(widget, event, callback) {
    id <- l_bind(widget, "layer", "add", event, callback)
    callbackFunctions$layer[[.cbid(widget,id)]] <- callback
    return(id)
}

#' @templateVar type layer
#' @template templ_bind_ids
#' 
#' @template param_widget
#' 
#' @export
l_bind_layer_ids <- function(widget) {
    l_bind(widget, "layer", "ids")
}

#' @templateVar type layer
#' @template templ_bind_get
#' 
#' @template param_widget
#'
#' @export
l_bind_layer_get <- function(widget, id) {
    l_bind(widget, "layer", "get", id)
}

#' @templateVar type layer
#' @template templ_bind_delete
#' 
#' @template param_widget
#' 
#' @export
l_bind_layer_delete <- function(widget, id) {
    l_bind(widget, "layer", "delete", id)
    callbackFunctions$layer[[.cbid(widget,id)]] <- NULL
    invisible(NULL)
}

#' @templateVar type layer
#' @template templ_bind_reorder
#' 
#' @template param_widget
#' 
#' @export
l_bind_layer_reorder <- function(widget, ids) {
    l_bind(widget, "layer", "reorder", ids)
}

#### State Bindings

#' @title Add a state change binding
#' 
#' @description The callback of a state change binding is evaluated when certain
#'   states change, as specified at binding creation.
#' 
#' @inheritParams l_bind_canvas
#' @template param_target
#' @param event vector with state names
#' 
#' @details 
#' Bindings, callbacks, and binding substitutions are described in detail in
#' loon's documentation webpage, i.e. run \code{l_help("learn_R_bind")}
#' 
#' @return state change binding id
#' 
#' @seealso \code{\link{l_info_states}}, \code{\link{l_bind_state_ids}},
#'   \code{\link{l_bind_state_get}}, \code{\link{l_bind_state_delete}},
#'   \code{\link{l_bind_state_reorder}}
#' 
#' 
#' @export
l_bind_state <- function(target, event, callback) {

    obj_eval <- .loonobject(target)
    
    id <- obj_eval('bind', 'state', 'add',
                   event, .Tcl.callback(callback))
    
    callbackFunctions$state[[paste(c(environment(obj_eval)$specifier,id), collapse='.')]] <- callback
    
    return(id)
}

#' @templateVar type state
#' @template templ_bind_ids
#' 
#' @template param_target
#' 
#' @export
l_bind_state_ids <- function(target) {
    
    (.loonobject(target))('bind', 'state', 'ids')
    
}

#' @templateVar type state
#' @template templ_bind_get
#' 
#' @template param_target
#'
#' @export
l_bind_state_get <- function(target, id) {
    
    (.loonobject(target))('bind', 'state', 'get', id)
    
}

#' @templateVar type state
#' @template templ_bind_delete
#' 
#' @template param_target
#' 
#' @export
l_bind_state_delete <- function(target, id) {

    obj_eval <- .loonobject(target)

    obj_eval('bind', 'state', 'delete', id)

    callbackFunctions$state[[paste(c(environment(obj_eval)$specifier,id), collapse='.')]] <- NULL
    invisible(NULL)
}

#' @templateVar type state
#' @template templ_bind_reorder
#' 
#' @template param_target
#' 
#' @export
l_bind_state_reorder <- function(target, ids) {

    (.loonobject(target))('bind', 'state', 'reorder', ids)

}

#### Item Bindings
#' @title Create a Canvas Binding
#' 
#' @description Canvas bindings are triggered by a mouse/keyboard gesture over
#'   the plot as a whole.
#' 
#' @template param_widget
#' @param tags item tags as as explained in
#'   \code{l_help("learn_R_bind.html#item-bindings")}
#' @param event event patterns as defined for Tk canvas widget
#'   \url{http://www.tcl.tk/man/tcl8.6/TkCmd/bind.htm#M5}.
#' @template param_callback
#' 
#' @details 
#' Item bindings are used for evaluating callbacks at certain mouse and/or
#' keyboard gestures events (i.e. X events) on visual items on the canvas. Items
#' on the canvas can have tags and item bindings are specified to be evaluated
#' at certain X events for items with specific tags.
#' 
#' Note that item bindings get currently evaluated in the order that they are
#' added.
#' 
#' Bindings, callbacks, and binding substitutions are described in detail in
#' loon's documentation webpage, i.e. run \code{l_help("learn_R_bind")}
#' 
#'  
#' @return item binding id
#' 
#' @seealso \code{\link{l_bind_item_ids}}, \code{\link{l_bind_item_get}},
#'   \code{\link{l_bind_item_delete}}, \code{\link{l_bind_item_reorder}} 
#' 
#' @export
l_bind_item <- function(widget, tags, event, callback) {
    id <- l_bind(widget, "item", "add", tags, event, callback)
    callbackFunctions$item[[.cbid(widget,id)]] <- callback
    return(id)
}

#' @templateVar type item
#' @template templ_bind_ids
#' 
#' @template param_widget
#' 
#' @export
l_bind_item_ids <- function(widget) {
    l_bind(widget, "item", "ids")
}

#' @templateVar type item
#' @template templ_bind_get
#'
#' @template param_widget
#'
#' @export
l_bind_item_get <- function(widget, id) {
    l_bind(widget, "item", "get", id)
}

#' @templateVar type item
#' @template templ_bind_delete
#' 
#' @template param_widget
#' 
#' @export
l_bind_item_delete <- function(widget, id) {
    l_bind(widget, "item", "delete", id)
    callbackFunctions$item[[.cbid(widget,id)]] <- NULL
    invisible(NULL)
}

#' @templateVar type item
#' @template templ_bind_reorder
#' 
#' @template param_widget
#' 
#' @description Reordering item bindings has currently no effect. Item bindings 
#'   are evaluated in the order in which they have been added.
#' 
#' @export
l_bind_item_reorder <- function(widget, ids) {
    warning('item binding order has currently no effect.')
    l_bind(widget, "item", "reorder", ids)
}

#### Glyph Bindings

#' @templateVar type glyph
#' @template templ_bind_collection
#' 
#' @template param_widget
#' 
#' @param event a vector with one or more of the following evnets: \code{'add'},
#'   \code{'delete'}, \code{'relabel'}
#'   
#' @export
l_bind_glyph <- function(widget, event, callback) {
    id <- l_bind(widget, "glyph", "add", event, callback)
    callbackFunctions$glyph[[.cbid(widget,id)]] <- callback
    return(id)
}

#' @templateVar type glyph
#' @template templ_bind_ids
#' 
#' @template param_widget
#' 
#' @export
l_bind_glyph_ids <- function(widget) {
    l_bind(widget, "glyph", "ids")
}


#' @templateVar type glyph
#' @template templ_bind_get
#'
#' @template param_widget
#' 
#' @export
l_bind_glyph_get <- function(widget, id) {
    l_bind(widget, "glyph", "get", id)
}

#' @templateVar type glyph
#' @template templ_bind_delete
#' 
#' @template param_widget
#' 
#' @export
l_bind_glyph_delete <- function(widget, id) {
    l_bind(widget, "glyph", "delete", id)
    callbackFunctions$glyph[[.cbid(widget,id)]] <- NULL
    invisible(NULL)
}

#' @templateVar type glyph
#' @template templ_bind_reorder
#' 
#' @template param_widget
#' 
#' @export
l_bind_glyph_reorder <- function(widget, ids) {
    l_bind(widget, "glyph", "reorder", ids)
}

#### Navigator Bindings

#' @templateVar type navigator
#' @template templ_bind_collection
#' 
#' @template param_widget
#' @param event a vector with one or more of the following evnets: \code{'add'},
#'   \code{'delete'}, \code{'relabel'}
#' 
#' @export
l_bind_navigator <- function(widget, event, callback) {
    id <- l_bind(widget, "navigator", "add", event, callback)
    callbackFunctions$navigator[[.cbid(widget,id)]] <- callback
    return(id)
}

#' @templateVar type navigator
#' @template templ_bind_ids
#' 
#' @template param_widget
#' 
#' @export
l_bind_navigator_ids <- function(widget) {
    l_bind(widget, "navigator", "ids")
}


#' @templateVar type navigator
#' @template templ_bind_get
#'
#' @template param_widget
#' 
#' @export
l_bind_navigator_get <- function(widget, id) {
    l_bind(widget, "navigator", "get", id)
}

#' @templateVar type navigator
#' @template templ_bind_delete
#' 
#' @template param_widget
#' 
#' @export
l_bind_navigator_delete <- function(widget, id) {
    l_bind(widget, "navigator", "delete", id)
    callbackFunctions$navigator[[.cbid(widget,id)]] <- NULL 
    invisible(NULL)
}

#' @templateVar type navigator
#' @template templ_bind_reorder
#' 
#' @template param_widget
#' 
#' @export
l_bind_navigator_reorder <- function(widget, ids) {
    l_bind(widget, "navigator", "reorder", ids)
}

#### Context Bindings

#' @templateVar type context
#' @template templ_bind_collection
#' 
#' @template param_widget
#' 
#' @param event a vector with one or more of the following evnets: \code{'add'},
#'   \code{'delete'}, \code{'relabel'}
#' 
#' @export
l_bind_context <- function(widget, event, callback) {
    id <- l_bind(widget, "context", "add", event, callback)
    callbackFunctions$context[[.cbid(widget,id)]] <- callback
    return(id)
}

#' @templateVar type context
#' @template templ_bind_ids
#' 
#' @template param_widget
#' 
#' @export
l_bind_context_ids <- function(widget) {
    l_bind(widget, "context", "ids")
}

#' @templateVar type context
#' @template templ_bind_get
#'
#' @template param_widget
#'
#' @export
l_bind_context_get <- function(widget, id) {
    l_bind(widget, "context", "get", id)
}

#' @templateVar type context
#' @template templ_bind_delete
#' 
#' @template param_widget
#' 
#' @export
l_bind_context_delete <- function(widget, id) {
    l_bind(widget, "context", "delete", id)
    callbackFunctions$context[[.cbid(widget,id)]] <- NULL
    invisible(NULL)
}

#' @templateVar type context
#' @template templ_bind_reorder
#' 
#' @template param_widget
#' 
#' @export
l_bind_context_reorder <- function(widget, ids) {
    l_bind(widget, "context", "reorder", ids)
}
