

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

#' @export
l_bind <- function(widget, ...) {
    l_throwErrorIfNotLoonWidget(widget)
    as.character(tcl(widget, "bind", ...))
}

####

#' @export
l_bind_canvas <- function(widget, event, callback) {    
    id <- l_bind(widget, "canvas", "add", event, callback)
    callbackFunctions$canvas[[.cbid(widget,id)]] <- callback
    return(id)
}

#' @export
l_bind_canvas_ids <- function(widget) {
    l_bind(widget, "canvas", "get")
}


#' @export
l_bind_canvas_get <- function(widget, id) {
    l_bind(widget, "canvas", "get", id)
}


#' @export
l_bind_canvas_delete <- function(widget, id) {
    l_bind(widget, "canvas", "delete", id)
    callbackFunctions$canvas[[.cbid(widget,id)]] <- NULL
}

#' @export
l_bind_canvas_reorder <- function(widget, ids) {
    warning('canvas binding order has currently no effect.')    
    l_bind(widget, "canvas", "reorder", ids)
}

####

#' @export
l_bind_layer <- function(widget, event, callback) {
    id <- l_bind(widget, "layer", "add", event, callback)
    callbackFunctions$layer[[.cbid(widget,id)]] <- callback
    return(id)
}

#' @export
l_bind_layer_ids <- function(widget) {
    l_bind(widget, "layer", "ids")
}


#' @export
l_bind_layer_get <- function(widget, id) {
    l_bind(widget, "layer", "get", id)
}

#' @export
l_bind_layer_delete <- function(widget, id) {
    l_bind(widget, "layer", "delete", id)
    callbackFunctions$layer[[.cbid(widget,id)]] <- NULL    
}

#' @export
l_bind_layer_reorder <- function(widget, ids) {
    l_bind(widget, "layer", "reorder", ids)
}

#### State Bindings

#' @export
l_bind_state <- function(target, event, callback) {

    obj_eval <- .loonobject(target)
    
    id <- obj_eval('bind', 'state', 'add',
                   event, .Tcl.callback(callback))
    
    callbackFunctions$state[[paste(c(environment(obj_eval)$specifier,id), collapse='.')]] <- callback
    
    return(id)
}

#' @export
l_bind_state_ids <- function(target) {
    
    (.loonobject(target))('bind', 'state', 'ids')
    
}

#' @export
l_bind_state_get <- function(target, id) {
    
    (.loonobject(target))('bind', 'state', 'get', id)
    
}


#' @export
l_bind_state_delete <- function(target, id) {

    obj_eval <- .loonobject(target)

    obj_eval('bind', 'state', 'delete', id)

    callbackFunctions$state[[paste(c(environment(obj_eval)$specifier,id), collapse='.')]] <- NULL
    
}

#' @export
l_bind_state_reorder <- function(target, ids) {

    (.loonobject(target))('bind', 'state', 'reorder', ids)

}

####

#' @export
l_bind_item <- function(widget, tags, event, callback) {
    id <- l_bind(widget, "item", "add", tags, event, callback)
    callbackFunctions$item[[.cbid(widget,id)]] <- callback
    return(id)
}

#' @export
l_bind_item_ids <- function(widget) {
    l_bind(widget, "item", "ids")
}

#' @export
l_bind_item_get <- function(widget, id) {
    l_bind(widget, "item", "get", id)
}

#' @export
l_bind_item_delete <- function(widget, id) {
    l_bind(widget, "item", "delete", id)
    callbackFunctions$item[[.cbid(widget,id)]] <- NULL
}

#' @export
l_bind_item_reorder <- function(widget, ids) {
    warning('item binding order has currently no effect.')
    l_bind(widget, "item", "reorder", ids)
}

####

#' @export
l_bind_glyph <- function(widget, event, callback) {
    id <- l_bind(widget, "glyph", "add", event, callback)
    callbackFunctions$glyph[[.cbid(widget,id)]] <- callback
    return(id)
}


#' @export
l_bind_glyph_ids <- function(widget) {
    l_bind(widget, "glyph", "ids")
}


#' @export
l_bind_glyph_get <- function(widget, id) {
    l_bind(widget, "glyph", "get", id)
}

#' @export
l_bind_glyph_delete <- function(widget, id) {
    l_bind(widget, "glyph", "delete", id)
    callbackFunctions$glyph[[.cbid(widget,id)]] <- NULL
}

#' @export
l_bind_glyph_reorder <- function(widget, ids) {
    l_bind(widget, "glyph", "reorder", ids)
}

####

#' @export
l_bind_navigator <- function(widget, event, callback) {
    id <- l_bind(widget, "navigator", "add", event, callback)
    callbackFunctions$navigator[[.cbid(widget,id)]] <- callback
    return(id)
}

#' @export
l_bind_navigator_ids <- function(widget) {
    l_bind(widget, "navigator", "ids")
}


#' @export
l_bind_navigator_get <- function(widget, id) {
    l_bind(widget, "navigator", "get", id)
}

#' @export
l_bind_navigator_delete <- function(widget, id) {
    l_bind(widget, "navigator", "delete", id)
    callbackFunctions$navigator[[.cbid(widget,id)]] <- NULL    
}

#' @export
l_bind_navigator_reorder <- function(widget, ids) {
    l_bind(widget, "navigator", "reorder", ids)
}

####

#' @export
l_bind_context <- function(widget, event, callback) {
    id <- l_bind(widget, "context", "add", event, callback)
    callbackFunctions$context[[.cbid(widget,id)]] <- callback
    return(id)
}

#' @export
l_bind_context_ids <- function(widget) {
    l_bind(widget, "context", "ids")
}


#' @export
l_bind_context_get <- function(widget, id) {
    l_bind(widget, "context", "get", id)
}

#' @export
l_bind_context_delete <- function(widget, id) {
    l_bind(widget, "context", "delete", id)
    callbackFunctions$context[[.cbid(widget,id)]] <- NULL    
}

#' @export
l_bind_context_reorder <- function(widget, ids) {
    l_bind(widget, "context", "reorder", ids)
}

