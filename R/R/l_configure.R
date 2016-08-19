#' @title Modify states
#' @aliases "[<-.loon"
#' @export
l_configure <- function(target, ...) {

    obj_eval <- .loonobject(target)
    
    args <- list('configure', ...)
    
    if (!is.null(args[['data']])) {
        ## convert data.frames to Tcl dicts
        args[['data']] <- l_data(args[['data']])
    }

    if (!is.null(args[['command']])) {
        if (is.function(args[['command']])) {
            callback <- .Tcl.callback(args[['command']])
            callbackFunctions$general[[paste0(paste(environment(obj_eval)$specifier,
                                                    collapse='.'),
                                              '.command')]] <- args[['command']]
            args[['command']] <- callback
        }
    }
    
    argnames <- names(args)
    if (any(substr(argnames,0,5) == 'which')) {
        argwhich <- argnames[substr(names(args),0,5) == 'which']
        
        for (w in argwhich) {
            if(!is.logical(args[[w]]) && !is.character(args[[w]])) {
                args[[w]] <- args[[w]]-1
            }
        }
    }

    ## nested lists in polygons layer or point glyphs
    if(any(environment(obj_eval)$type %in% c("layer", 'glyph'))) {
        if (!is.null(args[['x']])) {
            if (is.list(args[['x']])) {
                args[['x']] <- l_Rlist2nestedTclList(args[['x']])
            }
            if (is.list(args[['y']])) {
                args[['y']] <- l_Rlist2nestedTclList(args[['y']])
            }            
        }
    }
    
    do.call('obj_eval', args)
    
    invisible(environment(obj_eval)$loon_obj)
}
