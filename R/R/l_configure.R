#' @title Modify one or multiple plot states
#' 
#' @aliases [<-.loon
#' 
#' @description All of loon's displays have plot states. Plot states specify 
#'   what is displayed, how it is displayed and if and how the plot is linked 
#'   with other loon plots. Layers, glyphs, navigators and contexts have states 
#'   too (also refered to as plot states). This function modifies one or 
#'   multiple plot states.
#'   
#' @template param_target
#' @param ... state=value pairs
#'   
#'   
#' @seealso \code{\link{l_cget}}, \code{\link{l_info_states}},
#'   \code{\link{l_create_handle}}
#'   
#' @aliases "[<-.loon"
#' @export
#' 
#' @examples 
#' p <- l_plot(iris, color = iris$Species)
#' l_configure(p, color='red')
#' p['size'] <- ifelse(iris$Species == "versicolor", 2, 8)
l_configure <- function(target, ...) {
    UseMethod("l_configure", target)
}

#' @export
l_configure.loon <- function(target, ...) {
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
    
    invisible(target)
}

#' @export
l_configure.character <- function(target, ...) {
    widget <- try(l_create_handle(target), silent = TRUE)
    if ("try-error" %in% class(widget)) {
        stop(paste0(target, " is not configurable via l_configure()"))
    }
    else {
        l_configure(widget, ...)
    }
}
