#' @title Query a Plot State
#' 
#' @aliases [.loon
#'  
#' @description All of loon's displays have plot states. Plot states specify 
#'   what is displayed, how it is displayed and if and how the plot is linked 
#'   with other loon plots. Layers, glyphs, navigators and contexts have states 
#'   too (also refered to as plot states). This function queries a single plot 
#'   state.
#'   
#' @template param_target
#' @param state state name
#'   
#' @seealso \code{\link{l_configure}}, \code{\link{l_info_states}},
#'   \code{\link{l_create_handle}}
#'   
#' @export
#' 
#' @examples 
#' p <- l_plot(iris, color = iris$Species)
#' l_cget(p, "color")
#' p['selected']
l_cget <- function(target, state) {

    obj_eval <- .loonobject(target, as.character)

    if(substr(state,1,1) != "-") {
        dash_state <- paste("-", state, sep='')
    } else {
        dash_state <- state
        state <- substring(state, 2)
    }

    type <- obj_eval('info', 'stateType', state)
    
    if (type %in% c("double", "positive_double", "integer",
                    "positive_integer", "tempcoords", "in_unit_interval")) {
        environment(obj_eval)$convert <- function(x) {as.numeric(as.character(x))}
    } else if (type == "boolean") {
        environment(obj_eval)$convert <- function(x) {as.logical(as.character(x))}
    } else if (type == "data") {
        environment(obj_eval)$convert <- function(result) { 
            ## create a data.frame with all characters
            vars <- tcl('dict', 'keys', result)
            l <- sapply(as.character(tcl('dict','keys',result)),
                        FUN=function(var){
                            as.character(tcl('dict','get', result, var))
                        }, simplify=FALSE, USE.NAMES=TRUE)
            
            l[['stringsAsFactors']] <- FALSE
            
            do.call(data.frame, l)
        }          
    } else if (type == "nested_double") {
        environment(obj_eval)$convert <- function(result) {
            dim <- as.numeric(tcl('llength', result))
            out <- vector(mode='list', length=dim)
            for (i in 1:dim) {
               out[[i]] <- as.numeric(tcl('lindex', result, i-1)) 
            }
            out
        }
    } else if (state %in% c("n","p")) {
        environment(obj_eval)$convert <- function(x) {as.numeric(as.character(x))}
    } else if (type == "nested_double") {
        environment(obj_eval)$convert <- l_nestedTclList2Rlist
    } else {
        dim <- obj_eval('info', 'stateDimension', state)
        if (dim == "1") {
            environment(obj_eval)$convert <- function(x) {
                paste(as.character(x), collapse=' ')
            }
        }
     }
    
    obj_eval('cget', dash_state)    
}
