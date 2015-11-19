
#' @export
l_info_states <- function(target, states='all') {
    
    obj_eval <- .loonobject(target, function(tclvalue) {
        sapply(as.character(tcl('dict', 'keys', tclvalue)),
               FUN=function(var){
                   sapply(c("type", "dimension", "defaultvalue", "description"),
                          function(field) {
                              v <- as.character(tcl('dict','get', tclvalue, var, field))
                              if (length(v) == 0)
                                  v <- ""
                              
                              if (field=="description")
                                  v <- paste(v, collapse=' ')
                              
                              v
                          }, simplify=FALSE)
               }, simplify=FALSE, USE.NAMES=TRUE)
    })
    
    if (length(states) == 1 && states == 'all') {
        result <- obj_eval('info', 'states')
    } else {
        result <-  obj_eval('info', 'states', states)
    }

    return(result)
}

