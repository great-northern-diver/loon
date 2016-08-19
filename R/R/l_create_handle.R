#' @title Create a loon object handle
#' 
#' @description This function can be used to create the loon object handles from
#'   a vector of the widget path name and the object ids (in the order of the 
#'   parent-child relationships).
#'   
#'   
#' @details loon's plot handles are useful to query and modify plot states 
#'   via the command line.
#'   
#' @templateVar page learn_R_intro
#' @templateVar section re-creating-object-handles
#' @template see_l_help
#' 
#' @param target loon object specification (e.g. \code{".l0.plot"})
#'   
#' @export
#' 
#' @examples 
#' 
#' # plot handle
#' p <- l_plot(x=1:3, y=1:3)
#' p_new <- l_create_handle(unclass(p))
#' p_new['showScales']
#' 
#' # glyph handle
#' gl <- l_glyph_add_text(p, text=LETTERS[1:3])
#' gl_new <- l_create_handle(c(as.vector(p), as.vector(gl)))
#' gl_new['text']
#' 
#' # layer handle
#' l <- l_layer_rectangle(p, x=c(1,3), y=c(1,3), color='yellow', index='end')
#' l_new <- l_create_handle(c(as.vector(p), as.vector(l)))
#' l_new['color']
#' 
#' # navigator handle
#' g <- l_graph(linegraph(completegraph(LETTERS[1:3])))
#' nav <- l_navigator_add(g)
#' nav_new <- l_create_handle(c(as.vector(g), as.vector(nav)))
#' nav_new['from']
#' 
#' # context handle
#' con <- l_context_add_context2d(nav)
#' con_new <- l_create_handle(c(as.vector(g), as.vector(nav), as.vector(con)))
#' con_new['separator']
l_create_handle <- function(target) {
    
    ## first check for loon objects
    if (is(target,'l_layer') || is(target,'l_glyph') || is(target, 'l_navigator')) {
        loon_obj <- target
        hasRecognized <- TRUE
    } else if (is(target, 'l_context')) {
        loon_obj <- target
        hasRecognized <- TRUE
    } else { 
        ## strip attributes
        specifier <- vapply(target, as.vector, character(1), USE.NAMES=FALSE)
        
        widget <- structure(as.vector(specifier[1]), class='loon')
        
        loon_obj <- switch(length(specifier),
                           '1'= {
                               widget
                           },
                           '2'= {
                               switch(substr(specifier[2], 1, 5),
                                      layer= {
                                          structure(as.vector(specifier[2]),
                                                    widget=widget,
                                                    class=c('loon', 'l_layer'))
                                      },
                                      glyph={
                                          structure(as.vector(specifier[2]),
                                                    widget=widget,
                                                    class=c('loon', 'l_glyph'))
                                      },
                                      navig={
                                          structure(as.vector(specifier[2]),
                                                    widget=widget,
                                                    class=c('loon', 'l_navigator'))
                                      },
                                      stop(paste0("Invalid target specifier: ",
                                                  target))
                           )
                       },
                       '3' = {
                           if(substr(specifier[2], 1, 5) != "navig" ||
                              substr(specifier[3], 1, 5) != "conte") {
                               stop(paste0("Invalid target specifier: ", target))
                           } else {
                               navigator <- structure(as.vector(specifier[2]),
                                                      widget=widget,
                                                      class=c('loon', 'l_navigator'))
                               structure(as.vector(specifier[3]),
                                         navigator=navigator,
                                         widget=widget,
                                         class=c('loon', 'l_context'))
                           }
                       },
                       stop(paste0("Invalid target specifier: ", target))
        )
    }
    loon_obj
}