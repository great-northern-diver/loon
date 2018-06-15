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
    if (is(target,'loon')) {
        loon_obj <- target
        hasRecognized <- TRUE
    } else { 
        ## strip attributes
        specifier <- vapply(target, as.vector, character(1), USE.NAMES=FALSE)
        
        widget <- specifier[1]
        
        if (!l_isLoonWidget(widget)) stop(widget, " is not a valid loon widget")
        
        loon_obj <- if (length(specifier) == 1) {
            
            cl <- as.character(tcl("info", "object", "class", widget))
            
            wcl <- switch(
                cl,
                "::loon::classes::Scatterplot_Widget" = "l_plot",
                "::loon::classes::Histogram_Widget" = "l_hist",
                "::loon::classes::Serialaxes_Widget" = "l_serialaxes",
                "::loon::classes::Graph_Widget" = "l_graph",
                character(0)
            )
            structure(widget, class = c(wcl, "loon"))
        } else if (length(specifier) == 2) {
            
            spec_2 <- specifier[2]
            spec_2_short <- substr(spec_2, 1, 5)
            if ( spec_2_short == "layer" || spec_2 == "model") {
                structure(as.vector(spec_2),
                          widget = widget,
                          class = c(paste0("l_layer_", l_layer_getType(widget, specifier[2])), 'l_layer', 'loon'))
            } else if (spec_2 == "root") {
                structure("root", widget=as.vector(widget), class=c("l_layer_group", "l_layer", "loon"))
            } else if (spec_2_short == "glyph") {
                structure(as.vector(specifier[2]),
                          widget= widget,
                          class=c(paste0("l_glyph_", l_glyph_getType(widget, spec_2)), 'l_glyph', 'loon'))
                
            } else if (spec_2_short == "navig") {
                structure(as.vector(specifier[2]),
                          widget=widget,
                          class=c('l_navigator', 'loon'))
            } else {
                stop(paste0("Invalid target specifier: ", target))  
            }
        } else if (length(specifier) == 3) {
            if(substr(specifier[2], 1, 5) != "navig" ||
               substr(specifier[3], 1, 5) != "conte") {
                stop(paste0("Invalid target specifier: ", target))
            } else {
                navigator <- structure(as.vector(specifier[2]),
                                       widget=widget,
                                       class=c('l_navigator', 'loon'))
                structure(as.vector(specifier[3]),
                          navigator=navigator,
                          widget=widget,
                          class=c('l_context', 'loon'))
            }
        } else {
            stop(paste0("Invalid target specifier: ", target))
        }
    }
    
    loon_obj
}
