#' @rdname loonGrob
#' 
#' 
#' @examples
#'  
#' \dontrun{
#' ## Time series decomposition examples
#' 
#' decompose <- decompose(co2) 
#' # or decompose <- stl(co2, "per")
#' p <- l_plot(decompose, title = "Atmospheric carbon dioxide over Mauna Loa")
#' 
#' library(grid)
#' lgrob <- loonGrob(p)
#' grid.newpage()
#' grid.draw(lgrob)
#' }
#' 
#' @export


loonGrob.l_compound <- function(target, name = NULL, gp = NULL, vp = NULL){
    # target is a named list of class "l_compound"
    browser()
    loonWidgets <- loonGrob_getPlots(target)
    # a list of loon widgets (not loon widgets have been thrown away)
    widget <- loonWidgets$widget
    lgrob <- lapply(widget, 
                    function(w){
                        loonGrob(w)
                    }
    )
    
    # It does not produce anything, but recover the loon plots. If 
    # Sometimes we draw loonGrob without showScales or showLabels or etc, 
    # after drawing loonGrob, we need to recover the loon plot. 
    recover.args <- loonWidgets$recover.args # recover.args can be NULL, then it means the loonGrob is excatly the same with the loon plot and nothing needs recover
    loonGrob_recoverLoonWidgets(widget, recover.args)
    
    # get location and some other arrangeGrob arguments
    lgetLoc <- loonGrob_getLocations(target)
    arrangeGrob.args <- lgetLoc$arrangeGrob.args
    advanced.args <- lgetLoc$advanced.args
    
    arrangeGrob <- do.call(gridExtra::arrangeGrob, c(lgrob, arrangeGrob.args))
    advanced_arrangeGrob <- loonGrob_advancedSetting(widget, advanced.args, arrangeGrob)
    
    gTree( 
        children = gList(advanced_arrangeGrob),
        name = name, 
        vp = vp , 
        gp = gp
    )
}

loonGrob_getPlots <- function(target) {
    UseMethod("loonGrob_getPlots", target)
}

loonGrob_getPlots.default <- function(target) {
    stop("loonGrob_getPlots.default no valid inheritance")
}

loonGrob_getLocations <- function(target) {
    UseMethod("loonGrob_getLocations", target)
}

loonGrob_getLocations.default <- function(target) {
    stop("loonGrob_getLocations.default no valid inheritance")
}

loonGrob_recoverLoonWidgets <- function(widget, recover.args) {
    UseMethod("loonGrob_recoverLoonWidgets", widget)
}

loonGrob_recoverLoonWidgets.default <- function(widget, recover.args) {
    # no need to recover loon plot
    NULL
}

loonGrob_advancedSetting <- function(widget, advanced.args, arrangeGrob) {
    UseMethod("loonGrob_advancedSetting", widget)
}

loonGrob_advancedSetting.default <- function(widget, advanced.args, arrangeGrob) {
    # no advanced setting, just return arrangeGrob
    arrangeGrob
}

## l_ts time series
loonGrob_getPlots.l_ts <- function(target){
    # throw errors if not loon widget
    lapply(target, 
           function(tar){
               l_throwErrorIfNotLoonWidget(tar)
           }
    )
    widgets <- target
    showLabels <- lapply(widgets,
                          function(w){
                             w['showLabels'] 
                          }
    )
    scalesMargins <- lapply(widgets,
                            function(w){
                                w['scalesMargins'] 
                            }
    )
    new_widget <- lapply(widgets,
                        function(w){
                            w['showLabels'] <- FALSE
                            w['scalesMargins'] <- c(30, 50, 0, 0)
                            w
                        }
    )
    class(new_widget) <- class(widgets)
    list(widget = new_widget,
         recover.args = list(showLabels = showLabels,
                             scalesMargins = scalesMargins
                             )
         )
}

loonGrob_getLocations.l_ts <- function(target){
    arrangeGrob.args <- list(
        name =  "l_ts",
        nrow = 4,
        ncol = 1,
        heights = rep(1,4),
        bottom = "time",
        top = target$original['title']
    )
    
    advanced.args <- list(
        is_multiple_xlabels = FALSE,
        is_multiple_ylabels = TRUE,
        base_size = 8
    )
    
    list(
        arrangeGrob.args = arrangeGrob.args,
        advanced.args =  advanced.args
    )
}

loonGrob_recoverLoonWidgets.l_ts <- function(widget, recover.args){
    if (!is.null(recover.args)) {
        len_widget <- length(widget)
        showLabels <- recover.args$showLabels
        if(len_widget != length(showLabels)) {
            stop("the length of showLabels arguments should be equal to the length of widgets")
        }
        scalesMargins <- recover.args$scalesMargins
        if(len_widget != length(scalesMargins)) {
            stop("the length of scalesMargins arguments should be equal to the length of widgets")
        }
        lapply(seq_len(len_widget),
               function(i){
                   widgeti <- widget[[i]]
                   l_configure(widgeti,
                               showLabels = showLabels[[i]], 
                               scalesMargins = scalesMargins[[i]])
               }
        )
    } else NULL
}

loonGrob_advancedSetting.l_ts <- function(widget, advanced.args, arrangeGrob){
    # the following will show how to place multiple labels around gTree
    if(!is.null(advanced.args)) {
        loonGrob_setLabels(widget, advanced.args, arrangeGrob)    
    } else arrangeGrob
}


loonGrob_setLabels <- function(widget, advanced.args, arrangeGrob) {
    
    # want to draw multiple xlabels?
    is_multiple_xlabels <- if(is.null(advanced.args$is_multiple_xlabels)) FALSE else {
        if(is.logical(advanced.args$is_multiple_xlabels)) advanced.args$is_multiple_xlabels else {
            warning("is_multiple_xlabels is a boolean variable")
            FALSE
        }
    }
    
    # want to draw multiple ylabels?
    is_multiple_ylabels <- if(is.null(advanced.args$is_multiple_ylabels)) FALSE else {
        if(is.logical(advanced.args$is_multiple_ylabels)) advanced.args$is_multiple_ylabels else {
            warning("is_multiple_ylabels is a boolean variable")
            FALSE
        }
    }
    
    if(!is_multiple_xlabels & !is_multiple_ylabels) {
        arrangeGrob
    } else if(!is_multiple_xlabels & is_multiple_ylabels) {
        theme <- gridExtra::ttheme_minimal(base_size = advanced.args$base_size, 
                                           core = list(fg_params=list(rot=90)))
        ylabels <- gridExtra::tableGrob(names(arrangeGrob$grobs)[order(arrangeGrob$layout$t)],
                                        theme = theme)
        cbind(ylabels, arrangeGrob, size = "last")
    } else if(is_multiple_xlabels & !is_multiple_ylabels) {
        theme <- gridExtra::ttheme_minimal(base_size = advanced.args$base_size)
        xlabels <- gridExtra::tableGrob(
            matrix(
                names(arrangeGrob$grobs)[order(arrangeGrob$layout$b)], 
                ncol = ncol(arrangeGrob)),
            theme = theme)
        rbind(arrangeGrob, xlabels, size = "first")
    } else {
        theme <- gridExtra::ttheme_minimal(base_size = advanced.args$base_size, 
                                           core = list(fg_params=list(rot=90)))
        ylabels <- gridExtra::tableGrob(names(arrangeGrob$grobs)[order(arrangeGrob$layout$t)],
                                        theme = theme)
        cb <- cbind(arrangeGrob, ylabels, size = "last")
        # the very left is ylabels
        xlabels <- gridExtra::tableGrob(
            matrix(
                c("" , names(arrangeGrob$grobs)[order(arrangeGrob$layout$b)]), 
                ncol = ncol(arrangeGrob) + 1),
            theme = theme)
        rbind(cb, xlabels, size = "first")
    }
}
