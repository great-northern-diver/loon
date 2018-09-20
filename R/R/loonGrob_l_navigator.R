
#' @rdname loonGrob
#' 
#' @export

loonGrob.l_navigator <- function(target, name = NULL, gp = NULL, vp = NULL){
    
    widget <- target
    color <- as_hex6color(widget['color'])
    label <- widget['label']
    
    vp <- vpStack(
        plotViewport(margins = c(4.1, 5.1, 2.1, 2.1), name = "plotViewport"),
        dataViewport(xscale = c(0,1), yscale = c(0,1), name = "dataViewport")
    )
    
    gList(
        pointsGrob(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                   gp = gpar(fill = color, cex = 3.5),  
                   pch = 21, vp = vp
        ),
        if(length(label) != 0) {
            textGrob(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                     paste(label, collapse = " "),  
                     gp = gpar(fill = "black", fontsize = 9), vp = vp)
        } else NULL
    )
}