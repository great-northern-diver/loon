#' Create a grob of serialaxes layer
#' 
#' @import grid
#' 
#' @export
#' 
#' @examples 
#' 
#' s <- l_serialaxes(data=oliveAcids, color=olive$Area, title="olive data")
#' s['axesLayout'] <- 'parallel'
#' 
#' library(grid)
#' grid.newpage(); grid.loon(s)

loonGrob.l_serialaxes <- function(widget, name = NULL, gp = NULL, vp = NULL){
    
    # show or not
    showGuides <- widget['showGuides']
    showAxes <- widget['showAxes']
    showAxesLabels <- widget['showAxesLabels']
    showLabels <- widget['showLabels']
    showArea <- widget['showArea']
    # which scaled method
    scaling <- widget['scaling']
    # title
    title <- widget['title']
    
    # sequences
    seqName <- widget['sequence']
    len.xaxis <- length(seqName)
    
    # active or not
    active <- widget['active']
    display_order <- get_model_display_order(widget)
    active_displayOrder <- display_order[active]
    dat <- sapply( widget['data'], as.numeric)  # convert to numeric if not
    activeData <- dat[active_displayOrder , seqName] 
    
    if(is.null(activeData)) {
        n <- NULL
        scaledActiveData <- NULL
    } else {
        activeData <- matrix(activeData, ncol = len.xaxis)
        n <- dim(activeData)[1]
        
        scaledActiveData <- switch(scaling, 
                                   "variable" = {
                                       apply2min <- apply(dat, 2, "min")
                                       apply2max <- apply(dat, 2, "max")
                                       t(
                                           (t(activeData) - apply2min)/ 
                                               (apply2max  - apply2min) 
                                       )
                                   }, 
                                   "observation" = {
                                       apply1min <- apply(activeData, 1, "min")
                                       apply1max <- apply(activeData, 1, "max")
                                       (activeData - apply1min ) / (apply1max - apply1min )
                                   }, 
                                   "data" = {
                                       minD <- min(dat)
                                       maxD <- max(dat)
                                       (activeData - minD)/ (maxD - minD)
                                   }, 
                                   "none" = NULL)
    }
    
    
    activeSelected <- widget['selected'][active_displayOrder]
    activeColor <- get_display_color( as_hex6color(widget['color'][active_displayOrder]), activeSelected)
    activeLinewidth <- widget['linewidth'][active_displayOrder]
    
    if (widget['axesLayout'] == "parallel") {
        
        xlim <- ylim <- c(-0.1, 1.12)
        xaxis <- seq(0, 1, length.out =  len.xaxis)
        if( is.null(scaledActiveData) ) parallelGrob <- grob(name = name, gp = gp)
        else {
            
            parallelGrob <- gTree( children = 
                                       gList(
                                           do.call(
                                               gList,
                                               lapply(seq_len(n), function(i){
                                                   if (showArea) {
                                                       polygonGrob(
                                                           x = unit( c(xaxis, rev(xaxis) ), "native"), 
                                                           y = unit( c(scaledActiveData[i, ], rep(0, len.xaxis)), "native"),
                                                           gp = gpar(fill = activeColor[i], 
                                                                     col = NA)
                                                       )
                                                   } else {                                
                                                       linesGrob(
                                                           x = unit(xaxis, "native"), y = unit(scaledActiveData[i, ], "native"),
                                                           gp = gpar(col = activeColor[i], 
                                                                     lwd = activeLinewidth[i])
                                                       )
                                                   } 
                                               })
                                           )
                                       ),
                                   name = "parallelAxes"
            )
        }
        
        gTree(
            children = gList(
                
                if (showGuides){
                    
                    yaxis <- grid.pretty(ylim)
                    len.yaxis <- length(yaxis)
                    gTree(children = do.call(
                        gList, 
                        lapply(1:(len.xaxis + len.yaxis + 1), function(i) {
                            if(i == 1){
                                rectGrob(gp = gpar(col = NA, fill = "#EBEBEB" ))
                                
                            }else if( i > 1 && i<= (1 + len.xaxis)){
                                if(showAxes) linesGrob(x = unit(rep(xaxis[i - 1],2 ), "native"), 
                                                       y =  unit(c(0, 1), "native"),
                                                       gp = gpar(col =  "white", lwd = 2))
                            } else {
                                linesGrob(x = unit(c(0, 1), "native"),
                                          y =  unit(rep(yaxis[i - (1 + len.xaxis)],2 ), "native"), 
                                          gp = gpar(col ="white", lwd = 2))
                            }
                        })),
                        name = "guide")
                    
                } else {
                    if(showAxes){
                        gTree ( children =  do.call(
                            gList, 
                            lapply(1:(len.xaxis), function(i) {
                                linesGrob(x = unit(rep(xaxis[i],2 ), "native"), 
                                          y =  unit(c(0, 1), "native"),
                                          gp = gpar(col =  "black", lwd = 2))
                            })), 
                            name = "guide") 
                    }
                }, 
                
                if (showLabels & title != "") {
                    textGrob(title, name = "title", y = unit(1, "npc") - unit(.8, "lines"),
                             gp = gpar(fontsize = 18, fontface="bold"), vjust = .5)
                },
                
                if( showAxesLabels ){
                    gTree(
                        children =    do.call(
                            gList, 
                            lapply(1:(len.xaxis), function(i) {
                                textGrob(seqName[i], x = unit(xaxis[i], "native"), y = unit(0, "npc") + unit(1.2, "lines"),
                                         gp = gpar(fontsize = 9), vjust = .5)
                            })), 
                        name = "axes labels"
                    )
                }, 
                clipGrob(name = "clip"),
                parallelGrob
            ),
            vp = vpStack(
                plotViewport(margins = rep(0, 4), name = "plotViewport"),
                dataViewport(xscale = xlim, yscale = ylim, name = "dataViewport")
            ),
            name = "l_serialaxes"
        )
        
    } else if (widget['axesLayout'] == "radial") {
        
        xlim <- ylim <- c(-0.2, 1.2) 
        angle <- seq(0, 2*pi, length.out = len.xaxis + 1)[1:len.xaxis]
        if( is.null(scaledActiveData) ) radialGrob <- grob(name = name, gp = gp)
        else {
            
            radialGrob <- gTree(
                children = do.call(
                    gList,
                    lapply(seq_len(n), function(i){
                        radialxais <- 0.5 + 0.5 * scaledActiveData[i,] * cos(angle)
                        radialyais <- 0.5 + 0.5 * scaledActiveData[i,] * sin(angle)
                        if(showArea){
                            polygonGrob(
                                x = unit(  c(radialxais, radialxais[1]), "native"), 
                                y = unit(  c(radialyais, radialyais[1]), "native"),
                                gp = gpar(fill = activeColor[i], 
                                          col = NA)
                            )
                        } else {
                            linesGrob(
                                x = unit( c(radialxais,  radialxais[1]), "native"), 
                                y = unit( c(radialyais,  radialyais[1]), "native"),
                                gp = gpar(col = activeColor[i],
                                          lwd = activeLinewidth[i])
                            )
                        }
                    })
                ),
                name = "radialAxes"
            )
        }
        
        gTree(
            children = gList(
                if (showGuides) {
                    gTree(children = gList(
                        rectGrob(gp = gpar(col = NA, fill = "#EBEBEB" )),
                        polygonGrob(unit( 0.5 + 0.5 * cos(seq(0, 2*pi, length=101)), "native"), 
                                    unit( 0.5 + 0.5 * sin(seq(0, 2*pi, length=101)), "native"), 
                                    gp = gpar(fill = NA, col = "white",
                                              lwd = 2) ),
                        if(showAxes){
                            
                            polylineGrob( x = unit( c(rep(0.5, len.xaxis) ,0.5 + 0.5 * cos(angle)), "native" ), 
                                          y = unit( c(rep(0.5, len.xaxis) ,0.5 + 0.5 * sin(angle)), "native" ),
                                          id = rep(1:len.xaxis, 2),
                                          gp = gpar(col =  "white", lwd = 2))
                        }
                    ),
                    name = "guide")
                    
                } else {
                    if(showAxes){
                        
                        polylineGrob( x = unit( c(rep(0.5, len.xaxis) ,0.5 + 0.5 * cos(angle)), "native" ), 
                                      y = unit( c(rep(0.5, len.xaxis) ,0.5 + 0.5 * sin(angle)), "native" ),
                                      id = rep(1:len.xaxis, 2),
                                      gp = gpar(col =  "black", lwd = 2), name = "guide")
                        
                    }
                },
                
                if (showLabels & title != "") {
                    
                    textGrob(title, name = "title", y = unit(1, "npc") - unit(.8, "lines"),
                             gp = gpar(fontsize = 18, fontface="bold"), vjust = .5)
                    
                },
                
                if (showAxesLabels) {
                    
                    gTree(
                        children =    do.call(
                            gList, 
                            lapply(1:(len.xaxis), function(i) {
                                textGrob(seqName[i], x = unit( 0.5 +  0.545 * cos(angle[i]), "native"), 
                                         y = unit( 0.5 + 0.545 * sin(angle[i] ), "native"),
                                         gp = gpar(fontsize = 9), vjust = .5)
                            })), 
                        name = "axes labels"
                    )
                    
                }, 
                clipGrob(name = "clip"),
                radialGrob
            ),
            vp = vpStack(
                plotViewport(margins = rep(0, 4), name = "plotViewport"),
                dataViewport(xscale = xlim, yscale = ylim, name = "dataViewport")
            ),
            name = "l_serialaxes"
        )
    } else grob(name = name, gp = gp, vp = vp)
}
