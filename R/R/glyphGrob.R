# glyph grob
textGlyphGrob <- function(widget, layerid, glyph, states){
    active <- states$active
    if(l_layer_isVisible(widget, layerid) & length(states$x[active])!=0 & length(states$y[active])!=0 ){
        selected <- states$selected
        sel_color <- as.character(.Tcl("set loon::Options(select-color)"))
        label <- glyph['text']
        # the size is an arbitrary choice
        tGrob <- textGrob(label = label[active], x = states$x[active], 
                          y = states$y[active],
                          gp=gpar(fontsize= 7, col=states$color[active] ))
        
        if(length(states$x[selected]) != 0 & length(states$y[selected]) != 0 ){
            gTree(
                children = gList(
                    tGrob,
                    textGrob(
                        label = label[selected],
                        x = states$x[selected], y = states$y[selected],
                        gp = gpar(fontsize = 7, col = sel_color)
                    )
                )
            )
        } else tGrob
    } else NULL
}

pointrangeGlyphGrob <- function(widget, layerid, glyph, states){
    active <- states$active
    if(l_layer_isVisible(widget, layerid) & length(states$x[active])!=0 & length(states$y[active])!=0 ){
        sel_color <- as.character(.Tcl("set loon::Options(select-color)"))
        selected <- states$selected
        ymin <- glyph['ymin']
        ymax <- glyph['ymax']
        showArea <- glyph['showArea']
        linewidth <- glyph['linewidth']
        
        pointrangeTree <- function(applyTerm, col = NULL){
            gTree(children = do.call(gList, 
                                     lapply(applyTerm, function(i){
                                         if(active[i]){
                                             gList(pointsGrob( x = states$x[i], y = states$y[i],
                                                               gp = gpar(col = if(is.null(col)) states$color[i] else col, cex = 1,
                                                                         fill = if(showArea) NULL else 
                                                                             if(is.null(col)) states$color[i] else col
                                                               ),
                                                               pch = if(showArea) 21 else 19), 
                                                   linesGrob( x = rep(states$x[i], 2), 
                                                              y = unit(c(ymin[i], ymax[i]), "native"), 
                                                              gp = gpar(col = if(is.null(col)) states$color[i] else col, 
                                                                        lwd = linewidth[i]))
                                             )
                                         } else NULL
                                     })))
        }
        
        pT <- pointrangeTree(applyTerm = 1:length(active))
        if(length(states$x[selected]) != 0 & length(states$y[selected]) != 0 ){
            selectedIndex <- which(selected == T) 
            gTree(children = gList(
                pT, 
                pointrangeTree(applyTerm = selectedIndex, col = sel_color)
            ))
        } else pT
    } else NULL
}

imageGlyphGrob <- function(widget, layerid, glyph, states, images){
    active <- states$active
    if(l_layer_isVisible(widget, layerid) & length(states$x[active])!=0 & length(states$y[active])!=0 ){
        sel_color <- as.character(.Tcl("set loon::Options(select-color)"))
        selected <- states$selected
        imageTree <- function(applyTerm ,col = NULL){
            gTree(children = do.call(gList, 
                                     lapply(applyTerm, function(i){
                                         if(active[i]){
                                             gList(
                                                 rectGrob(x = states$x[i], y =  states$y[i],
                                                          width = unit(0.7, "native"), height = unit(0.7, "native"), 
                                                          gp = gpar(fill = if(is.null(col)) states$color[i] else col, 
                                                                    col = if(is.null(col)) states$color[i] else col ) ), 
                                                 rasterGrob(images[[i]], x = states$x[i], y =  states$y[i], 
                                                            width = unit(0.6, "native"), height = unit(0.6, "native"))
                                             )
                                         } else NULL
                                     })))
        }
        
        iT <- imageTree(1:length(active))
        if(length(states$x[selected]) != 0 & length(states$y[selected]) != 0 ){
            selectedIndex <- which(selected == T) 
            gTree(children = gList(
                iT, 
                imageTree(selectedIndex, col = sel_color)
            ))
        } else iT
    } else NULL
}

serialaxesGlyphGrob <- function(widget, layerid, glyph, states){
    active <- states$active
    if(l_layer_isVisible(widget, layerid) & length(states$x[active])!=0 & length(states$y[active])!=0 ){
        sel_color <- as.character(.Tcl("set loon::Options(select-color)"))
        selected <- states$selected
        # show or not
        showAxes <- glyph['showAxes']
        showEnclosing <- glyph['showEnclosing']
        showArea <- glyph['showArea']
        # scaling way
        scaling <- glyph['scaling']
        # line width
        linewidth <- glyph['linewidth']
        # bbox color
        bboxColor <- as_hex6color(glyph['bboxColor'])
        # axes color
        axesColor <- as_hex6color(glyph['axesColor'])
        # parallel or radial
        axesLayout <- glyph['axesLayout']
        dat <- sapply( glyph['data'], as.numeric)  # convert to numeric if not
        dimension <- dim(dat)[2]
        apply2min <- apply(dat, 2, "min")
        apply2max <- apply(dat, 2, "max")
        apply1min <- apply(dat, 1, "min")
        apply1max <- apply(dat, 1, "max")
        scaledData <- switch(scaling, 
                             "variable" = t( ( t(dat) - apply2min)/ 
                                                 (apply2max  - apply2min) ), 
                             "observation" = (dat - apply1min ) / (apply1max - apply1min ), 
                             "data" = (dat - min(dat))/ (max(dat) - min(dat)), 
                             "none" = NULL)
        # arbitrary choice of 
        scaleX <- diff(c(widget['panX'], widget['panX'] + widget['deltaX']/widget['zoomX']))/10
        scaleY <- diff( c(widget['panY'], widget['panY'] + widget['deltaY']/widget['zoomY']))/10
        
        parallelTree <- function(applyTerm, col = NULL) {
            xaxis <- seq(-0.5 * scaleX, 0.5 * scaleX, length.out = dimension)
            gTree ( children = do.call(gList, 
                                       lapply(applyTerm, function(i){
                                           if(active[i]){
                                               yaxis <- (scaledData[i, ] - 0.5) * scaleY
                                               xpos <- as.numeric(states$x[i])
                                               ypos <- as.numeric(states$y[i])
                                               x <- xpos + xaxis
                                               y <- ypos + yaxis
                                               gList(
                                                   if(showEnclosing) {
                                                       polylineGrob(x=unit( (c(0, 0, 1, 0, 0, 1, 1, 1) - 0.5) * scaleX + 
                                                                                xpos, "native"),
                                                                    y=unit( (c(0, 0, 0, 1, 1, 0, 1, 1) - 0.5) * scaleY + 
                                                                                ypos, "native"),
                                                                    id=rep(1:4, 2),
                                                                    gp=gpar(col = bboxColor)) 
                                                   } else NULL ,
                                                   if(showAxes) {
                                                       polylineGrob(x = unit( rep(x, each = 2), "native"), 
                                                                    y = unit( rep(c(ypos - 0.5 * scaleY, 
                                                                                    ypos + 0.5 * scaleY), dimension), "native"),
                                                                    id = rep(1:dimension, each = 2), 
                                                                    gp = gpar(col = axesColor))
                                                   } else NULL ,
                                                   if(showArea) {
                                                       polygonGrob(x = unit(c(x, rev(x)), "native"), 
                                                                   y = unit(c(y, rep(ypos - 0.5 * scaleY, dimension)), "native"), 
                                                                   gp = gpar(fill = if(is.null(col)) states$color[i] else col, col = NA))
                                                   } else {
                                                       linesGrob(x = unit(x, "native"), 
                                                                 y = unit(y, "native"), 
                                                                 gp = gpar(col = if(is.null(col)) states$color[i] else col))
                                                   }
                                               )
                                           } else NULL
                                       })))
        }
        radialTree <- function(applyTerm, col = NULL){
            angle <- seq(0, 2*pi, length.out = dimension + 1)[1:dimension]
            gTree ( children = do.call(gList, 
                                       lapply(applyTerm, function(i){
                                           if(active[i]){
                                               xpos <- as.numeric(states$x[i])
                                               ypos <- as.numeric(states$y[i])
                                               radialxais <- scaleX * scaledData[i,] * cos(angle)
                                               radialyais <- scaleY * scaledData[i,] * sin(angle)
                                               x <- xpos + radialxais
                                               y <- ypos + radialyais
                                               gList(
                                                   if(showArea) {
                                                       polygonGrob(x = unit(c(x, x[1]), "native"), 
                                                                   y = unit(c(y, y[1]), "native"), 
                                                                   gp = gpar(fill = if(is.null(col)) states$color[i] else col, col = NA))
                                                   } else {
                                                       linesGrob(x = unit(c(x, x[1]), "native"), 
                                                                 y = unit(c(y, y[1]), "native"), 
                                                                 gp = gpar(col = if(is.null(col)) states$color[i] else col))
                                                   },
                                                   if(showEnclosing) {
                                                       polygonGrob(unit( xpos + scaleX * cos(seq(0, 2*pi, length=101)), "native"), 
                                                                   unit( ypos + scaleY * sin(seq(0, 2*pi, length=101)), "native"), 
                                                                   gp = gpar(fill = NA, col = bboxColor) )
                                                   } else NULL ,
                                                   if(showAxes) {
                                                       
                                                       polylineGrob( x = unit( c(rep(xpos, dimension) ,xpos + scaleX * cos(angle)), "native" ), 
                                                                     y = unit( c(rep(ypos, dimension) ,ypos + scaleY * sin(angle)), "native" ),
                                                                     id = rep(1:dimension, 2),
                                                                     gp = gpar(col =  axesColor))
                                                   } else NULL 
                                               )
                                           } else NULL
                                       })))
        }
        if(axesLayout == "parallel"){
            pT <- parallelTree(1:length(active), col = NULL)
            if(length(states$x[selected]) != 0 & length(states$y[selected]) != 0 ){
                selectedIndex <- which(selected == T)
                gTree(children = gList(
                    pT, 
                    parallelTree(selectedIndex, col = sel_color)
                ))
            } else pT
        } else {
            rT <- radialTree(1:length(active), col = NULL)
            if(length(states$x[selected]) != 0 & length(states$y[selected]) != 0 ){
                selectedIndex <- which(selected == T)
                gTree(children = gList(
                    rT, 
                    radialTree(selectedIndex, col = sel_color)
                ))
            } else rT
        }
        
    } else NULL
}

polygonGlyphGrob <- function(widget, layerid, glyph, states){
    active <- states$active
    if(l_layer_isVisible(widget, layerid) & length(states$x[active])!=0 & length(states$y[active])!=0 ){
        selected <- states$selected
        sel_color <- as.character(.Tcl("set loon::Options(select-color)"))
        showArea <- glyph['showArea']
        linewidth <- glyph['linewidth']
        
        # the size is an arbitrary choice
        polygonTree <- function(applyTerm, col = NULL){
            gTree(children = do.call(gList, 
                                     lapply(applyTerm, function(i){
                                         if(active[i]){
                                             x <- as.numeric(states$x[i]) + glyph['x'][[i]]/15
                                             y <- as.numeric(states$y[i]) - glyph['y'][[i]]/15
                                             if(showArea[i]){
                                                 polygonGrob( x = unit(x , "native"), 
                                                              y = unit(y , "native"),
                                                              gp = gpar(
                                                                  fill = if(is.null(col)) states$color[i] else col, 
                                                                  col =  if(is.null(col)) states$color[i] else col, 
                                                                  lwd = states$linewidth[i]
                                                              )
                                                 )
                                             } else {
                                                 polylineGrob( x = unit(c(x, x[1]), "native"), 
                                                               y = unit(c(y, y[1]), "native"),
                                                               gp = gpar(
                                                                   fill = if(is.null(col)) states$color[i] else col, 
                                                                   col =  if(is.null(col)) states$color[i] else col, 
                                                                   lwd = states$linewidth[i]
                                                               )
                                                 )
                                             }
                                         } else NULL
                                     }) 
            ))
        }
        pT <- polygonTree(1:length(active))
        if(length(states$x[selected]) != 0 & length(states$y[selected]) != 0 ){
            selectedIndex <- which(selected == T)
            gTree(
                children = gList(
                    pT,
                    polygonTree(selectedIndex, col = sel_color)
                ) 
            )
        } else pT
    } else NULL
}

