#' Create a grid grob from a loon widget handle
#' 
#' Grid grobs are useful to create publication quality graphics.
#' 
#' @template param_widget
#' 
#' @return a grid grob
#' 
#' @import grid
#' 
#' @export
#' 
#' @examples 
#' 
#' library(grid)
#' widget <- with(iris, l_plot(Sepal.Length, Sepal.Width))
#' loonGrob <- loon2grid(widget)
#' grid.ls(loonGrob, viewports=TRUE, fullNames=TRUE)
#' grid.newpage(); grid.draw(loonGrob)
#' 
#' widget['title'] <- "Iris Data"
#' grid.edit("Scatterplot model", gp=gpar(alpha = 0.6))
#' 
#' \dontrun{
#' demo("l_glyph_sizes", ask = FALSE)
#'   
#' p <- l_create_handle(".l0.plot")
#' 
#' m <- 6  
#' loonGrob <- loon2grid(p)
#' grid.newpage()
#' grid.draw(loonGrob)
#' }
#' 

loon2grid <- function(widget, ...) {
    UseMethod("loon2grid")
}

#' @export
loon2grid.default <- function(widget, margins = NULL, border = NULL, ...) {
    
    l_isLoonWidget(widget) || stop("widget does not seem to exist") 
    
    isSerialaxes <-  "serialaxes" %in% unlist(strsplit(as.character( widget), split = "[.]"))
    
    if(isSerialaxes){
      
      # serial axes is isolate from other layers
      serialaxesGrob(widget)
      
    } else {
        
        xlim <- c(widget['panX'], widget['panX'] + widget['deltaX']/widget['zoomX'])
        ylim <- c(widget['panY'], widget['panY'] + widget['deltaY']/widget['zoomY'])
        
        
        swapAxes <- widget['swapAxes']
        showScales <- widget['showScales']
        
        showLabels <- widget['showLabels']
        showGuides <- widget['showGuides']
        
        if (swapAxes) {
            tmp <- xlim
            xlim <- ylim
            ylim <- tmp
        }
        
        title <- widget['title']
        
        if(is.null(margins)){
            margins <- if (showScales) c(4.1, 5.1, 2.1, 2.1) else rep(2.1, 4)
            if (title != "" && showLabels) margins[3] <- 3.1
        }
        
        if(is.null(border)){
            border <- as_hex6color(widget['foreground'])
        }
        xylab_dist <- if (showScales) c(-3, -4.2) else c(-1, -1)
        
        if (!swapAxes) {
            xlabelGrob <- textGrob(widget['xlabel'], y = unit(xylab_dist[1], "lines"), name = "x label")
            ylabelGrob <- textGrob(widget['ylabel'], x = unit(xylab_dist[2], "lines"), rot = 90, name = "y label")
        } else {
            xlabelGrob <- textGrob(widget['xlabel'], x = unit(xylab_dist[2], "lines"), rot = 90, name = "x label")
            ylabelGrob <- textGrob(widget['ylabel'], y = unit(xylab_dist[1], "lines"), name = "y label")
        }
        gTree(
            children = gList(
                if (showLabels) {
                    gList(
                        xlabelGrob,
                        ylabelGrob,
                        if (title != "") {
                            textGrob(title, name = "title", y = unit(1, "npc") + unit(.8, "lines"),
                                     gp = gpar(fontsize = 18, fontface="bold"), vjust = .5)
                        } else NULL )
                } else NULL,
                if (showGuides){
                    xaxis <- grid.pretty(xlim)
                    len.xaxis <- length(xaxis)
                    yaxis <- grid.pretty(ylim)
                    len.yaxis <- length(yaxis)
                    gTree(children = gList(
                      rectGrob(gp = gpar(col = border, 
                                         fill = as_hex6color(widget['guidesBackground']) )),
                      do.call(
                      gList, 
                      lapply(1:(len.xaxis + len.yaxis), function(i) {
                        if(i <=  len.xaxis ){
                          linesGrob(x = unit(rep(xaxis[i],2 ), "native"), 
                                    y =  unit(c(0.003,0.997), "npc"),
                                    gp = gpar(col = as_hex6color(widget['guidelines']), lwd = 2))
                        }else {
                          linesGrob(x = unit(c(0.003,0.997), "npc") ,
                                    y =  unit(rep(yaxis[i - len.xaxis],2 ), "native"), 
                                    gp = gpar(col = as_hex6color(widget['guidelines']), lwd = 2))
                        }
                      }))),
                      name = "guide")
                } else {
                    rectGrob(gp = gpar(col = border, 
                                       fill = as_hex6color(widget['background'])),
                             name = "bbox") },
                if (showScales) {
                    gList(
                        xaxisGrob( name = "x axis"),
                        yaxisGrob( name = "y axis")
                    )  
                } else NULL,
                clipGrob(name = "clip"),
                getGrob.group(widget, "root", ...),
                # draw boundary
                polylineGrob(x=unit( c(0,0, 1, 0, 0, 1, 1, 1), "npc"),
                             y=unit( c(0,0, 0, 1, 1, 0, 1, 1), "npc"),
                             id=rep(1:4, 2),
                             gp=gpar(col = border, lwd=1)) 
            ),
            vp = vpStack(
                plotViewport(margins = margins, name = "plotViewport"),
                dataViewport(xscale = xlim, yscale = ylim, name = "dataViewport")
            ),
            name = "loon plot"
        )
    }
}


## Layer to grob ====
getGrob <- function(widget, layerid, states) {
    UseMethod("getGrob", layerid)
}

getGrob.group <- function(widget, layerid, states = NULL, glyph, images) {
  
  gTree(
    children = do.call(gList, 
                       lapply(rev(l_layer_getChildren(widget, layerid)), 
                              function(l) {
                                
                                type <- l_layer_getType(widget, l)
                                
                                layer_states <- if (type == "group") {
                                  NULL
                                } else {
                                  target <-  l_create_handle(if (l == "model") widget else c(widget, l))
                                  get_layer_states(target, widget, layerid, type)
                                }
                                
                                if(type == "scatterplot"){
                                  glyphId <- l_glyph_ids(widget)
                                  if(l == "model" & length(glyphId) != 0){
                                    glyphType <- l_glyph_getType(widget, glyphId[length(glyphId)])
                                    layer_grob <- switch(glyphType, 
                                                         "text" = textGlyphGrob(widget, structure(l, class = type), 
                                                                                glyph, layer_states),
                                                         "polygon" = polygonGlyphGrob(widget, structure(l, class = type), 
                                                                                      glyph, layer_states), 
                                                         "pointrange" = pointrangeGlyphGrob(widget, structure(l, class = type), 
                                                                                            glyph, layer_states), 
                                                         "image" = imageGlyphGrob(widget, structure(l, class = type), 
                                                                                  glyph, layer_states, images), 
                                                         "serialaxes" = serialaxesGlyphGrob(widget, structure(l, class = type), 
                                                                                            glyph, layer_states))
                                  } else layer_grob <- getGrob(widget, structure(l, class = type), layer_states)
                                } else layer_grob <- getGrob(widget, structure(l, class = type), layer_states)

                                
                                if (!is.null(layer_grob)) {
                                  editGrob(layer_grob, name = paste(l_layer_getLabel(widget, l), l, collapse = " " ))
                                } else {
                                  NULL
                                }
                              })),
    name = l_layer_getLabel(widget, layerid)
  )
}

# __model grobs----
getGrob.scatterplot <- function(widget, layerid, states) {
  active <- states$active
  if(l_layer_isVisible(widget, layerid) && 
     length(states$x[active])!=0 && 
     length(states$y[active])!=0  ){
    scatterGrob(states)
  }else NULL
}


scatterGrob <- function(states, ...){
  active <- states$active
  selected <- states$selected
  sel_color <- as.character(.Tcl("set loon::Options(select-color)"))
  states$size <- as_r_point_size(states$size)
  states$glyph <- glyph_to_pch(states$glyph)
  # filled circles rectangles and trangles
  filledColIndex <- which( states$glyph[active] %in% 21:24)
  if(length(filledColIndex) != 0 & length(filledColIndex) != length(states$glyph[active]) ){
    gT <- gTree(
      children = gList(
        pointsGrob(
          x = states$x[active][filledColIndex], y = states$y[active][filledColIndex],
          gp = gpar(col = "black",
                    fill = states$col[active][filledColIndex], 
                    cex = states$size[active][filledColIndex]),
          pch = states$glyph[active][filledColIndex]
        ), 
        pointsGrob(
          x = states$x[active][-filledColIndex], y = states$y[active][-filledColIndex],
          gp = gpar(col = states$col[active][-filledColIndex], 
                    cex = states$size[active][-filledColIndex]),
          pch = states$glyph[active][-filledColIndex]
        )
      ), ...
    )
  }else if(length(filledColIndex) == length(states$glyph[active])){
    gT <- pointsGrob(
      x = states$x[active], y = states$y[active],
      gp = gpar(col = "black",
                fill = states$col[active], 
                cex = states$size[active]),
      pch = states$glyph[active], ...
    )
  }else{
    gT <- pointsGrob(
      x = states$x[active], y = states$y[active],
      gp = gpar(col = states$col[active], cex = states$size[active]),
      pch = states$glyph[active], ...
    )
  }
  # if any selected, non active selected will return character(0)
  if(length(states$x[selected]) != 0 && length(states$y[selected]) != 0 ){
    gTree(
      children = gList(
        gT,
        pointsGrob(
          x = states$x[selected], y = states$y[selected],
          gp = gpar(col = sel_color, 
                    fill = sel_color,
                    cex = states$size[selected]),
          pch = states$glyph[selected],...
        )
      )
    )
  }else gT
}



getGrob.histogram <- function(widget, layerid, states) {
  active <- states$active
  swapAxes <- widget['swapAxes']
  activeX <- if (swapAxes) as.numeric( states$y )[active]  else as.numeric( states$x )[active] 
  if(l_layer_isVisible(widget, layerid) & length(activeX)!=0  ){
    
    n <- length(activeX) 
    activeSelected <- states$selected[active]
    activeColor <- states$color[active]
    sel_color <- as.character(.Tcl("set loon::Options(select-color)"))
    binId <- list()
    minActivex <- min(activeX)
    maxActivex <- max(activeX)
    i <- 1
    binwidth <- states$binwidth
    binX <- c()
    while(minActivex + (i-1) * binwidth <= maxActivex){
      left <- minActivex + (i - 1) * binwidth
      right <- minActivex + i * binwidth
      binId[[i]] <- which( (activeX <right & activeX >= left ) == TRUE)
      binX <- c(binX, left, right)
      i <- i + 1
    } 
    
    binHeight <- if(states$yshows == "frequency") {sapply(binId, "length") 
    }else {sapply(binId, "length") / (n * binwidth)}
    
    
    binX <- unique(binX)
    
    gTree(children = do.call (
      gList,
      lapply(1:length(binHeight) , function(i){
        if(binHeight[i] != 0){
          if(!swapAxes){
            x <- unit(mean(c(binX[i], binX[i + 1])), "native")
            y <- unit(mean(c(0, binHeight[i])), "native")
            
            width <- unit(binwidth, "native")
            height <- unit(binHeight[i], "native")
          }else{
            y <- unit(mean(c(binX[i], binX[i + 1])), "native")
            x <- unit(mean(c(0, binHeight[i])), "native")
            
            height <- unit(binwidth, "native")
            width <- unit(binHeight[i], "native")
          }
          
          isSelected  <- activeSelected[binId[[i]]]
          
          if(states$showStackedColors){
            
            if(any(isSelected)){
              binSelected <- length(which(isSelected == TRUE))
              oldColorBinHeight <- table(activeColor[binId[[i]]][which(isSelected == FALSE)])
              colorBinHeight <- if(states$yshows == "frequency") {c(binSelected, oldColorBinHeight)
              }else{c(binSelected, oldColorBinHeight) / (n * binwidth)}
              names(colorBinHeight) <- c(sel_color, names(oldColorBinHeight))
            }else{colorBinHeight <- if(states$yshows == "frequency") { table(activeColor[binId[[i]]])
            }else{table(activeColor[binId[[i]]]) / (n * binwidth)} }
            
            cumsumColorBinHeight <- c(0, cumsum(colorBinHeight))
            do.call(gList, 
                    lapply(1:length(colorBinHeight), function(i){
                      if(!swapAxes){
                        y <- unit(mean(c(cumsumColorBinHeight[i], 
                                         cumsumColorBinHeight[i+1])), "native")
                        height <- unit(colorBinHeight[i], "native")
                      }else{
                        x <- unit(mean(c(cumsumColorBinHeight[i], 
                                         cumsumColorBinHeight[i+1])), "native")
                        width <- unit(colorBinHeight[i], "native")
                      }
                      rectGrob(
                        x = x, y = y, width = width, height = height, 
                        gp = gpar(fill = names(colorBinHeight)[i], 
                                  col = if(states$showOutlines) states$colorOutline else NA)) 
                    }))
          }else{
            rectGrobObject <- rectGrob(
              x = x, y = y, width = width, height = height, 
              gp = gpar(fill = states$colorFill, 
                        col = if(states$showOutlines) states$colorOutline else NA))
            if(any(isSelected)){
              binSelected <- if(states$yshows == "frequency") {length(which(isSelected == TRUE))
              }else{length(which(isSelected == TRUE)) / (n * binwidth)}
              if(!swapAxes){
                y <- unit(mean(c(0, binSelected)), "native")
                height <- unit(binSelected, "native") 
              }else{
                x <- unit(mean(c(0, binSelected)), "native")
                width<- unit(binSelected, "native")
              }
              
              gList(
                rectGrobObject,
                rectGrob(
                  x = x, y = y, width = width, height = height, 
                  gp = gpar(fill = sel_color, 
                            col = if(states$showOutlines) states$colorOutline else NA))
              )
            }else{rectGrobObject}
          }
        }
      }))
    )
  } else NULL 
}


getGrob.graph <- function(widget, layerid, states) {
  active <- states$active
  if(l_layer_isVisible(widget, layerid) && 
     length(states$x[active])!=0 && 
     length(states$y[active])!=0  ){
    activeNode <- states$nodes[active]
    activeX <- states$x[active]
    activeY <- states$y[active]
    isActiveEdge <- states$activeEdge
    linesTextsGrobObject <- do.call(
      gList, 
      lapply(1:length(activeNode) , function(i) {
        nodeFrom <- activeNode[i]
        nodeFrom_EdgeId <- which (states$from[isActiveEdge] == nodeFrom)
        if(length(nodeFrom_EdgeId) != 0){
          nodeTo <- states$to[isActiveEdge][nodeFrom_EdgeId]
          nodeTo_CoordId <- which (activeNode %in% nodeTo == TRUE)
          len_nodeTo <- length(nodeTo_CoordId)
          cols <- states$colorEdge[isActiveEdge][nodeFrom_EdgeId]
          x <- unit( c( rep(activeX[i], len_nodeTo), activeX[nodeTo_CoordId] ), "native")
          y <- unit( c( rep(activeY[i], len_nodeTo), activeY[nodeTo_CoordId] ), "native")
          polylineGrobObject <- polylineGrob(x, y,
                                             id=rep(1:len_nodeTo, 2),
                                             gp=gpar(col= cols, lwd=1))
        }else{polylineGrobObject <- NULL}
        if(states$showOrbit){
          activeAngle <- states$orbitAngle[active]
          orbitDistance <- states$orbitDistance
          textsGrobObject <- textGrob(label = activeNode[i], 
                                      x = unit(activeX[i], "native") + 
                                        unit(orbitDistance * cos(activeAngle[i]),"mm" ), 
                                      y = unit(activeY[i], "native") + 
                                        unit(orbitDistance * sin(activeAngle[i]),"mm" ), 
                                      gp=gpar(fontsize= 8, col= "black"))
          gList(polylineGrobObject, 
                textsGrobObject)
        }else{polylineGrobObject}
      }))
    states$col <- states$color
    scatterGrobObject <- scatterGrob(states)
    gTree(children = 
            gList(
              linesTextsGrobObject,
              scatterGrobObject)
    )
  }
}


# __primitive grobs----
getGrob.polygon <- function(widget, layerid, states) {
    if(l_layer_isVisible(widget, layerid) & length(states$x)!=0  & length(states$y)!=0  ){
        polygonGrob(
            x = states$x, y = states$y,
            gp = gpar(
                fill = states$color, col = states$linecolor, lwd = states$linewidth
            )
        )} else NULL
}



getGrob.line <- function(widget, layerid, states) {
    if(l_layer_isVisible(widget, layerid) & length(states$x)!=0  & length(states$y)!=0  ){
        linesGrob(
            x = states$x, y = states$y,
            gp = gpar(col = states$color, lwd = states$linewidth)
        )} else NULL
}

getGrob.rectangle <- function(widget, layerid, states) {
    if(l_layer_isVisible(widget, layerid) & length(states$x)!=0  & length(states$y)!=0  ){
        xcoords <- as.numeric(states$x)
        ycoords <- as.numeric(states$y)
        
        x <- unit(mean(xcoords), "native")
        y <- unit(mean(ycoords), "native")
        
        width <- unit(diff(range(xcoords)), "native")
        height <- unit(diff(range(ycoords)), "native")
        
        rectGrob(
            x = x, y = y, width = width, height = height, 
            gp = gpar(fill = states$color, col = states$linecolor, lwd = states$linewidth)
        )} else NULL
    
}

getGrob.oval <- function(widget, layerid, states) {
  if(l_layer_isVisible(widget, layerid) & length(states$x)!=0  & length(states$y)!=0  ){
    xcoords <- as.numeric(states$x)
    ycoords <- as.numeric(states$y)
    
    angle <- seq(0, 2*pi, length=101)
    
    xCenter <- mean(xcoords)
    yCenter <- mean(ycoords)
    # x radius
    xRadius <- diff(range(xcoords))/2
    # y radius
    yRadius <- diff(range(ycoords))/2
    
    x <- unit( mean(xcoords) + xRadius * cos(angle), "native")
    y <- unit( mean(ycoords) + yRadius * sin(angle), "native")
    
    polygonGrob(x, y, gp = gpar(fill = states$color, col = states$linecolor,
                                     lwd = states$linewidth) )
  } else NULL
}

getGrob.text <- function(widget, layerid, states) {
    if(l_layer_isVisible(widget, layerid) & length(states$x)!=0  & length(states$y)!=0  ){
        textGrob(
            label = states$text, x = states$x, y = states$y,
            rot = states$angle,
            just = states$anchor, 
            gp=gpar(fontsize= states$size, col=states$color)
        )} else NULL
}

getGrob.points <- function(widget, layerid, states) {
    active <- states$active
    
    if(l_layer_isVisible(widget, layerid) & length(states$x[active])!=0  & length(states$y[active]) !=0 ){
        states$size <- as_r_point_size(states$size)
        pointsGrob(
            x = states$x[active], y = states$y[active],
            gp = gpar(col = states$color[active], cex = states$size[active]),
            pch = 16
        )  
    } else NULL
}

getGrob.texts <- function(widget, layerid, states) {
    active <- states$active
    if(l_layer_isVisible(widget, layerid)  && 
       length(states$x[active])!=0  && 
       length(states$y[active]) !=0 ){
        gTree(
            children = do.call(
                gList, 
                lapply(seq_along(states$x), function(i) {
                    if(active[i]){
                        textGrob(
                            label = states$text[i], x = states$x[i], 
                            y = states$y[i],
                            rot = states$angle[i],
                            just = states$anchor[i],
                            gp=gpar(fontsize= states$size[i], col=states$color[i])
                        ) 
                    }
                }))
        )} else NULL
}

getGrob.polygons <- function(widget, layerid, states) {
    active <- states$active
    if(l_layer_isVisible(widget, layerid) & length(states$x[active])!=0  & length(states$y[active])!=0  ){
        gTree(
            children = do.call(
                gList, 
                lapply(seq_along(states$x), function(i) {
                    if(active[i]){
                        polygonGrob(
                            x = states$x[[i]], y = states$y[[i]],
                            gp = gpar(
                                fill = states$color[i], 
                                col = states$linecolor[i], 
                                lwd = states$linewidth[i]
                            )
                        ) 
                    }else NULL
                }))
        )} else NULL
}

getGrob.rectangles <- function(widget, layerid, states) {
    active <- states$active
    if(l_layer_isVisible(widget, layerid) & length(states$x[active])!=0  & length(states$y[active])!=0 ){
        
        gTree(
            children = do.call(
                gList, 
                lapply(seq_along(states$x), function(i) {
                    if(active[i]){
                        xcoords <- as.numeric(states$x[[i]])
                        ycoords <- as.numeric(states$y[[i]])
                        
                        x <- unit(mean(xcoords), "native")
                        y <- unit(mean(ycoords), "native")
                        
                        width <- unit(diff(range(xcoords)), "native")
                        height <- unit(diff(range(ycoords)), "native")
                        
                        rectGrob(
                            x = x, y = y, width = width, height = height, 
                            gp = gpar(fill = states$color[i], col = states$linecolor[i], 
                                      lwd = states$linewidth[i])
                        ) 
                    }else NULL
                }))
        )
      } else NULL
}

getGrob.lines <- function(widget, layerid, states) {
    active <- states$active
    if(l_layer_isVisible(widget, layerid) & length(states$x[active])!=0  & length(states$y[active])!=0 ){
        gTree(
            children = do.call(
                gList, 
                lapply(seq_along(states$x), function(i) {
                    if(active[i]){
                        linesGrob(
                            x = states$x[[i]], y = states$y[[i]],
                            gp = gpar(col = states$color[i], 
                                      lwd = states$linewidth[i])
                        )
                    }else NULL
                }))
        )} else NULL
}

# Get Attributes ====
glyph_to_pch <- function(glyph) {
  
  vapply(glyph, function(x) {
    switch(
      x,
      circle = 16,
      ocircle = 1,
      ccircle = 21,
      square = 15,
      osquare = 0,
      csquare = 22,
      triangle = 17,
      otriangle = 2,
      ctriangle = 24,
      diamond = 18,
      odiamond = 5,
      cdiamond = 23,
      {
        # warning("glype type ", glyph, " will be mapped to circle")
        16
      }
    )
  }, numeric(1))
  
}


# see optionDatabase.tcl
as_r_point_size <- function(s) {
  
  if (is.null(s)) {
    NULL
  } else {
    # trial and error to choose 7
    size <- sqrt(s / 7)
    size[size < 0.1] <- 0.1   
    
    size
  }
}

# Model layers have selected state
get_model_color <- function(widget) {
  
  col <- as_hex6color(widget['color'])
  sel <- widget['selected']
  
  sel_color <- as.character(.Tcl("set loon::Options(select-color)"))
  
  if (grepl("^#", sel_color) && nchar(sel_color) == 13) {
    sel_color <- loon:::hex12tohex6(sel_color)
  }
  
  col[sel] <- sel_color
  col
}

as_hex6color <- function(color) {
  if(length(color)){
    col <- suppressWarnings(loon:::hex12tohex6(color))
    col[color == ""] <- NA
  }else {col <- NA}
  col
}




xy_coords <- function(target, widget, layerid, type, native_unit = TRUE) {
  
  xy <- if (type %in% c("scatterplot","graph") ) {
    list(
      x = if (length(widget['xTemp']) == 0) widget['x'] else widget['xTemp'],
      y = if (length(widget['yTemp']) == 0) widget['y'] else widget['yTemp']
    )
  } else if (type %in% c('polygon', 'line', 'rectangle', 'text', 'oval',
                         'points', 'texts', 'polygons', 'rectangles', 'lines')) {
    list(
      x = l_cget(target, "x"),
      y = l_cget(target, "y")
    )
  } else if(type == "histogram"){
    list(
      x = l_cget(target, "x"), 
      y = NA
    )
  } else {
    stop("unknown layer type ", type)
  }
  
  if (widget['swapAxes']) {
    names(xy) <- c("y", "x")
  } 
  
  if (native_unit & length(xy$x) != 0 & length(xy$y) != 0) {
    xy <- if (type %in% c('polygons', 'rectangles', 'lines')) {
      list(x = lapply(xy$x, function(xi)unit(xi, "native")),
           y = lapply(xy$y, function(yi)unit(yi, "native")))
    } else {
      list(x = unit(xy$x, "native"), y = unit(xy$y, "native"))   
    }
    
  }
  xy
}


cartesian_model_widget_states <- c(
  "x", "y", "swapAxes",
  "tag", "itemLabel",
  "useLoonInspector", "selectionLogic",  "linkingGroup",
  "zoomX", "zoomY", "panY", "panX", "deltaX", "deltaY",
  "linkingKey", "linkingKey",  "showItemLabels",  "selectBy",
  "background", "foreground", "guidesBackground", "guidelines",
  "minimumMargins", "labelMargins", "scalesMargins", "xTemp", "yTemp" 
)


#' @export
get_layer_states <- function(target, widget, layerid, type, omit = NULL) {
  
  states_info <- l_info_states(target)
  state_names <- setdiff(names(states_info), c(omit, cartesian_model_widget_states))
  
  states <- setNames(lapply(state_names, function(state) l_cget(target, state)), state_names)
  
  # Add Coordinates
  if (type != "group") {
    states <- c(xy_coords(target, widget, layerid, type), states)                
  }
  
  # Deal with color
  is_color <- vapply(states_info[state_names], function(s) s$type %in% c("color", "colorOrTransparent"), 
                     logical(1))
  if (any(is_color)) {
    for (state_name in state_names[is_color]) {
      states[[state_name]] <- as_hex6color(states[[state_name]])            
    }
    
  }
  
  states
}
