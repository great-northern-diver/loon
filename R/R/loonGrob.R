
#' @export
grid.loon <- function (target, name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {

    lg <- loonGrob(target, name = name, gp = gp, vp = vp)
    
    if (draw) 
        grid.draw(lg)
    
    invisible(lg)
}

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
#' lgrob <- loonGrob(widget)
#' grid.ls(lgrob, viewports=TRUE, fullNames=TRUE)
#' grid.newpage(); grid.draw(lgrob)
#' 
#' widget['title'] <- "Iris Data"
#' grid.edit("Scatterplot model", gp=gpar(alpha = 0.6))
#' 
#' 
#' p <- demo("l_layers", ask = FALSE)$value
#' 
#' 
#' 
#' \dontrun{
#' p <- demo("l_glyph_sizes", ask = FALSE)$value
#' 
#' m <- 6  
#' lgrob <- loonGrob(p)
#' grid.newpage()
#' grid.draw(lgrob)
#' }
#' 

loonGrob <- function(target, ...) {
    UseMethod("loonGrob")
}

#' @export
loonGrob.default <- function(target, ...) {
    stop("loonGrob.default no valid inheritance")
}
    

#' @export
loonGrob.l_plot <- function(target,  name = NULL, gp = NULL, vp = NULL) {
    rl <- l_create_handle(c(target, "root"))
    cartesian2dGrob(target, loonGrob(rl), name = name, gp = gp, vp = vp)
}

#' @export
loonGrob.l_hist <- function(target, name = NULL, gp = NULL, vp = NULL) {
    rl <- l_create_handle(c(target, "root"))
    cartesian2dGrob(target, loonGrob(rl), name = name, gp = gp, vp = vp)
}

#' @export
loonGrob.l_graph <- function(target, name = NULL, gp = NULL, vp = NULL) {
    rl <- l_create_handle(c(target, "root"))
    cartesian2dGrob(target, loonGrob(rl), name = name, gp = gp, vp = vp)
}

cartesian2dGrob <- function(widget, interiorPlotGrob = NULL, name = NULL, gp = NULL, vp = NULL) {
    
    l_isLoonWidget(widget) || stop("widget does not seem to exist") 

    if (!is.null(interiorPlotGrob) && !is.grob(interiorPlotGrob))
        stop("interiorPlotGrob must be either NULL or a grob")
    
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
    
    
    margins <- if (showScales) c(4.1, 5.1, 2.1, 2.1) else rep(2.1, 4)
    if (title != "" && showLabels) margins[3] <- 3.1
    
    border <- as_hex6color(widget['foreground'])
    
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
                    interiorPlotGrob,
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
        ),
        name = name, vp = vp, gp = gp
    )
}



loonGrob.l_group <- function(target, name = NULL, gp = NULL, vp = NULL) {

    l_children_layers <- lapply(rev(l_layer_getChildren(target)), function(layerid) {
        l_create_handle(c(target, layerid))
    })
    
    l_visible_children_layer <- Filter(function(x) {
        l_layer_isVisible(target, x)
    }, l_children_layers)
    
    l_children_grobs <- lapply(l_visible_children_layer, loonGrob)
    
    gTree(
        children = do.call(gList, l_children_grobs),
        name = name, gp = gp, vp = vp
    )
}



# __primitive grobs----
loonGrob.l_polygon <- function(target, name = NULL, gp = NULL, vp = NULL) {
    
    states <- get_layer_states(target)
    
    # TODO if (any(states$active))
    if(length(states$x)!=0  & length(states$y)!=0){
        polygonGrob(
            x = states$x, y = states$y,
            gp = gpar(
                fill = states$color, col = states$linecolor, lwd = states$linewidth
            )
        )
    } else {
        grob(name = name, gp = gp, vp = vp)
    }
}


loonGrob.l_line <- function(target, name = NULL, gp = NULL, vp = NULL) {
    
    states <- get_layer_states(target)
    
    if(length(states$x)!=0  & length(states$y)!=0) {
        linesGrob(
            x = states$x, y = states$y,
            gp = gpar(col = states$color, lwd = states$linewidth)
        )
    } else {
        grob(name = name, gp = gp, vp = vp)
    }
}

loonGrob.l_rectangle <- function(target, name = NULL, gp = NULL, vp = NULL) {
    
    states <- get_layer_states(target)
    
    if (length(states$x)!=0  & length(states$y)!=0) {
        xcoords <- as.numeric(states$x)
        ycoords <- as.numeric(states$y)
        
        x <- unit(mean(xcoords), "native")
        y <- unit(mean(ycoords), "native")
        
        width <- unit(diff(range(xcoords)), "native")
        height <- unit(diff(range(ycoords)), "native")
        
        # TODO wrap in gTree with name, vp, gp
        rectGrob(
            x = x, y = y, width = width, height = height, 
            gp = gpar(fill = states$color, col = states$linecolor, lwd = states$linewidth)
        )
    } else {
        grob(name = name, gp = gp, vp = vp)
    }
    
}

loonGrob.l_oval <- function(target, name = NULL, gp = NULL, vp = NULL) {
    
    states <- get_layer_states(target)
    
    if (length(states$x)!=0  & length(states$y)!=0 ){
        xcoords <- as.numeric(states$x)
        ycoords <- as.numeric(states$y)
        
        angle <- seq(0, 2*pi, length=101)
        
        xCenter <- mean(xcoords)
        yCenter <- mean(ycoords)
        
        xRadius <- diff(range(xcoords))/2
        yRadius <- diff(range(ycoords))/2
        
        x <- unit( mean(xcoords) + xRadius * cos(angle), "native")
        y <- unit( mean(ycoords) + yRadius * sin(angle), "native")
        
        polygonGrob(x, y, gp = gpar(fill = states$color, col = states$linecolor,
                                    lwd = states$linewidth) )
    } else {
        grob(name = name, gp = gp, vp = vp)
    }
}

loonGrob.l_text <- function(target, name = NULL, gp = NULL, vp = NULL) {
    
    states <- get_layer_states(target)
    
    if(length(states$x)!=0  & length(states$y)!=0) {
        textGrob(
            label = states$text, x = states$x, y = states$y,
            rot = states$angle,
            just = states$anchor, 
            gp=gpar(fontsize= states$size, col=states$color)
        )
    } else {
        grob(name = name, gp = gp, vp = vp)
    }     
}

loonGrob.l_points <- function(target, name = NULL, gp = NULL, vp = NULL) {
    
    states <- get_layer_states(target)
    
    active <- states$active
    
    if(length(states$x[active])!=0  & length(states$y[active]) !=0 ){
        states$size <- as_r_point_size(states$size)
        pointsGrob(
            x = states$x[active], y = states$y[active],
            gp = gpar(col = states$color[active], cex = states$size[active]),
            pch = 16
        )  
    } else {
        grob(name = name, gp = gp, vp = vp)
    }    
}

loonGrob.l_texts <- function(target, name = NULL, gp = NULL, vp = NULL) {
    
    states <- get_layer_states(target)
    
    
    active <- states$active
    if(length(states$x[active])!=0  && 
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
                })),
            name = name, gp = gp, vp = vp
        )   
    } else {
        grob(name = name, gp = gp, vp = vp)
    }    
}

loonGrob.l_polygons <- function(target, name = NULL, gp = NULL, vp = NULL) {
    
    states <- get_layer_states(target)
    
    active <- states$active
    
    if(length(states$x[active])!=0  & length(states$y[active])!=0  ){
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
        )  
    } else {
        grob(name = name, gp = gp, vp = vp)
    }    
}

loonGrob.l_rectangles <- function(target, name = NULL, gp = NULL, vp = NULL) {
    
    states <- get_layer_states(target)
    
    
    active <- states$active
    if(length(states$x[active])!=0  & length(states$y[active])!=0 ){
        
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
    } else {
        grob(name = name, gp = gp, vp = vp)
    }    
}

loonGrob.l_lines <- function(target, name = NULL, gp = NULL, vp = NULL) {
    
    states <- get_layer_states(target)
    
    
    active <- states$active
    if(length(states$x[active])!=0  & length(states$y[active])!=0 ){
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
        )
    } else {
        grob(name = name, gp = gp, vp = vp)
    }    
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
      NA_integer_
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


get_model_display_order <- function(widget) {
    # thesis p 130
    # need to peel of the item number and add 1
    #.Tcl('.l3.plot.canvas find withtag "model&&point"')
    #.Tcl('.l3.plot.canvas gettags 35')
    seq_len(l_cget(widget, "n"))
}
