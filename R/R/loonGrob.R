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
#' l_setColorList_baseR()
#' 
#' widget <- with(iris, l_plot(Sepal.Length, Sepal.Width))
#' 
#' g <- loonGrob(widget)
#' grid.ls(g, viewports=TRUE, fullNames=TRUE)
#' grid.newpage()
#' grid.draw(g)
#' 
#' widget['title'] <- "Iris Data"
#' 
#' grid.edit("Scatterplot", gp=gpar(alpha = 0.6))
#' 
#' widget['showScales'] <- TRUE
#' g <- loonGrob(widget)
#' grid.newpage()
#' grid.draw(g)
#' 
#' 
loonGrob <- function(widget) {
    
    # check if valid widget path

    xlim <- c(widget['panX'], widget['panX'] + widget['deltaX']/widget['zoomX'])
    ylim <- c(widget['panY'], widget['panY'] + widget['deltaY']/widget['zoomY'])
    
    swapAxes <- widget['swapAxes']
    showScales <- widget['showScales']
    showLabels <- widget['showLabels']
    
    if (swapAxes) {
        tmp <- xlim
        xlim <- ylim
        ylim <- tmp
    }
    
    margins <- if (showScales) c(4.1, 5.1, 2.1, 2.1) else rep(2.1, 4)

    title <- widget['title']
    if (title != "" && showLabels) margins[3] <- 3.1
    
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
            if (showScales) {
                gList(
                    xaxisGrob(name = "x axis"),
                    yaxisGrob(name = "y axis")
                )  
            } else NULL,
            if (showLabels) {
                gList(
                    xlabelGrob,
                    ylabelGrob,
                    if (title != "") {
                        textGrob(title, name = "title", y = unit(1, "npc") + unit(.8, "lines"),
                                 gp = gpar(fontsize = 24, fontface="bold"), vjust = .5)
                    } else {
                        NULL
                    }
                )
            } else NULL,
            rectGrob(name = "bbox"),
            clipGrob(name = "clip"),
            getGrob.group(widget, "root")
        ),
        vp = vpStack(
            plotViewport(margins = margins, name = "plotViewport"),
            dataViewport(xscale = xlim, yscale = ylim, name = "dataViewport")
        ),
        name = "loon plot"
    )
}

get_xy_mapping <- function(widget) {
    
    x <- if (length(widget['xTemp']) == 0) widget['x'] else widget['xTemp']
    y <- if (length(widget['yTemp']) == 0) widget['y'] else widget['yTemp']
    
    if (widget['swapAxes']) {
        list(x = y, y = x)
    } else {
        list(x = x, y = y)
    }
}

getGrob <- function(widget, layerid) {
    UseMethod("getGrob", layerid)
}

getGrob.group <- function(widget, layerid) {
    gTree(
       children = do.call(gList, lapply(l_layer_getChildren(widget, layerid), function(l) {
           type <- l_layer_getType(widget, l)
           getGrob(widget, structure(l, class = type))
       })),
       name = l_layer_getLabel(widget, layerid)
    )
}

getGrob.scatterplot <- function(widget, layerid) {
 
    # implement glyphs
    
    size <- sqrt(widget['size'] / 4)
    size[size < 0.1] <- 0.1
    
    xy <- get_xy_mapping(widget)
    
    pointsGrob(
        x = xy$x, y = xy$y,
        gp = gpar(
            col = get_color(widget),
            cex = size
        ),
        pch = get_pch(widget, layerid),
        name = l_layer_getLabel(widget, layerid)
    )
}


get_pch <- function(widget, layerid) {
  l <- if (layerid == "model") {
      widget
  } else {
      l_create_handle(c(widget, layerid))
  }
  
  glyph <- l['glyph']
  
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
              warning("glype type ", glyph, " will be mapped to circle")
              16
          }
      )
  }, numeric(1))
  
}

get_color <- function(widget) {
    col <- hex12tohex6(widget['color'])
    sel <- widget['selected']
    
    sel_color <- as.character(.Tcl("set loon::Options(select-color)"))
    
    if (grepl("^#", sel_color) && nchar(sel_color) == 13) {
        sel_color <- hex12tohex6(sel_color)
    }
    
    col[sel] <- sel_color
    col
}


getGrob.histogram <- function(widget, layerid) {
    NULL
}

getGrob.graph <- function(widget, layerid) {
    NULL
}

getGrob.polygon <- function(widget, layerid) {
    NULL
}

getGrob.line <- function(widget, layerid) {
    NULL
}

getGrob.rectangle <- function(widget, layerid) {
    NULL
}

getGrob.oval <- function(widget, layerid) {
    NULL
}

getGrob.text <- function(widget, layerid) {
    NULL
}

getGrob.points <- function(widget, layerid) {
    NULL
}

getGrob.texts <- function(widget, layerid) {
    NULL
}

getGrob.polygons <- function(widget, layerid) {
    NULL
}

getGrob.rectangles <- function(widget, layerid) {
    NULL
}

getGrob.lines <- function(widget, layerid) {
    NULL
}

