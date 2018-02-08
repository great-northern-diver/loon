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
#' grid.ls(g, viewports = TRUE, fullNames = TRUE)
#' 
#' ## with primitive layers from l_layers demo
#' 
#' p <- with(olive,
#'           l_plot(x=linoleic, y=oleic,
#'                  color=Region, title="Olive Data"))
#'                  
#' ## Layer a Group
#' l.g <- l_layer_group(p, label="Drawings", parent="root", index="end")
#' 
#' ## Layer Points
#' l.pts <- l_layer_points(p,
#'                         x=seq(from=200,to=1600, length.out=20),
#'                         y=seq(from=6000,to=8000, length.out=20),
#'                         color="steelblue", size=20:39)
#' 
#' ## Polygon
#' i <- with(olive, chull(linoleic, oleic))
#' x.hull <- olive$linoleic[i]
#' y.hull <- olive$oleic[i]
#' l_layer_polygon(p, x.hull, y.hull, color="thistle",
#'                 linecolor="black", linewidth=4, parent=l.g)
#' 
#' ## Rectangle
#' l_layer_rectangle(p, x=c(1100, 1300), y=c(7600, 8300), linewidth=2)
#' 
#' ## Oval
#' l_layer_oval(p, x=c(1500, 1750), y=c(7900, 8100),
#'              color="", linecolor="orange", linewidth=4)
#' 
#' ## Line
#' x <- with(olive, linoleic[Region=="North"])
#' y <- with(olive, oleic[Region=="North"])
#' 
#' fit <- lm(y~x)
#' xr <- seq(from=min(x), to=max(x), length.out=20)
#' yp <- predict(fit, data.frame(x=xr), interval="prediction")
#' 
#' l.pi <- l_layer_polygon(p, x=c(xr, rev(xr)),
#'                         y=c(yp[,2],rev(yp[,3])),
#'                         color="lightgreen",
#'                         linecolor= "darkgreen", linewidth=2,
#'                         label="predition interval west liguria")
#' 
#' l.fit <- l_layer_line(p, x=xr, y=yp[,1],
#'                       color="darkgreen", linewidth=8,
#'                       label="fit west liguria")
#' 
#' 
#' ## Text (size does not work and color is gray)
#' bbox <- l_layer_bbox(p, "root")
#' l_layer_texts(p, x=seq(from=bbox[1], to=bbox[3], length.out=length(LETTERS)),
#'               y=rev(seq(from=bbox[2], to=bbox[4], length.out=length(LETTERS))),
#'               text=LETTERS, size=seq_along(LETTERS),
#'               angle=seq_along(LETTERS)/length(LETTERS)*360)
#' 
#' # text
#' l_layer_text(p, x = 750, y = 6500, text = "Hello World")
#' 
#' # polygons
#' l_layer_polygons(p,
#'   x = list(
#'     c(500, 500, 600, 750, 650, 550),
#'     c(500, 500, 750, 750, 625),
#'     c(1250, 1250, 1400, 1400),
#'     c(1250, 1250, 1400)
#'   ),
#'   y = list(
#'     c(6500, 6800, 7200, 6800, 6500, 7000),
#'     c(8200, 8500, 8500, 8200, 8400),
#'     c(8200, 8500, 8500, 8300),
#'     c(6500, 7200, 6850)
#'   ),
#'   color = c('blue', 'orange', 'yellow', 'red')
#' )
#' 
#' # rectangles
#' l_layer_rectangles(p,
#'   x = lapply(1:4, function(x) 500 + x*40 + c(0, 20)),
#'   y = lapply(1:4, function(y) 6500 + y*100 + c(0, 60))
#' )
#' 
#' # lines
#' l_layer_lines(p,
#'   x = replicate(3, c(1250, 1300, 1500), simplify = FALSE),
#'   y = list(
#'     c(7500, 8200, 8000),
#'     c(7200, 8000, 7200),
#'     c(6800, 6700, 7000)
#'   ),
#'   color = "magenta"
#' )
#' 
#' l_scaleto_world(p)
#' 
#' p['showScales'] <- TRUE
#' 
#' g <- loonGrob(p)
#' library(grid)
#' grid.newpage()
#' grid.draw(g)
#' 
#' grid.ls(g, viewports=TRUE, fullNames=TRUE)
#' 
#' 
loonGrob <- function(widget) {
    
    l_isLoonWidget(widget) || stop("widget does not seem to exist")    

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



## Layer to grob====
getGrob <- function(widget, layerid) {
    UseMethod("getGrob", layerid)
}

getGrob.group <- function(widget, layerid) {
    gTree(
       children = do.call(gList, lapply(rev(l_layer_getChildren(widget, layerid)), function(l) {
           type <- l_layer_getType(widget, l)
           
           layer_grob <- getGrob(widget, structure(l, class = type))
           
           if (!is.null(layer_grob)) {
               editGrob(layer_grob, name = l_layer_getLabel(widget, l))
           } else {
               NULL
           }
       })),
       name = l_layer_getLabel(widget, layerid)
    )
}

# __model grobs----
getGrob.scatterplot <- function(widget, layerid) {
 
    # implement glyphs
    
    size <- sqrt(widget['size'] / 4)
    size[size < 0.1] <- 0.1
    
    xy <- get_xy_mapping(widget, layerid)
    col <- get_color_model(widget)
    
    pointsGrob(
        x = xy$x, y = xy$y,
        gp = gpar(col = col, cex = size),
        pch = get_pch(widget, layerid)
    )
}




getGrob.histogram <- function(widget, layerid) {
    NULL
}

getGrob.graph <- function(widget, layerid) {
    NULL
}

# __primitive grobs----
getGrob.polygon <- function(widget, layerid) {
    
    xy <- get_xy_mapping(widget, layerid)
    s <- layer_states(widget, layerid)
    
    polygonGrob(
        x = xy$x, y = xy$y,
        gp = gpar(
            fill = s$color, col = s$linecolor, lwd = s$linewidth
        )
    )
}



getGrob.line <- function(widget, layerid) {
    
    xy <- get_xy_mapping(widget, layerid)
    s <- layer_states(widget, layerid)
    
    linesGrob(
        x = xy$x, y = xy$y,
        gp = gpar(col = s$color, lwd = s$linewidth)
    )
}

getGrob.rectangle <- function(widget, layerid) {
    
    xy <- get_xy_mapping(widget, layerid, native_unit = FALSE)
    
    x <- unit(mean(xy$x), "native")
    y <- unit(mean(xy$y), "native")
    
    width <- unit(diff(range(xy$x)), "native")
    height <- unit(diff(range(xy$y)), "native")
    
    s <- layer_states(widget, layerid)
    
    rectGrob(
        x = x, y = y, width = width, height = height, 
        gp = gpar(fill = s$color, col = s$linecolor, lwd = s$linewidth)
    )
}

getGrob.oval <- function(widget, layerid) {
    
    #xy <- get_xy_mapping(widget, layerid)
    #target <- c(widget, layerid)
    
    #s <- layer_states(widget, layerid)

    # warning("no oval grob currently implemented")
    NULL
}

getGrob.text <- function(widget, layerid) {
    
    xy <- get_xy_mapping(widget, layerid)
    s <- layer_states(widget, layerid)
    
    # names(s) 
    # "text"    "color"   "angle"   "size"    "anchor"  "justify"   

   #  browser()
    textGrob(
        label = s$text, x = xy$x, y = xy$y,
        gp = gpar(col = s$color),
        rot = s$angle,
        just = s$anchor
    )
}

getGrob.points <- function(widget, layerid) {
   
    xy <- get_xy_mapping(widget, layerid) 
    s <- layer_states(widget, layerid)
    
    s$size <- sqrt(s$size / 4)
    s$size[s$size < 0.1] <- 0.1
    
    s$x <- xy$x
    s$y <- xy$y
    
    s <- subset(s, s$active)
    
    pointsGrob(
        x = s$x, y = s$y,
        gp = gpar(col = s$color, cex = s$size),
        pch = 16
    )
}

getGrob.texts <- function(widget, layerid) {
    
    xy <- get_xy_mapping(widget, layerid) 
    s <- layer_states(widget, layerid)
    
    s$x <- xy$x
    s$y <- xy$y
    
    gTree(
        children = do.call(
            gList, 
            lapply(split(s, 1:nrow(s)), function(si) {
                textGrob(
                    label = si$text, x = si$x, y = si$y,
                    gp = gpar(col = si$color),
                    rot = si$angle,
                    just = si$anchor
                )
            }))
    )
}

getGrob.polygons <- function(widget, layerid) {
    
    xy <- get_xy_mapping(widget, layerid) 
    s <- layer_states(widget, layerid)
    
    states <- split(s, 1:nrow(s))
    
    gTree(
        children = do.call(
            gList, 
            lapply(seq_along(states), function(i) {
                polygonGrob(
                    x = xy$x[[i]], y = xy$y[[i]],
                    gp = gpar(
                        fill = states[[i]]$color, col = states[[i]]$linecolor, lwd = states[[i]]$linewidth
                    )
                )
            }))
    )
}

getGrob.rectangles <- function(widget, layerid) {
    
    xy <- get_xy_mapping(widget, layerid) 
    s <- layer_states(widget, layerid)
    
    s <- layer_states(widget, layerid)
    
    states <- split(s, 1:nrow(s))
    
    gTree(
        children = do.call(
            gList, 
            lapply(seq_along(states), function(i) {
                
                x <- unit(mean(xy$x[[i]]), "native")
                y <- unit(mean(xy$y[[i]]), "native")
                width <- unit(diff(range(xy$x[[i]])), "native")
                height <- unit(diff(range(xy$y[[i]])), "native")
                
                rectGrob(
                    x = x, y = y, width = width, height = height, 
                    gp = gpar(fill = states[[i]]$color, col = states[[i]]$linecolor, lwd = states[[i]]$linewidth)
                )
            }))
    )
    

}

getGrob.lines <- function(widget, layerid) {
    xy <- get_xy_mapping(widget, layerid) 
    s <- layer_states(widget, layerid)
    
    
    states <- split(s, 1:nrow(s))
    
    gTree(
        children = do.call(
            gList, 
            lapply(seq_along(states), function(i) {
                linesGrob(
                    x = xy$x[[i]], y = xy$y[[i]],
                    gp = gpar(col = states[[i]]$color, lwd = states[[i]]$linewidth)
                )
            }))
    )
}


# Get Attributes ====

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

get_color_model <- function(widget, layerid) {
    
    col <- as_color(widget['color'])
    sel <- widget['selected']
    
    sel_color <- as.character(.Tcl("set loon::Options(select-color)"))
    
    if (grepl("^#", sel_color) && nchar(sel_color) == 13) {
        sel_color <- hex12tohex6(sel_color)
    }
    
    col[sel] <- sel_color
    col
}

as_color <- function(color) {
    col <- suppressWarnings(hex12tohex6(color))
    col[color == ""] <- NA
    col
}




get_xy_mapping <- function(widget, layerid, native_unit = TRUE) {
    
    type <- class(layerid)
    if (is.null(type) || type == "character") type <- l_layer_getType(widget, layerid)
    
    xy <- if (type == "scatterplot") {
        list(
            x = if (length(widget['xTemp']) == 0) widget['x'] else widget['xTemp'],
            y = if (length(widget['yTemp']) == 0) widget['y'] else widget['yTemp']
        )
    } else if (type %in% c('polygon', 'line', 'rectangle', 'oval', 'text',
                           'points', 'texts', 'polygons', 'rectangles', 'lines')) {
        list(
            x = l_cget(c(widget, layerid), "x"),
            y = l_cget(c(widget, layerid), "y")
        )
    } else {
        stop("unknown layer type ", type)
    }
    
    if (widget['swapAxes']) {
        names(xy) <- c("y", "x")
    } 
    
    if (native_unit) {
        xy <- if (type %in% c('polygons', 'rectangles', 'lines')) {
            list(x = lapply(xy$x, function(xi)unit(xi, "native")),
                 y = lapply(xy$y, function(yi)unit(yi, "native")))
        } else {
            list(x = unit(xy$x, "native"), y = unit(xy$y, "native"))   
        }

    }
    
    xy
}

#' @export
layer_states <- function(widget, layerid, omit = c("x", "y", "tag", "itemLabel", "dash")) {
    
    target <- l_create_handle(c(widget, layerid))
    states_info <- l_info_states(target)
    state_names <- setdiff(names(states_info), omit)
    
    
    states <- sapply(state_names, function(state) l_cget(target, state), USE.NAMES = TRUE, simplify = FALSE)
    
    n <- vapply(states, length, numeric(1))
    
    if (any(n != n[1])) stop("dimension missmatch: ", layerid )
    
    is_color <- vapply(states_info[state_names], function(s) s$type %in% c("color", "colorOrTransparent"), logical(1))
    
    if (any(is_color)) {
        for (state_name in state_names[is_color]) {
            states[[state_name]] <- as_color(states[[state_name]])            
        }
    }
    as.data.frame(states, stringsAsFactors = FALSE)
}

