
#' Create and optionally draw a grid grob from a loon widget handle
#'
#' @template param_target
#'
#' @template param_gridname
#'
#' @template param_gridgp
#'
#' @param draw a logical value indicating whether graphics output should be produced.
#'
#' @template param_gridvp
#'
#' @return a grid grob of the loon plot
#'
#' @import grid
#' @import grDevices
#' @import stats
#'
#' @seealso \code{\link{loonGrob}}, \code{\link{plot.loon}}
#'
#' @examples
#'
#' library(grid)
#' widget <- with(iris, l_plot(Sepal.Length, Sepal.Width))
#' grid.loon(widget)
#'
#' @export
grid.loon <- function (target, name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {

    lg <- loonGrob(target, name = name, gp = gp, vp = vp)

    if (draw)
        grid.draw(lg)

    invisible(lg)
}



#' Plot the current view of any loon plot in the current device.
#'
#' This is a wrapper for \code{grid.loon()} to simplify the plotting of
#' loon plots on any device.  Frequent users are recommended to use
#' \code{grid.loon()} for more control.
#'
#' @param x the loon plot to be plotted on the current device
#' @param y NULL, will be ignored.
#' @param ... parameters passed to \code{loonGrob}
#'
#' @return invisible()
#'
#'
#' @seealso \code{\link{loonGrob}}, \code{\link{grid.loon}}
#'
#' @examples
#'
#' loonPlot <- with(iris, l_plot(Sepal.Length, Sepal.Width))
#' loonPlot['color'] <- iris$Species
#' loonPlot['selected'] <- iris$Species == "versicolor"
#' l_scaleto_selected(loonPlot)
#' loonPlot['showGuides'] <- TRUE
#' plot(loonPlot)
#'
#' @export
plot.loon <- function (x, y = NULL, ...) {
    if (!is.null(y)) warning("argument y is ignored")
    lg <- grid.loon(x, ...)
    invisible(lg)
}


#' Create a grid grob from a loon widget handle
#'
#' Grid grobs are useful to create publication quality graphics.
#'
#' @template param_target
#'
#' @template param_gridname
#'
#' @template param_gridgp
#'
#' @template param_gridvp
#'
#' @return a grid grob
#'
#' @import grid
#'
#' @seealso \code{\link{grid.loon}}
#'
#' @export
#'
#' @examples
#'
#' widget <- with(iris, l_plot(Sepal.Length, Sepal.Width))
#'
#' lgrob <- loonGrob(widget)
#'
#' library(grid)
#' grid.ls(lgrob, viewports=TRUE, fullNames=TRUE)
#' grid.newpage(); grid.draw(lgrob)
#'
#' \dontrun{
#' p <- demo("l_layers", ask = FALSE)$value
#'
#' lgrob <- loonGrob(p)
#' grid.newpage(); grid.draw(lgrob)
#'
#' p <- demo("l_glyph_sizes", ask = FALSE)$value
#'
#' lgrob <- loonGrob(p)
#' grid.newpage()
#' grid.draw(lgrob)
#' }
#'
loonGrob <- function(target, name = NULL, gp = NULL, vp = NULL) {
    UseMethod("loonGrob")
}


#' @export
loonGrob.default <- function(target, name = NULL, gp = NULL, vp = NULL) {
    stop("loonGrob.default no valid inheritance")
}


#' @export
loonGrob.l_plot <- function(target,  name = NULL, gp = NULL, vp = NULL) {
    rl <- l_create_handle(c(target, "root"))
    layers_grob <- loonGrob(rl, name = "l_plot_layers")

    gTree(children = gList(cartesian2dGrob(target, layers_grob, name = "l_plot")),
          name = name, gp = gp, vp = vp)
}

#' @export
loonGrob.l_plot3D <- function(target,  name = NULL, gp = NULL, vp = NULL) {
    rl <- l_create_handle(c(target, "root"))
    layers_grob <- loonGrob(rl, name = "l_plot_layers")

    axes_coords <- target["axesCoords"]

    adjust_brightness <- function(z_coord, r, g, b) {
        change <- as.integer(100 + 80 * z_coord)
        if (change < 0) {
            rgb(0,0,0)
        } else if (change <= 100) {
            rgb((r/256) * change/100, (g/256) * change/100, (b/256) * change/100)
        } else {
            rgb(r,g,b, maxColorValue=255)
        }
    }

    x_color <- adjust_brightness(axes_coords[[3]][1], 255, 0, 0)
    y_color <- adjust_brightness(axes_coords[[3]][2], 0, 0, 255)
    z_color <- adjust_brightness(axes_coords[[3]][3], 0, 255, 0)

    gTree(children = gList(cartesian2dGrob(target, layers_grob, name = "l_plot3D"),
                           linesGrob(x = c(0.5, 0.5 + 0.08*axes_coords[[1]][1]),
                                     y =  c(0.5, 0.5 + 0.08*axes_coords[[2]][1]),
                                     gp = gpar(col = x_color, lwd=1),
                                     name = "3d x axis"),
                           linesGrob(x = c(0.5,0.5 + 0.08*axes_coords[[1]][2]),
                                     y =  c(0.5,0.5 + 0.08*axes_coords[[2]][2]),
                                     gp = gpar(col = y_color, lwd=1),
                                     name = "3d y axis"),
                           linesGrob(x = c(0.5,0.5 + 0.08*axes_coords[[1]][3]),
                                     y =  c(0.5,0.5 + 0.08*axes_coords[[2]][3]),
                                     gp = gpar(col = z_color, lwd=1),
                                     name = "3d z axis")),
          name = name, gp = gp, vp = vp)
}

#' @export
loonGrob.l_hist <- function(target, name = NULL, gp = NULL, vp = NULL) {
    rl <- l_create_handle(c(target, "root"))
    layers_grob <- loonGrob(rl, name = "l_hist_layers")

    gTree(children = gList(cartesian2dGrob(target, layers_grob, name = "l_hist")),
          name = name, gp = gp, vp = vp)
}


#' @export
loonGrob.l_graph <- function(target, name = NULL, gp = NULL, vp = NULL) {
    rl <- l_create_handle(c(target, "root"))
    interiorGrob <- loonGrob(rl, name = "l_graph_layers")
    gTree(children = gList(cartesian2dGrob(target, interiorGrob, name = "l_graph")),
          name = name, gp = gp, vp = vp)
}

cartesian2dGrob <- function(widget, interiorPlotGrob = NULL, name = NULL, gp = NULL, vp = NULL) {

    l_isLoonWidget(widget) || stop("widget does not seem to exist")

    if (!is.null(interiorPlotGrob) && !is.grob(interiorPlotGrob))
        stop("interiorPlotGrob must be either NULL or a grob")

    panX <- widget['panX']
    deltaX <- widget['deltaX']
    zoomX <- widget['zoomX']
    xlim <- c(panX, panX + deltaX/zoomX)

    panY <- widget['panY']
    deltaY <- widget['deltaY']
    zoomY <- widget['zoomY']
    ylim <- c(panY, panY + deltaY/zoomY)


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
    xlabel <- widget['xlabel']
    ylabel <- widget['ylabel']

    # Figure out margins
    minimumMargins <- widget['minimumMargins']
    margins <- c(0, 0, 0, 0)
    if (showLabels) {
        labelMargins <- widget['labelMargins']
        if(xlabel == "") labelMargins[1] <- minimumMargins[1]
        if(ylabel == "") labelMargins[2] <- minimumMargins[2]
        if(title == "") labelMargins[3] <- minimumMargins[3]
        margins <- margins + labelMargins
    }
    if (showScales) {margins <- margins + widget['scalesMargins'] }
    if(showLabels | showScales) {
        margins <- apply(cbind(margins, minimumMargins), 1, max)
    }
    # loon pixel margin to grid margin
    margins <- pixels_2_lines(margins)


    border <- as_hex6color(widget['foreground'])

    xylab_loc <- if (showScales) c(-3.5, -6.5) else c(-1, -1)

    # Fonts
    xlabelFont <- get_font_info_from_tk(l_getOption("font-xlabel"))
    ylabelFont <- get_font_info_from_tk(l_getOption("font-ylabel"))
    titleFont <- get_font_info_from_tk(l_getOption("font-title"))
    scalesFont <- get_font_info_from_tk(l_getOption("font-scales"))


    if (!swapAxes) {
        xlabelGrob <- condGrob(test = showLabels,
                               grobFun = textGrob,
                               name = "x label",
                               label = xlabel,
                               y = unit(xylab_loc[1], "lines"),
                               gp = gpar(fontfamily = xlabelFont$family,
                                         fontsize = xlabelFont$size,
                                         fontface = xlabelFont$face
                               )
        )
        ylabelGrob <- condGrob(test = showLabels,
                               grobFun = textGrob,
                               name = "y label",
                               label = ylabel,
                               x = unit(xylab_loc[2], "lines"),
                               rot = 90,
                               gp = gpar(fontfamily = ylabelFont$family,
                                         fontsize = ylabelFont$size,
                                         fontface = ylabelFont$face
                               )
        )
    } else {
        xlabelGrob <- condGrob(test = showLabels,
                               grobFun = textGrob,
                               name = "x label",
                               label = xlabel,
                               x = unit(xylab_loc[2], "lines"),
                               rot = 90,
                               gp = gpar(fontfamily = xlabelFont$family,
                                         fontsize = xlabelFont$size,
                                         fontface = xlabelFont$face
                               ))
        ylabelGrob <- condGrob(test = showLabels,
                               grobFun = textGrob,
                               name = "y label",
                               label = ylabel,
                               y = unit(xylab_loc[1], "lines"),
                               gp = gpar(fontfamily = ylabelFont$family,
                                         fontsize = ylabelFont$size,
                                         fontface = ylabelFont$face
                               )
                               )
    }

    axis <- loon.pretty(widget)
    xaxis.major <- axis$xaxis.major
    xaxis.minor <- axis$xaxis.minor
    yaxis.major <- axis$yaxis.major
    yaxis.minor <- axis$yaxis.minor


    titleGrob <- condGrob(test =  showLabels & (title != ""),
                          grobFun = textGrob,
                          name = "title",
                          label = title,
                          y = unit(1, "npc") + unit(.8, "lines"),
                          gp = gpar(fontfamily = titleFont$family,
                                    fontsize = titleFont$size,
                                    fontface = titleFont$face
                          ),
                          vjust = .5)
    gTree(
        children = gList(
            rectGrob(gp = gpar(col = NA,
                               fill = as_hex6color(widget['background'])),
                     name = "bounding box") ,
            gTree(
                children = gList(
                    gTree(children = gList(xlabelGrob,
                                           ylabelGrob,
                                           titleGrob),
                          name = "labels"
                    ),
                    gTree(children = gList(
                        # background
                        condGrob(test = showGuides,
                                 grobFun = rectGrob,
                                 name = "guides background",
                                 gp = gpar(col = NA,
                                           fill = as_hex6color(widget['guidesBackground'])
                                 )),
                        # x major lines
                        do.call(gList,
                                lapply(xaxis.major,
                                       function(xaxis) {
                                           condGrob(test =  showGuides,
                                                    grobFun = linesGrob,
                                                    name = paste0("guidelines: xaxis (major), x = ", xaxis),
                                                    x = unit(rep(xaxis, 2), "native"),
                                                    y =  unit(c(0, 1), "npc"),
                                                    gp = gpar(col = as_hex6color(widget['guidelines']), lwd = 2))
                                       })),
                        # x minor lines
                        do.call(gList,
                                lapply(xaxis.minor,
                                       function(xaxis) {
                                           condGrob(test =  showGuides,
                                                    grobFun = linesGrob,
                                                    name = paste0("guidelines: xaxis (minor), x = ", xaxis),
                                                    x = unit(rep(xaxis,2 ), "native"),
                                                    y =  unit(c(0, 1), "npc"),
                                                    gp = gpar(col = as_hex6color(widget['guidelines']), lwd = 1))

                                       })),
                        # y major lines
                        do.call(gList,
                                lapply(yaxis.major,
                                       function(yaxis) {
                                           condGrob(test =  showGuides,
                                                    grobFun = linesGrob,
                                                    name = paste0("guidelines: yaxis (major), y = ", yaxis),
                                                    x = unit(c(0, 1), "npc") ,
                                                    y =  unit(rep(yaxis,2), "native"),
                                                    gp = gpar(col = as_hex6color(widget['guidelines']), lwd = 2))

                                       })),
                        # y minor lines
                        do.call(
                            gList,
                            lapply(yaxis.minor,
                                   function(yaxis) {
                                       condGrob(test =  showGuides,
                                                grobFun = linesGrob,
                                                name = paste0("guidelines: yaxis (minor), y = ", yaxis),
                                                x = unit(c(0, 1), "npc") ,
                                                y =  unit(rep(yaxis,2 ), "native"),
                                                gp = gpar(col = as_hex6color(widget['guidelines']), lwd = 1))

                                   }
                            )
                        )
                    ),
                    name = "guides"),

                    # Axes
                    gTree(children = gList(condGrob(test =  showScales,
                                                    grobFun = xaxisGrob,
                                                    name = "x axis",
                                                    at = xaxis.major,
                                                    gp = gpar(fontfamily = scalesFont$family,
                                                              fontsize = scalesFont$size,
                                                              fontface = scalesFont$face
                                                    )),
                                           condGrob(test =  showScales,
                                                    grobFun = yaxisGrob,
                                                    name = "y axis",
                                                    at = yaxis.major,
                                                    gp = gpar(fontfamily = scalesFont$family,
                                                              fontsize = scalesFont$size,
                                                              fontface = scalesFont$face
                                                    ))),
                          name = "axes"),


                    # Clipping
                    clipGrob(name = "clipping region"),
                    # Interior
                    interiorPlotGrob,
                    # draw boundary
                    condGrob(test =  sum(margins) > 0,
                             grobFun = rectGrob,
                             name = "boundary rectangle",
                             gp=gpar(col = border, fill = NA, lwd=1))
                ),
                vp = vpStack(
                    plotViewport(margins = margins, name = "plotViewport"),
                    dataViewport(xscale = xlim, yscale = ylim,
                                 name = if (swapAxes)
                                     "dataViewport: swapAxes" else
                                         "dataViewport")
                ),
                name = "loon plot"
            )
        ),
        name = name, vp = vp, gp = gp
    )
}



#' @export
loonGrob.l_layer_group <- function(target, name = NULL, gp = NULL, vp = NULL) {

    widget <- attr(target, "widget")

    l_children_layers <- lapply(rev(l_layer_getChildren(target)),
                                function(layerid) {
                                    l_create_handle(c(widget, layerid))
                                })

    l_visible_children_layer <- Filter(
        function(layerid) {l_layer_isVisible(widget, layerid)},
        l_children_layers)

    l_children_grobs <- lapply(l_visible_children_layer, loonGrob)

    gTree(
        children = do.call(gList, l_children_grobs),
        name = if(is.null(name)) {
            label <- l_layer_getLabel(widget, target)
            paste0("l_layer_group: ", label, " ", names(label))
        } else name,
        gp = gp,
        vp = vp
    )
}



# __primitive grobs----


#' @export
loonGrob.l_layer_polygon <- function(target, name = NULL, gp = NULL, vp = NULL) {

    widget <- attr(target, "widget")
    states <- get_layer_states(target)

    if(length(states$x)!=0  & length(states$y)!=0){
        polygonGrob(
            x = states$x, y = states$y,
            gp = if(is.null(gp)) {
                gpar(fill = states$color,
                     col = states$linecolor,
                     lwd = states$linewidth
                )
            } else gp,
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_polygon: ", label, " ", names(label))
            } else name,
            vp = vp
        )
    } else {
        grob(
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_polygon: ", label, " ", names(label))
            } else name,
            gp = gp, vp = vp)
    }
}


#' @export
loonGrob.l_layer_line <- function(target, name = NULL, gp = NULL, vp = NULL) {

    widget <- attr(target, "widget")
    states <- get_layer_states(target)

    if(length(states$x)!=0  & length(states$y)!=0) {
        linesGrob(
            x = states$x, y = states$y,
            gp = if(is.null(gp)) gpar(col = states$color, lwd = states$linewidth) else gp,
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_line: ", label, " ", names(label))
            } else name,
            vp = vp
        )

    } else {

        grob(
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_line: ", label, " ", names(label))
            } else name,
            gp = gp, vp = vp
        )
    }
}


#' @export
loonGrob.l_layer_rectangle <- function(target, name = NULL, gp = NULL, vp = NULL) {

    widget <- attr(target, "widget")
    states <- get_layer_states(target)

    if (length(states$x)!=0  & length(states$y)!=0) {
        xcoords <- as.numeric(states$x)
        ycoords <- as.numeric(states$y)

        x <- unit(mean(xcoords), "native")
        y <- unit(mean(ycoords), "native")

        width <- unit(diff(range(xcoords)), "native")
        height <- unit(diff(range(ycoords)), "native")

        rectGrob(
            x = x, y = y,
            width = width, height = height,
            gp = if(is.null(gp)) gpar(fill = states$color,
                                      col = states$linecolor,
                                      lwd = states$linewidth) else gp,
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_rectangle: ", label, " ", names(label))
            } else name,
            vp = vp
        )
    } else {
        grob(
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_rectangle: ", label, " ", names(label))
            } else name,
            gp = gp, vp = vp
        )
    }

}


#' @export
loonGrob.l_layer_oval <- function(target, name = NULL, gp = NULL, vp = NULL) {

    widget <- attr(target, "widget")
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

        polygonGrob(
            x, y,
            gp = if(is.null(gp)) {
                gpar(fill = states$color,
                     col = states$linecolor,
                     lwd = states$linewidth)
            } else gp,
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_oval: ", label, " ", names(label))
            } else name,
            vp = vp
        )

    } else {
        grob(
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_oval: ", label, " ", names(label))
            } else name,
            gp = gp, vp = vp)
    }
}


#' @export
loonGrob.l_layer_text <- function(target, name = NULL, gp = NULL, vp = NULL) {

    widget <- attr(target, "widget")
    states <- get_layer_states(target)
    adjustedCoords <- getGridTextCoords(
        text = states$text,
        angle = states$angle,
        anchor = states$anchor,
        just = states$justify
    )

    if(length(states$x) > 0  & length(states$y) > 0) {

        textGrob(
            label = states$text,
            x = states$x + adjustedCoords[1],
            y = states$y + adjustedCoords[2],
            just = states$justify,
            rot = states$angle,
            gp=if(is.null(gp)) {
                gpar(fontsize= as_r_text_size(states$size),
                     col=states$color)
            } else gp,
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_text: ", label, " ", names(label))
            } else name,
            vp = vp
        )
    } else {
        grob(
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_text: ", label, " ", names(label))
            } else name,
            gp = gp, vp = vp)
    }
}



#' @export
loonGrob.l_layer_points <- function(target, name = NULL, gp = NULL, vp = NULL) {

    widget <- attr(target, "widget")
    states <- get_layer_states(target)

    active <- states$active
    x <- states$x[active]
    y <- states$y[active]

    if(length(x)!=0  && length(y) !=0 ){
        size  <- as_r_point_size(states$size[active])
        color <- states$color[active]

        pointsGrob(
            x = x, y = y,
            gp = if(is.null(gp)) {
                gpar(col = color, cex = size)
            } else gp,
            pch = 16,
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_points: ", label, " ", names(label))
            } else name,
            vp = vp
        )

    } else {
        grob(
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_points: ", label, " ", names(label))
            } else name,
            gp = gp, vp = vp)
    }
}


#' @export
loonGrob.l_layer_texts <- function(target, name = NULL, gp = NULL, vp = NULL) {

    widget <- attr(target, "widget")
    states <- get_layer_states(target)

    active <- states$active
    x <- states$x[active]
    y <- states$y[active]

    if(length(x) > 0  && length(y) > 0 ){
        text  <- states$text[active]
        size  <- as_r_text_size(states$size[active])
        angle  <- states$angle[active]
        anchor  <- states$anchor[active]
        justify  <- states$justify[active]
        color <- states$color[active]
        textGrobs <- lapply(seq_along(x),
                            function(i) {
                                adjustedCoords <- getGridTextCoords(
                                    text = text[i],
                                    angle = angle[i],
                                    anchor = anchor[i],
                                    just = justify[i]
                                )
                                textGrob(
                                    label = text[i],
                                    x = x[i] + adjustedCoords[1],
                                    y = y[i] + adjustedCoords[2],
                                    rot = angle[i],
                                    # just = anchor[[i]],
                                    just = justify[i],
                                    gp=gpar(fontsize= size[i], col=color[i])
                                )
                            }
        )

        gTree(
            children = do.call(gList, textGrobs),
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_texts: ", label, " ", names(label))
            } else name,
            gp = gp, vp = vp
        )

    } else {
        grob(
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_texts: ", label, " ", names(label))
            } else name,
            gp = gp, vp = vp)
    }
}


#' @export
loonGrob.l_layer_polygons <- function(target, name = NULL, gp = NULL, vp = NULL) {

    widget <- attr(target, "widget")
    states <- get_layer_states(target)

    active <- states$active
    x <- states$x[active]
    y <- states$y[active]

    if(length(x)!=0  && length(y) !=0 ){
        linewidth  <- states$linewidth[active]
        linecolor <- states$linecolor[active]
        fill <- states$color[active]
        polygonGrobs <- lapply(seq_along(x),
                               function(i) {
                                   polygonGrob(
                                       x = x[[i]], y = y[[i]],
                                       gp = gpar(
                                           fill = fill[i],
                                           col = linecolor[i],
                                           lwd = linewidth[i]
                                       )
                                   )
                               }
        )

        gTree(
            children = do.call(gList, polygonGrobs),
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_polygons: ", label, " ", names(label))
            } else name,
            gp = gp, vp = vp
        )
    } else {
        grob(
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_polygons: ", label, " ", names(label))
            } else name,
            gp = gp, vp = vp)
    }
}


#' @export
loonGrob.l_layer_rectangles <- function(target, name = NULL, gp = NULL, vp = NULL) {

    widget <- attr(target, "widget")
    states <- get_layer_states(target)

    active <- states$active
    x <- states$x[active]
    y <- states$y[active]

    if(length(x)!=0  && length(y) !=0 ){
        linewidth  <- states$linewidth[active]
        linecolor <- states$linecolor[active]
        fill <- states$color[active]
        rectGrobs <- lapply(seq_along(x),
                            function(i) {
                                xcoords <- as.numeric(x[[i]])
                                ycoords <- as.numeric(y[[i]])

                                xloc <- unit(mean(xcoords), "native")
                                yloc <- unit(mean(ycoords), "native")

                                width <- unit(diff(range(xcoords)), "native")
                                height <- unit(diff(range(ycoords)), "native")

                                rectGrob(
                                    x = xloc, y = yloc,
                                    width = width,
                                    height = height,
                                    gp = gpar(fill = fill[i],
                                              col = linecolor[i],
                                              lwd = linewidth[i])
                                )
                            }
        )

        gTree(
            children = do.call(gList, rectGrobs),
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_rectangles: ", label, " ", names(label))
            } else name,
            gp = gp, vp = vp
        )

    } else {
        grob(
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_rectangles: ", label, " ", names(label))
            } else name,
            gp = gp, vp = vp)
    }
}


#' @export
loonGrob.l_layer_lines <- function(target, name = NULL, gp = NULL, vp = NULL) {

    widget <- attr(target, "widget")
    states <- get_layer_states(target)

    active <- states$active
    x <- states$x[active]
    y <- states$y[active]

    if(length(x) > 0  && length(y) > 0 ){
        linewidth  <- states$linewidth[active]
        linecolor <- states$color[active]
        lineGrobs <- lapply(seq_along(x),
                            function(i) {
                                linesGrob(
                                    x = x[[i]],
                                    y = y[[i]],
                                    gp = gpar(col = linecolor[i],
                                              lwd = linewidth[i])
                                )
                            }
        )

        gTree(
            children = do.call(gList, lineGrobs),
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_lines: ", label, " ", names(label))
            } else name,
            gp = gp, vp = vp
        )

    } else {
        grob(
            name = if(is.null(name)) {
                label <- l_layer_getLabel(widget, target)
                paste0("l_layer_lines: ", label, " ", names(label))
            } else name,
            gp = gp, vp = vp)
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
as_r_point_size <- function(size) {

    if (is.numeric(size)) {
        # trial and error to choose scale for size
        size <- sqrt(size / 12)
        size[size < 0.1] <- 0.1
    }
    size
}

as_r_text_size <- function(size){
    if (is.numeric(size)) {
        # trial and error to choose scale for size
        size <- 1 + 1.2 * (1 + size)^0.88
    }
    size
}

pixels_2_lines <- function(x) {
    x / 20
}

# Model layers have selected state
get_display_color <- function(color, selected) {

  sel_color <- as.character(l_getOption("select-color"))

  if (grepl("^#", sel_color) && nchar(sel_color) == 13) {
    sel_color <- hex12tohex6(sel_color)
  }

  color[selected] <- sel_color
  color
}

get_font_info_from_tk <- function(tkFont) {

  fontInfo <- as.character(.Tcl(paste("font actual", tkFont)))
  fontInfo <- matrix(fontInfo, ncol = 2, byrow = TRUE)

  fontFamily <- fontInfo[fontInfo[,1] == "-family", 2]
  if (!fontFamily %in% c("sans", "mono", "serif", "symbol")) fontFamily <- "sans"

  fontSize <- fontInfo[fontInfo[,1] == "-size", 2]
  if (fontSize <= 0) fontSize <- 8

  fontFace <- fontInfo[fontInfo[,1] == "-weight", 2]
  if (!fontFace %in% c("plain",
                       "bold",
                       "italic",
                       "oblique",
                       "bold-italic")
  ) fontFace <- "plain"

  list(family = fontFamily, face = fontFace, size = fontSize)
}

as_hex6color <- function(color) {

  if(length(color) > 0){
    col <- vapply(color, function(x) {
        if (x == "") "" else l_hexcolor(x)
    }, character(1))
    col <- suppressWarnings(hex12tohex6(col))
    col[color == ""] <- NA
    col
  } else {
    NA
  }
}




xy_coords_layer <- function(layer, native_unit = TRUE) {

  if (!is(layer, "l_layer")) stop("layer argument needs to be an l_layer")

  widget <- l_create_handle(attr(layer, "widget"))

  type <- l_layer_getType(attr(layer, "widget"), layer)

  xy <- if (type %in% c("scatterplot", "graph") ) {


      list(
          x = if (length(widget['xTemp']) == 0) widget['x'] else widget['xTemp'],
          y = if (length(widget['yTemp']) == 0) widget['y'] else widget['yTemp']
      )
  } else if (type %in% c('polygon', 'line', 'rectangle', 'text', 'oval',
                         'points', 'texts', 'polygons', 'rectangles', 'lines')) {
    list(
      x = l_cget(layer, "x"),
      y = l_cget(layer, "y")
    )
  } else if(type == "histogram"){
    list(
      x = l_cget(layer, "x"),
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
  "minimumMargins", "labelMargins", "scalesMargins", "xTemp", "yTemp",
  "showScales",  "title", "showLabels", "showGuides",  "xlabel", "ylabel"
)


#' get layer states
#'
#' @noRd
#' @examples
#'
#' \dontrun{
#' p <- l_plot(x = c(0,1), y = c(0,1))
#' l <- l_layer_rectangle(p, x = c(0,1), y = c(0,1))
#'
#' loon:::get_layer_states(p)
#' loon:::get_layer_states(as.vector(p))
#'
#' loon:::get_layer_states(l)
#' loon:::get_layer_states(c(as.vector(p), as.vector(l)))
#' }
#'
get_layer_states <- function(target, omit = NULL) {

    if (!is(target, "loon")) {
        target <- l_create_handle(target)
    }

    if (is(target, "l_layer")) {
        layer <- target
        widget <- l_create_handle(attr(target, "widget"))
        obj_states <- target
    } else {
        widget <- target
        layer <- l_create_handle(c(as.vector(widget), "model"))
        obj_states <- widget
    }

    states_info <- l_info_states(obj_states)
    state_names <- setdiff(names(states_info), c(omit, cartesian_model_widget_states))

    states <- setNames(lapply(state_names,
                              function(state) l_cget(target, state)),
                       state_names)

    # Add Coordinates
    if (!is(layer, "l_layer_group")) {
        states <- c(xy_coords_layer(layer), states)
    }


    # Deal with color
    is_color <- vapply(states_info[state_names],
                       function(s) s$type %in% c("color", "colorOrTransparent"),
                       logical(1))
    if (any(is_color)) {
        for (state_name in state_names[is_color]) {
            states[[state_name]] <- as_hex6color(states[[state_name]])
        }

    }

    states
}


# only works for scatterplot and serialaxes
get_model_display_order <- function(widget) {

    n <- l_cget(widget, "n")

    if (n == 0) {
        numeric(0)
    } else {
        can <- paste0(widget, ".canvas")
        id <- as.numeric(tcl(can, "find", "withtag", paste("layer", "model", sep = "&&")))

        i <- vapply(id, function(id_i) {
            tags <- as.character(tcl(can, "gettags", id_i))

            if (length(tags) >= 4) {
                as.numeric(sub("^item", "", tags[4]))
            } else {
                NA_integer_
            }
        }, numeric(1))

        if (any(is.na(i))) {
            seq_len(n)
        } else {
            i + 1
        }
    }
}

# A helper function to combine loon's justification and anchor
# for text to the correct coordinates for grid
#
getGridTextCoords  <-  function(text, angle, anchor, just){
    textWidth <- stringWidth(text)
    textHeight <- stringHeight(text)
    angle <- angle * pi / 180

    adjustedCoords <- switch(anchor,
                             "center" = unit(c(0, 0), units = "mm"),
                             "n" = {
                                 x <- 1/2 * sin(angle) * textHeight
                                 y <- -1/2 * cos(angle) * textHeight
                                 unit.c(x, y)
                             },
                             "e" = {
                                 x <- -1/2 * cos(angle) * textWidth
                                 y <- -1/2 * sin(angle) * textWidth
                                 unit.c(x, y)
                             },
                             "s" = {
                                 x <- - 1/2 * sin(angle) * textHeight
                                 y <- 1/2 * cos(angle) * textHeight
                                 unit.c(x, y)
                             },
                             "w" = {
                                 x <- 1/2 * cos(angle) * textWidth
                                 y <- 1/2 * sin(angle) * textWidth
                                 unit.c(x, y)
                             },
                             "sw" = {
                                 x <- - 1/2 * sin(angle) * textHeight +
                                     1/2 * cos(angle) * textWidth
                                 y <- 1/2 * cos(angle) * textHeight +
                                     1/2 * sin(angle) * textWidth
                                 unit.c(x, y)
                             },
                             "nw" = {
                                 x <- 1/2 * sin(angle) * textHeight +
                                     1/2 * cos(angle) * textWidth
                                 y <- -1/2 * cos(angle) * textHeight +
                                     1/2 * sin(angle) * textWidth
                                 unit.c(x, y)
                             },
                             "ne" =  {
                                 x <-  1/2 * sin(angle) * textHeight +
                                     (-1/2) * cos(angle) * textWidth
                                 y <- -1/2 * cos(angle) * textHeight +
                                     (-1/2) * sin(angle) * textWidth
                                 unit.c(x,y)
                             },
                             "se" = {
                                 x <- - 1/2 * sin(angle) * textHeight +
                                     (-1/2) * cos(angle) * textWidth
                                 y <- 1/2 * cos(angle) * textHeight +
                                     (-1/2) * sin(angle) * textWidth
                                 unit.c(x ,y)
                             }
    )
    # just can only be "left", "right" and "center"
    if(just == "left") {
        adjustedCoords[1] <- adjustedCoords[1] - 1/2 * cos(angle) * textWidth
        adjustedCoords[2] <- adjustedCoords[2] - 1/2 * sin(angle) * textWidth
    } else if(just == "right") {
        adjustedCoords[1] <- adjustedCoords[1] + 1/2 * cos(angle) * textWidth
        adjustedCoords[2] <- adjustedCoords[2] + 1/2 * sin(angle) * textWidth
    }
    adjustedCoords
}

# TODO Export and fully document
loon.pretty <- function(widget, digits = 3) {

    title <- widget['title']
    xlabel <- widget['xlabel']
    ylabel <- widget['ylabel']
    showScales <- widget['showScales']
    showLabels <- widget['showLabels']
    swap <- widget['swapAxes']
    panX <- widget['panX']
    panY <- widget['panY']
    deltaX <- widget['deltaX']
    deltaY <- widget['deltaY']
    zoomX <- widget['zoomX']
    zoomY <- widget['zoomY']

    if(swap) {
        tcl("set", "xfrom", panY)
        tcl("set", "yfrom", panX)
        tcl("set", "xto", panY + deltaY/zoomY)
        tcl("set", "yto", panX + deltaX/zoomX)
    } else {
        tcl("set", "xfrom", panX)
        tcl("set", "yfrom", panY)
        tcl("set", "xto", panX + deltaX/zoomX)
        tcl("set", "yto", panY + deltaY/zoomY)
    }

    # set the number of axes
    density <- as.numeric(.Tcl('set density $::loon::Options(ticks_density)'))
    canvasWidth <- as.numeric(tkwinfo("width",widget))
    canvasHeight <- as.numeric(tkwinfo("height",widget))

    # margins
    minimumMargins <- widget['minimumMargins']
    margins <- c(0, 0, 0, 0)
    if (showLabels) {
        labelMargins <- widget['labelMargins']
        if(xlabel == "") labelMargins[1] <- minimumMargins[1]
        if(ylabel == "") labelMargins[2] <- minimumMargins[2]
        if(title == "") labelMargins[3] <- minimumMargins[3]
        margins <- margins + labelMargins
    }
    if (showScales) {margins <- margins + widget['scalesMargins'] }
    if(showLabels | showScales) {
        margins <- apply(cbind(margins, minimumMargins), 1, max)
    }

    plotWidth <- canvasWidth - (margins[2] + margins[4])
    plotHeight <- canvasHeight - (margins[1] + margins[3])

    if(plotWidth < 0) plotWidth <- 0
    if(plotHeight < 0) plotHeight <- 0

    tcl("set", "m_x", floor(plotWidth/100 * density))
    tcl("set", "m_y", floor(plotHeight/100 * density))

    xaxis <- round(
        as.numeric(.Tcl('set xaxis [::loon::scales::extended $xfrom $xto [expr {2*$m_x}]]')),
        digits = digits
    )
    yaxis <- round(
        as.numeric(.Tcl('set yaxis [::loon::scales::extended $yfrom $yto [expr {2*$m_y}]]')),
        digits = digits
    )

    is.even <- function(x) !as.logical(x %% 2)

    len.xaxis <- length(xaxis)
    len.yaxis <- length(yaxis)

    xaxis.major <- xaxis[is.even(seq_len(len.xaxis))]
    xaxis.minor <- xaxis[!is.even(seq_len(len.xaxis))]

    yaxis.major <- yaxis[is.even(seq_len(len.yaxis))]
    yaxis.minor <- yaxis[!is.even(seq_len(len.yaxis))]

    list(xaxis.major = xaxis.major,
         xaxis.minor = xaxis.minor,
         yaxis.major = yaxis.major,
         yaxis.minor = yaxis.minor)
}

#' @title Create a named grob or a template grob depending on a test
#'
#' @description Creates and returns a grid object using the function
#' given by `grobFun` when `test` is `TRUE`  Otherwise a simple `grob()`
#' is produced with the same parameters.  All grob parameters are given in `...`.
#'
#' @param test Either `TRUE` or `FALSE` to indicate whether `grobFun` is to be used (default `TRUE`) or not.
#' @param grobFun The function to be used to create the grob when `test = TRUE` (e.g. `textGrob`, `polygonGrob`, etc.).
#' @param name The name to be used for the returned grob.
#' @param ... The arguments to be given to the `grobFun` (or to `grob()` when `test = FALSE`).
#'
#' @return A grob as produced by either the `grobFun` given or by `grob()` using the remaining arguments.
#' If `test = FALSE` then the name is suffixed by ": `grobFun name` arguments".
#'
#' @examples
#' myGrob <- condGrob(test = (runif(1) > 0.5),
#'                    grobFun = textGrob,
#'                    name = "my label",
#'                    label = "Some random text")
#' myGrob
#'
#' @export

condGrob <- function (test = TRUE,
                      grobFun = grob,
                      name = "grob name",
                      ...){
    if (test){
        grobFun(name = name, ...)
    } else {
        grob(name = paste0(name,
                           ": ",
                           deparse(substitute(grobFun)),
                           " arguments"),
        ...)
    }
}
