#' @title Layout Facets in a grid
#' @description It takes a loon widget and forms a matrix of loon widgets
#' layout by aesthetics ("color", "size", "glyph", etc). It can handle mutiple (more than 2) layout categories
#' @param widget A loon widget
#' @param by Layout categories. It could be "color", "size", etc.
#' Available layout categories can be achieved by \code{l_nDimStateNames(your widget)}
#' @param linkingGroup A linkingGroup for widgets. If missing, default would be a paste of
#' "layout" and the current tk path number
#' @param inheritLayers Logical. Should widget layers be inherited into layout panels?
#' @param separate Logical value. Separate several windows or pack all as a whole.
#' @param scales All panels can share the same scales if it is "fixed"; or all panels only vary the
#' scales across rows ("free_x" or "fixed_y"); or all panels only vary the
#' scales across columns ("free_y" or "fixed_x"); or their scales are totally free ("free"). Note that when rows or
#' columns share the same scale, the dynamic chages of scales will be synchronized.
#' @param ncol The number of layout columns
#' @param nrow The number of layout rows
#' @param byrow Are labels display by row or not?
#' @param labels_loc Labels location, "top" or "bottom"?
#' @param span It is used to control the proportion of panel size (height or width) and
#' the label size (label height and label width)
#' @param label_background Label background color
#' @param label_foreground Label foreground color
#' @param label_borderwidth Label border width
#' @param label_relief Label relief
#' @template param_parent
#' @param ... named arguments to modify the `loon` widget states
#'
#' @return an `l_layout` object (an `l_compound` object), being a list with named elements,
#' each representing a separate interactive plot.
#' The names of the plots should be self explanatory and a list
#' of all plots can be accessed from the `l_layout` object via `l_getPlots()`.
#'
#' @export
#'
#' @examples
#' p <- with(mtcars, l_plot3D(mpg, hp, wt, color = cyl))
#' fp <- l_layout_wrap(p, by = "color")
#'
l_layout_wrap <- function(widget,
                          by, linkingGroup,
                          inheritLayers = TRUE,
                          separate = FALSE,
                          scales = c("fixed", "fixed_x", "fixed_y", "free_x", "free_y", "free"),
                          nrow = NULL,
                          ncol = NULL,
                          byrow = TRUE,
                          labels_loc = c("top", "bottom"),
                          span = 10,
                          label_background = "gray80", label_foreground = "black",
                          label_borderwidth = 2,
                          label_relief = c("groove", "flat", "raised", "sunken", "ridge", "solid"),
                          parent = NULL, ...) {
    UseMethod("l_layout_wrap", widget)
}


#' @export
l_layout_wrap.loon <- function(widget,
                               by, linkingGroup,
                               inheritLayers = TRUE,
                               separate = FALSE,
                               scales = c("fixed", "fixed_x", "fixed_y", "free_x", "free_y", "free"),
                               nrow = NULL,
                               ncol = NULL,
                               byrow = FALSE,
                               labels_loc = c("top", "bottom"),
                               span = 10,
                               label_background = "gray80", label_foreground = "black",
                               label_borderwidth = 2,
                               label_relief = c("groove", "flat", "raised", "sunken", "ridge", "solid"),
                               parent = NULL, ...) {

    loon::l_isLoonWidget(widget) || stop(widget, " does not exist")
    if(missing(by)) return(widget)

    layouts <- get_layouts(
        widget, by,
        parent = parent,
        linkingGroup,
        inheritLayers = inheritLayers,
        separate = separate,
        ...)

    if(separate) {
        plots <- layouts$plots
        # scale to plot
        p <- structure(
            plots,
            class = c("l_layout", "l_compound", "loon")
        )
        return(p)
    }

    child <- layouts$child

    ## Two major objectives here
    # 1. pack plots and labels
    # 2. rename and reorder plots
    plots <- facet_wrap_layout(plots = layouts$plots,
                               subtitles = layouts$subtitles,
                               span = span,
                               xlabel = widget['xlabel'],
                               ylabel = widget['ylabel'],
                               title = widget['title'],
                               parent = child,
                               nrow = nrow,
                               ncol = ncol,
                               labels_loc = match.arg(labels_loc),
                               byrow = byrow,
                               label_background = label_background,
                               label_foreground = label_foreground,
                               label_borderwidth = label_borderwidth,
                               label_relief = match.arg(label_relief))


    # forbidden swapAxes showScales and showLabels
    layout_forbiddenSetting(plots,
                            child = child,
                            showLabels = TRUE,
                            swapAxes = widget['swapAxes'])

    scales <- match.arg(scales)
    scales <- switch(scales,
                     "fixed_x" = "free_y",
                     "fixed_y" = "free_x",
                     {
                         scales
                     })

    loonrange <- loonranges(widget)
    xrange <- loonrange$xrange
    yrange <- loonrange$yrange

    layout_wrap_synchronizeSetting(plots, child = child, scales = scales,
                                   xrange = xrange, yrange = yrange,
                                   zoomX = widget['zoomX'], zoomY = widget['zoomY'])

    p <- structure(
        plots,
        class = c("l_layout_wrap", "l_layout", "l_compound", "loon")
    )

    # comment(p) <- names(layouts$plots)
    return(p)
}

#' @export
l_layout_wrap.l_serialaxes <- function(widget,
                                       by, linkingGroup,
                                       inheritLayers = TRUE,
                                       separate = FALSE,
                                       scales = c("fixed", "fixed_x", "fixed_y", "free_x", "free_y", "free"),
                                       nrow = NULL,
                                       ncol = NULL,
                                       byrow = FALSE,
                                       labels_loc = c("top", "bottom"),
                                       span = 10,
                                       label_background = "gray80", label_foreground = "black",
                                       label_borderwidth = 2,
                                       label_relief = c("groove", "flat", "raised", "sunken", "ridge", "solid"),
                                       parent = NULL, ...) {

    loon::l_isLoonWidget(widget) || stop(widget, " does not exist")
    if(missing(by)) return(widget)

    layouts <- get_layouts(
        widget, by,
        parent = parent,
        linkingGroup,
        inheritLayers = inheritLayers,
        separate = separate,
        ...)

    if(separate) {
        plots <- layouts$plots
        # scale to plot
        p <- structure(
            plots,
            class = c("l_layout", "l_compound", "loon")
        )
        return(p)
    }

    child <- layouts$child
    ## Two major objectives here
    # 1. pack plots and labels
    # 2. rename and reorder plots
    plots <- facet_wrap_layout(plots = layouts$plots,
                               subtitles = layouts$subtitles,
                               span = span,
                               xlabel = "",
                               ylabel = "",
                               title = widget['title'],
                               parent = child,
                               nrow = nrow,
                               ncol = ncol,
                               labels_loc = match.arg(labels_loc),
                               byrow = byrow,
                               label_background = label_background,
                               label_foreground = label_foreground,
                               label_borderwidth = label_borderwidth,
                               label_relief = match.arg(label_relief))

    p <- structure(
        plots,
        class = c("l_layout_wrap", "l_layout", "l_compound", "loon")
    )

    # comment(p) <- names(facets$plots)
    return(p)
}

layout_wrap_synchronizeSetting <- function(plots, child, scales,
                                           xrange, yrange,
                                           zoomX = 5/6, zoomY = 5/6) {
    busy <- FALSE
    switch(scales,
           "fixed" = {
               synchronizeXYBindings <- function(W) {
                   if (!busy) {
                       busy <<- TRUE
                       class(W) <- "loon"
                       zoomX <- W['zoomX']
                       panX <- W['panX']
                       deltaX <- W['deltaX']

                       lapply(plots,
                              function(p) {
                                  l_configure(p, zoomX=zoomX, panX=panX, deltaX=deltaX)
                              })

                       zoomY <- W['zoomY']
                       panY <- W['panY']
                       deltaY <- W['deltaY']
                       lapply(plots,
                              function(p) {
                                  l_configure(p, zoomY=zoomY, panY=panY, deltaY=deltaY)
                              })
                       busy <<- FALSE
                       tcl('update', 'idletasks')
                   }
               }

               lapply(plots,
                      function(p) {
                          if(diff(xrange) != 0) {
                              l_configure(p,
                                          panX = xrange[1],
                                          deltaX = zoomX * diff(xrange),
                                          zoomX = zoomX)
                          }
                          if(diff(yrange) != 0) {
                              l_configure(p,
                                          panY = yrange[1],
                                          deltaY = zoomY * diff(yrange),
                                          zoomY = zoomY)
                          }
                      }
               )
               lapply(plots,
                      function(p) {
                          tcl(p, 'systembind', 'state', 'add',
                              c('zoomX', 'panX', 'zoomY', 'panY', 'deltaX', 'deltaY'),
                              synchronizeXYBindings)
                      }
               )
               callbackFunctions$state[[paste(child,"synchronizeXY", sep="_")]] <- synchronizeXYBindings
           },
           "free_x" = {
               # fixed Y
               synchronizeYBindings <- function(W) {
                   if (!busy) {
                       busy <<- TRUE
                       class(W) <- "loon"
                       zoomY <- W['zoomY']
                       panY <- W['panY']
                       deltaY <- W['deltaY']
                       lapply(plots,
                              function(p) {
                                  l_configure(p, zoomY=zoomY, panY=panY, deltaY=deltaY)
                              })
                       busy <<- FALSE
                       tcl('update', 'idletasks')
                   }
               }

               lapply(plots,
                      function(p) {
                          if(diff(yrange) != 0) {
                              l_configure(p,
                                          panY = yrange[1],
                                          deltaY = zoomY * diff(yrange),
                                          zoomY = zoomY)
                          }
                      }
               )
               lapply(plots,
                      function(p) {
                          tcl(p, 'systembind', 'state', 'add',
                              c('zoomY', 'panY', 'deltaY'),
                              synchronizeYBindings)
                      }
               )
               callbackFunctions$state[[paste(child,"synchronizeY", sep="_")]] <- synchronizeYBindings
           },
           "free_y" = {
               # fixed X
               synchronizeXBindings <- function(W) {
                   if (!busy) {
                       busy <<- TRUE
                       class(W) <- "loon"
                       zoomX <- W['zoomX']
                       panX <- W['panX']
                       deltaX <- W['deltaX']

                       lapply(plots,
                              function(p) {
                                  l_configure(p, zoomX=zoomX, panX=panX, deltaX=deltaX)
                              })
                       busy <<- FALSE
                       tcl('update', 'idletasks')
                   }
               }

               lapply(plots,
                      function(p) {
                          if(diff(xrange) != 0) {
                              l_configure(p,
                                          panX = xrange[1],
                                          deltaX = zoomX * diff(xrange),
                                          zoomX = zoomX)
                          }
                      }
               )
               lapply(plots,
                      function(p) {
                          tcl(p, 'systembind', 'state', 'add',
                              c('zoomX', 'panX', 'deltaX'),
                              synchronizeXBindings)
                      }
               )
               callbackFunctions$state[[paste(child,"synchronizeX", sep="_")]] <- synchronizeXBindings
           },
           "free" = NULL)

}

# get widget ranges
loonranges <- function(widget, f = 0.05) {
    if(inherits(widget, "l_plot") || inherits(widget, "l_graph")) {
        xrange <- extendrange(widget["x"], f = f)
        yrange <- extendrange(widget["y"], f = f)
    } else if(inherits(widget, "l_hist")) {

        bins <- getBinData(widget)
        xrange <- c()
        yrange <- c(0)

        lapply(bins,
               function(bin) {
                   xrange <<- c(xrange, bin$x0, bin$x1)
                   yrange <<- c(yrange, bin$count$all)
               })

        xrange <- grDevices::extendrange(xrange, f = f)
        yrange <- grDevices::extendrange(yrange, f = f)
    } else {
        xrange <- NULL
        yrange <- NULL
    }
    list(
        xrange = xrange,
        yrange = yrange
    )
}
