#' @title Layout Facets in a multiple panels
#' @description It takes a loon widget and forms a matrix of loon widget
#' facets.
#' @name l_facet
#' @export
#' @param widget A loon widget
#' @param by Layout categories which can be \code{loon} n dimensional states, e.g. "color", "size", etc or
#' a length n object. It takes a vector (single facet category), a data frame or a list (multiple facet categories).
#' @param layout layout facets as \code{'grid'}, \code{'wrap'} or \code{'separate'}
#' @param ... named arguments to modify the `loon` widget states
l_facet <- function(widget,
                    by,
                    layout = c("grid", "wrap", "separate"),
                    ...) {
    UseMethod("l_facet", widget)
}

#' @rdname l_facet
#' @param connectedScales How to connect the scales of all panels. It depends on \code{layout}.
#' \itemize{
#' \item{wrap: all panels share 'both' scales (fix 'x' and 'y'),
#' all panels only vary the scales across 'x' (fix 'x' and free 'y'),
#' all panels only vary the scales across 'y' (fix 'y' and free 'x') or
#' 'none' scales are fixed  (free 'x' and 'y').
#' }
#' \item{grid: all panels on the 'cross' (sharing the same row and column) connect the same scales (both 'x' and 'y'),
#' all panels only vary the scales across 'row' (fix 'x' and free 'y'),
#' all panels only vary the scales across 'column' (fix 'y' and free 'x') or
#' 'none' scales are fixed  (free 'x' and 'y').
#' }
#' }
#' Note that 'both' and 'cross', 'row' and 'y', 'column' and 'x' are equivalent for layout \code{grid} and \code{wrap}, respectively.
#' When 'row's ('y') or 'column's ('x') share the same scale, the dynamic chages of scales will be synchronized.
#' @param linkingGroup A linkingGroup for widgets. If missing, default would be a paste of
#' "layout" and the current tk path number.
#' @param ncol The number of layout columns
#' @param nrow The number of layout rows
#' @param inheritLayers Logical value. Should widget layers be inherited into layout panels?
#' @param labelLocation Labels location.
#' \itemize{
#' \item {
#' Length two vector for layout \code{grid}.
#' The first one is used to determine the position of column labels ('top' or 'bottom').
#' The second one is used to determine the position of row labels ('right' or 'left').
#' }
#' \item {
#' Length one vector for layout \code{wrap}, 'top' or 'bottom'.
#' }
#' }
#' @param labelBackground Label background color
#' @param labelForeground Label foreground color
#' @param labelBorderwidth Label border width
#' @param labelRelief Label relief
#' @template param_parent
#' @export
#'
#' @return an `l_facet` object (an `l_compound` object), being a list with named elements,
#' each representing a separate interactive plot.
#' The names of the plots should be self explanatory and a list
#' of all plots can be accessed from the `l_facet` object via `l_getPlots()`.
#'
#' @examples
#' library(maps)
#' p <- with(quakes, l_plot(long, lat, linkingGroup = "quakes"))
#' p["color"][quakes$mag < 5 & quakes$mag >= 4] <- "lightgreen"
#' p["color"][quakes$mag < 6 & quakes$mag >= 5] <- "lightblue"
#' p["color"][quakes$mag >= 6] <- "firebrick"
#' # A Fiji map
#' NZFijiMap <- map("world2", regions = c("New Zealand", "Fiji"), plot = FALSE)
#' l_layer(p, NZFijiMap,
#'         label = "New Zealand and Fiji",
#'         color = "forestgreen",
#'         index = "end")
#' fp <- l_facet(p, by = "color", layout = "grid",
#'               linkingGroup = "quakes")
#'
l_facet.loon <- function(widget,
                         by,
                         layout = c("grid", "wrap", "separate"),
                         connectedScales = c("cross", "row", "column", "both", "x", "y", "none"),
                         linkingGroup,
                         nrow = NULL,
                         ncol = NULL,
                         inheritLayers = TRUE,
                         labelLocation = c("top", "right"),
                         labelBackground = "gray80", labelForeground = "black",
                         labelBorderwidth = 2,
                         labelRelief = c("groove", "flat", "raised", "sunken", "ridge", "solid"),
                         parent = NULL, ...) {

    loon::l_isLoonWidget(widget) || stop(widget, " does not exist")
    if(missing(by)) return(widget)
    layout <- match.arg(layout)

    separate <- layout == "separate"

    facets <- get_facets(widget, by,
                         parent = parent,
                         linkingGroup,
                         inheritLayers = inheritLayers,
                         byDeparse = deparse(substitute(by)),
                         separate = separate,
                         ...)

    # synchronize
    connectedScales <- match.arg(connectedScales)
    connectedScales <- switch(connectedScales,
                              "cross" = "both",
                              "row" = "y",
                              "column" = "x",
                              {
                                  connectedScales
                              })

    loonrange <- loonranges(widget)
    xrange <- loonrange$xrange
    yrange <- loonrange$yrange

    if(separate) {

        plots <- facets$plots

        # force scales
        forceScales(plots = plots,
                    xrange = xrange,
                    yrange = yrange,
                    connectedScales = connectedScales,
                    zoomX = widget['zoomX'],
                    zoomY = widget['zoomY'])

        facet_separate_layout(plots = plots,
                              subtitles = facets$subtitles,
                              title = widget['title'],
                              xlabel = widget['xlabel'],
                              ylabel = widget['ylabel'],
                              ...)

        return(
            structure(
                plots,
                class = c("l_facet", "l_compound", "loon")
            )
        )
    }

    child <- facets$child

    if(layout == "grid") {

        plots <- facet_grid_layout(plots = facets$plots,
                                   subtitles = facets$subtitles,
                                   parent = child,
                                   xlabel = widget['xlabel'],
                                   ylabel = widget['ylabel'],
                                   title = widget['title'],
                                   swapAxes = widget['swapAxes'],
                                   labelLocation = labelLocation,
                                   labelBackground = labelBackground,
                                   labelForeground = labelForeground,
                                   labelBorderwidth = labelBorderwidth,
                                   labelRelief = match.arg(labelRelief))

        if(connectedScales == "both") {

            layout_grid_synchronizeSetting(plots,
                                           xrange = xrange,
                                           yrange = yrange,
                                           child = child,
                                           zoomX = widget['zoomX'], zoomY = widget['zoomY'])

        } else {

            layout_wrap_synchronizeSetting(plots,
                                           child = child,
                                           connectedScales = connectedScales,
                                           xrange = xrange, yrange = yrange,
                                           zoomX = widget['zoomX'], zoomY = widget['zoomY'])
        }

        plots <- structure(
            plots,
            class = c("l_facet_grid", "l_facet", "l_compound", "loon")
        )

    } else if(layout == "wrap") {

        if(length(labelLocation) > 1)
            labelLocation <- labelLocation[1]

        plots <- facet_wrap_layout(plots = facets$plots,
                                   subtitles = facets$subtitles,
                                   xlabel = widget['xlabel'],
                                   ylabel = widget['ylabel'],
                                   title = widget['title'],
                                   parent = child,
                                   swapAxes = widget['swapAxes'],
                                   nrow = nrow,
                                   ncol = ncol,
                                   labelLocation = labelLocation,
                                   labelBackground = labelBackground,
                                   labelForeground = labelForeground,
                                   labelBorderwidth = labelBorderwidth,
                                   labelRelief = match.arg(labelRelief))

        layout_wrap_synchronizeSetting(plots, child = child, connectedScales = connectedScales,
                                       xrange = xrange, yrange = yrange,
                                       zoomX = widget['zoomX'], zoomY = widget['zoomY'])

        plots <- structure(
            plots,
            class = c("l_facet_wrap", "l_facet", "l_compound", "loon")
        )
    } else stop("Unknown layout type")

    # forbidden swapAxes and showLabels
    layout_forbiddenSetting(plots,
                            child = child,
                            showLabels = TRUE,
                            swapAxes = widget['swapAxes'])

    # synchronize scales
    linkOneDimentionalStates(plots, oneDimentionalStates = "showScales")

    return(plots)
}

#' @rdname l_facet
#' @export
#' @examples
#' # serialaxes facets
#' s <- l_serialaxes(iris, color = iris$Species,
#'                   scaling = "observation")
#' fs <- l_facet(s, layout = "wrap", by = iris$Species)
#' # The linkingGroup can be printed or accessed by
#' l_configure(s, linkingGroup = fs[[1]]['linkingGroup'], sync = "pull")
#'
l_facet.l_serialaxes <- function(widget,
                                 by,
                                 layout = c("grid", "wrap", "separate"),
                                 linkingGroup,
                                 nrow = NULL,
                                 ncol = NULL,
                                 labelLocation = c("top", "right"),
                                 labelBackground = "gray80", labelForeground = "black",
                                 labelBorderwidth = 2,
                                 labelRelief = c("groove", "flat", "raised", "sunken", "ridge", "solid"),
                                 parent = NULL, ...) {

    loon::l_isLoonWidget(widget) || stop(widget, " does not exist")
    if(missing(by)) return(widget)
    layout <- match.arg(layout)

    separate <- layout == "separate"

    facets <- get_facets(widget, by,
                         parent = parent,
                         linkingGroup,
                         byDeparse = deparse(substitute(by)),
                         separate = separate,
                         ...)

    if(separate) {

        plots <- facets$plots

        facet_separate_layout(plots = plots,
                              subtitles = facets$subtitles,
                              title = widget['title'])

        return(
            structure(
                plots,
                class = c("l_facet", "l_compound", "loon")
            )
        )
    }

    child <- facets$child

    if(layout == "grid") {

        plots <- facet_grid_layout(plots = facets$plots,
                                   subtitles = facets$subtitles,
                                   parent = child,
                                   xlabel = "",
                                   ylabel = "",
                                   title = widget['title'],
                                   labelLocation = labelLocation,
                                   labelBackground = labelBackground,
                                   labelForeground = labelForeground,
                                   labelBorderwidth = labelBorderwidth,
                                   labelRelief = match.arg(labelRelief))

        plots <- structure(
            plots,
            class = c("l_facet_grid", "l_facet", "l_compound", "loon")
        )

    } else if(layout == "wrap") {

        if(length(labelLocation) > 1)
            labelLocation <- labelLocation[1]

        plots <- facet_wrap_layout(plots = facets$plots,
                                   subtitles = facets$subtitles,
                                   xlabel = "",
                                   ylabel = "",
                                   title = widget['title'],
                                   parent = child,
                                   nrow = nrow,
                                   ncol = ncol,
                                   labelLocation = labelLocation,
                                   labelBackground = labelBackground,
                                   labelForeground = labelForeground,
                                   labelBorderwidth = labelBorderwidth,
                                   labelRelief = match.arg(labelRelief))

        plots <- structure(
            plots,
            class = c("l_facet_wrap", "l_facet", "l_compound", "loon")
        )
    } else stop("Unknown layout type")

    return(plots)
}

######################################## loonGrob ########################################
#' @rdname l_getLocations
#'
#' @export
l_getLocations.l_facet <- function(target) {

    nPlots <- length(target)
    plotNames <- names(target)

    layout_position <- layout_position(target)
    dims <- apply(layout_position, 2, max)
    nrow <- dims[1]
    ncol <- dims[2]

    layout_matrix <- matrix(rep(NA, nrow * ncol), nrow = nrow)
    for(i in seq(dim(layout_position)[1])) {
        layout_matrix[layout_position[i, 1], layout_position[i, 2]] <- i
    }

    list(
        nrow = dims[1],
        ncol = dims[2],
        layout_matrix = layout_matrix,
        heights = NULL,
        widths = NULL
    )
}
#' @rdname l_getPlots
#'
#' @export
l_getPlots.l_facet <- function(target){
    # throw errors if elements of compound are a not loon widget
    lapply(target,
           function(tar){l_throwErrorIfNotLoonWidget(tar) }
    )
    target
}


loonGrob_layoutType.l_facet <- function(target) "locations"

######################################## facets helper function ########################################
layout_wrap_synchronizeSetting <- function(plots, child, connectedScales,
                                           xrange, yrange,
                                           zoomX = 5/6, zoomY = 5/6) {

    # force scales
    forceScales(plots = plots,
                xrange = xrange,
                yrange = yrange,
                connectedScales = connectedScales,
                zoomX = zoomX,
                zoomY = zoomY)

    busy <- FALSE
    switch(connectedScales,
           "both" = {

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
                          tcl(p, 'systembind', 'state', 'add',
                              c('zoomX', 'panX', 'zoomY', 'panY', 'deltaX', 'deltaY'),
                              synchronizeXYBindings)
                      }
               )
               callbackFunctions$state[[paste(child,"synchronizeXY", sep="_")]] <- synchronizeXYBindings
           },
           "y" = {


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
                          tcl(p, 'systembind', 'state', 'add',
                              c('zoomY', 'panY', 'deltaY'),
                              synchronizeYBindings)
                      }
               )
               callbackFunctions$state[[paste(child,"synchronizeY", sep="_")]] <- synchronizeYBindings

           },
           "x" = {
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
                          tcl(p, 'systembind', 'state', 'add',
                              c('zoomX', 'panX', 'deltaX'),
                              synchronizeXBindings)
                      }
               )
               callbackFunctions$state[[paste(child,"synchronizeX", sep="_")]] <- synchronizeXBindings
           },
           "none" = NULL)
}

# get widget ranges
loonranges <- function(widget, f = 0.05) {
    # if(inherits(widget, "l_plot") || inherits(widget, "l_graph")) {
    #     xrange <- extendrange(widget["x"], f = f)
    #     yrange <- extendrange(widget["y"], f = f)
    # } else if(inherits(widget, "l_hist")) {
    #
    #     bins <- getBinData(widget)
    #     xrange <- c()
    #     yrange <- c(0)
    #
    #     lapply(bins,
    #            function(bin) {
    #                xrange <<- c(xrange, bin$x0, bin$x1)
    #                yrange <<- c(yrange, bin$count$all)
    #            })
    #
    #     xrange <- grDevices::extendrange(xrange, f = f)
    #     yrange <- grDevices::extendrange(yrange, f = f)
    # } else {
    #     xrange <- c(0, 0)
    #     yrange <- c(0, 0)
    # }

    if(!inherits(widget, "l_serialaxes") && inherits(widget, "loon")) {

        xrange <- c(widget['panX'], widget['panX'] + widget['deltaX']/widget['zoomX'])
        yrange <- c(widget['panY'], widget['panY'] + widget['deltaY']/widget['zoomY'])

    } else {
        xrange <- c(0, 0)
        yrange <- c(0, 0)
    }

    list(
        xrange = xrange,
        yrange = yrange
    )
}

layout_position <- function(target) {
    plotNames <- names(target)

    layout_position <- t(
        as.matrix(
            as.data.frame(
                lapply(strsplit(vapply(strsplit(plotNames, "x"), function(x) x[2], character(1)), "y"), as.numeric),
                fix.empty.names = FALSE)
        )
    )
    colnames(layout_position) <- c("x", "y")
    layout_position
}

forceScales <- function(plots, xrange, yrange, connectedScales = "both",
                        zoomX = 5/6, zoomY = 5/6) {
    lapply(plots,
           function(p) {
               if(connectedScales == "x" || connectedScales == "both") {
                   if(diff(xrange) != 0) {
                       l_configure(p,
                                   panX = xrange[1],
                                   deltaX = zoomX * diff(xrange),
                                   zoomX = zoomX)
                   }
               }
               if(connectedScales == "y" || connectedScales == "both") {
                   if(diff(yrange) != 0) {
                       l_configure(p,
                                   panY = yrange[1],
                                   deltaY = zoomY * diff(yrange),
                                   zoomY = zoomY)
                   }
               }
           }
    )
}

layout_forbiddenSetting <- function(plots, child,
                                    showLabels = TRUE, swapAxes = FALSE) {
    undoStateChanges <- function(W) {
        l_configure(W, showLabels = showLabels, swapAxes = swapAxes)
    }
    lapply(plots,
           function(p) {
               undoStateChanges(p)
               tcl(p, 'systembind', 'state', 'add',
                   c('showLabels', 'swapAxes'),
                   undoStateChanges)
           })
    callbackFunctions$state[[paste(child,"undoStateChanges", sep="_")]] <- undoStateChanges
}

layout_grid_synchronizeSetting <- function(plots, child, xrange, yrange, zoomX = 5/6, zoomY = 5/6) {

    # force scales
    forceScales(plots = plots,
                xrange = xrange,
                yrange = yrange,
                zoomX = zoomX,
                zoomY = zoomY)

    layout_position <- layout_position(plots)
    plotsHash <- list()
    for (i in 1:length(plots)) {

        tmpX <- which(layout_position[, "y"] == layout_position[i, "y"])
        shareX <- tmpX[tmpX != i]

        tmpY <- which(layout_position[, "x"] == layout_position[i, "x"])
        shareY <- tmpY[tmpY != i]
        plotsHash[[paste("scatter_x_",
                         plots[[i]],
                         sep="")]] <- plots[shareX]
        plotsHash[[paste("scatter_y_",
                         plots[[i]],
                         sep="")]] <- plots[shareY]
    }
    busy <- FALSE
    synchronizeBindings <- function(W) {
        if (!busy) {
            busy <<- TRUE
            class(W) <- "loon"
            zoomX <- W['zoomX']; zoomY <- W['zoomY']
            panX <- W['panX']; panY <- W['panY']
            deltaX <- W['deltaX']; deltaY <- W['deltaY']

            lapply(plotsHash[[paste("scatter_x_", W, sep="")]], function(p) {
                l_configure(p, zoomX=zoomX, panX=panX, deltaX=deltaX)
            })
            lapply(plotsHash[[paste("scatter_y_",W,sep="")]], function(p) {
                l_configure(p, zoomY=zoomY, panY=panY, deltaY=deltaY)
            })
            busy <<- FALSE
            tcl('update', 'idletasks')
        }
    }
    lapply(plots,
           function(p) {
               tcl(p, 'systembind', 'state', 'add',
                   c('zoomX', 'panX', 'zoomY', 'panY', 'deltaX', 'deltaY'),
                   synchronizeBindings)
           }
    )
    callbackFunctions$state[[paste(child,"synchronize", sep="_")]] <- synchronizeBindings
}

updateYshows <- function(plots, swapAxes = FALSE,
                         tkXlabel, tkYlabel, xlabel, ylabel) {

    p <- plots[[1]]

    # sychronize yshows
    if(swapAxes) {
        # xlabel is "yshows"
        if(xlabel != "")

            l_bind_state(p,
                         "yshows",
                         callback = function() {
                             tcltk::tkconfigure(tkXlabel,
                                                text = p['yshows'])
                         })
    } else {
        # ylabel is "yshows"
        if(ylabel != "")
            l_bind_state(p,
                         "yshows",
                         callback = function() {
                             tcltk::tkconfigure(tkYlabel,
                                                text = paste(paste0(" ", strsplit(p['yshows'], "")[[1]], " "), collapse = "\n"))
                         })
    }

    return(invisible())
}
