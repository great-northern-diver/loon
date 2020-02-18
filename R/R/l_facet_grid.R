#' @title Layout Facets in a grid
#' @description It takes a loon widget and forms a matrix of loon widgets
#' faceting by aesthetics ("color", "size", "glyph", etc). It can handle mutiple (more than 2) faceting categories
#' @param widget A loon widget
#' @param by Faceting categories. It could be "color", "size", etc.
#' Available faceting categories can be achieved by \code{l_nDimStateNames(your widget)}
#' @param linkingGroup A linkingGroup for widgets. If missing, default would be a paste of
#' "facet" and the current tk path number
#' @param inheritLayers Logical. Should widget layers be inherited into faceting panels?
#' @param byrow Are labels display by row or not?
#' @param column_labels_loc Column labels location, one of "top" or "bottom".
#' @param row_labels_loc Row labels location, one of "right" or "left".
#' @param span It is used to control the proportion of panel size (height or width) and
#' the label size (label height and label width)
#' @param label_background Label background color
#' @param label_foreground Label foreground color
#' @param label_borderwidth Label border width
#' @param label_relief Label relief
#' @template param_parent
#' @param ... named arguments to modify the `loon` widget states
#'
#' @return an `l_facet` object (an `l_compound` object), being a list with named elements,
#' each representing a separate interactive plot.
#' The names of the plots should be self explanatory and a list
#' of all plots can be accessed from the `l_facet` object via `l_getPlots()`.
#'
#' @export
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
#' fp <- l_facet_grid(p, by = "color", linkingGroup = "quakes")
#'
#' s <- l_serialaxes(iris, color = iris$Species, scaling = "observation")
#' fs <- l_facet_grid(s, by = "color")
#' # The linkingGroup can be printed or accessed by
#' l_configure(s, linkingGroup = fs[[1]]['linkingGroup'], sync = "pull")
l_facet_grid <- function(widget,
                         by, linkingGroup,
                         inheritLayers = TRUE,
                         byrow = TRUE,
                         column_labels_loc = c("top", "bottom"),
                         row_labels_loc = c("right", "left"),
                         span = 10,
                         label_background = "gray80", label_foreground = "black",
                         label_borderwidth = 2,
                         label_relief = c("groove", "flat", "raised", "sunken", "ridge", "solid"),
                         parent = NULL, ...) {
    UseMethod("l_facet_grid", widget)
}

#' @export
l_facet_grid.loon <- function(widget,
                              by, linkingGroup,
                              inheritLayers = TRUE,
                              byrow = TRUE,
                              column_labels_loc = c("top", "bottom"),
                              row_labels_loc = c("right", "left"),
                              span = 10,
                              label_background = "gray80", label_foreground = "black",
                              label_borderwidth = 2,
                              label_relief = c("groove", "flat", "raised", "sunken", "ridge", "solid"),
                              parent = NULL, ...) {

    loon::l_isLoonWidget(widget) || stop(widget, " does not exist")
    if(missing(by)) return(widget)

    facets <- get_facets(widget, by,
                         parent = parent,
                         linkingGroup,
                         inheritLayers = inheritLayers,
                         ...)
    child <- facets$child

    ## Two major objectives here
    # 1. pack plots and labels
    # 2. rename and reorder plots
    plots <- facet_grid_layout(plots = facets$plots,
                               subtitles = facets$subtitles,
                               span = span,
                               parent = child,
                               row_labels_loc = match.arg(row_labels_loc),
                               column_labels_loc = match.arg(column_labels_loc),
                               byrow = byrow,
                               label_background = label_background,
                               label_foreground = label_foreground,
                               label_borderwidth = label_borderwidth,
                               label_relief = match.arg(label_relief))

    # forbidden swapAxes showScales and showLabels
    swapAxes <- widget['swapAxes']
    undoStateChanges <- function(W) {
        l_configure(W, showLabels = TRUE, showScales = FALSE, swapAxes = swapAxes)
    }
    lapply(plots,
           function(p) {
               undoStateChanges(p)
               tcl(p, 'systembind', 'state', 'add',
                   c('showLabels', 'showScales', 'swapAxes'),
                   undoStateChanges)
           })
    callbackFunctions$state[[paste(child,"undoStateChanges", sep="_")]] <- undoStateChanges

    p <- structure(
        plots,
        class = c("l_facet_grid", "l_facet", "l_compound", "loon")
    )
    ## Make bindings for scatter synchronizing zoom and pan
    layout_position <- layout_position(p)
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
    return(p)
}

#' @export
l_facet_grid.l_serialaxes <- function(widget,
                                      by, linkingGroup,
                                      inheritLayers = TRUE,
                                      byrow = TRUE,
                                      column_labels_loc = c("top", "bottom"),
                                      row_labels_loc = c("right", "left"),
                                      span = 10,
                                      label_background = "gray80", label_foreground = "black",
                                      label_borderwidth = 2,
                                      label_relief = c("groove", "flat", "raised", "sunken", "ridge", "solid"),
                                      parent = NULL, ...) {

    loon::l_isLoonWidget(widget) || stop(widget, " does not exist")
    if(missing(by)) return(widget)

    facets <- get_facets(widget, by,
                         parent = parent,
                         linkingGroup,
                         inheritLayers = inheritLayers,
                         ...)

    ## Two major objectives here
    # 1. pack plots and labels
    # 2. rename and reorder plots
    plots <- facet_grid_layout(plots = facets$plots,
                               subtitles = facets$subtitles,
                               span = span,
                               parent = facets$child,
                               row_labels_loc = match.arg(row_labels_loc),
                               column_labels_loc = match.arg(column_labels_loc),
                               byrow = byrow,
                               label_background = label_background,
                               label_foreground = label_foreground,
                               label_borderwidth = label_borderwidth,
                               label_relief = match.arg(label_relief))

    structure(
        plots,
        class = c("l_facet_grid", "l_facet", "l_compound", "loon")
    )
}

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
