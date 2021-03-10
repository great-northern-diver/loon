#' @title Layout Facets across multiple panels
#' @description It takes a loon widget and forms a matrix of loon widget
#' facets.
#' @name l_facet
#' @export
#' @param widget A loon widget
#' @param by loon plot can be separated by some variables into mutiple panels.
#' This argument can take a \code{vector}, a \code{list} of same lengths or a \code{data.frame} as input.
#' @param on if the \code{by} is a formula, an optional data frame containing the variables in the \code{by}.
#' If variables in \code{by} is not found in data, the variables are taken from environment(formula),
#' typically the environment from which the function is called.
#' @param layout layout facets as \code{'grid'}, \code{'wrap'} or \code{'separate'}
#' @param connectedScales Determines how the scales of the facets are to be connected depending
#' on which \code{layout} is used.  For each value of \code{layout}, the scales are connected
#' as follows:
#' \itemize{
#' \item{\code{layout = "wrap":}  Across all facets, when \code{connectedScales} is
#'    \itemize{
#'    \item{\code{"x"}, then  only the "x"  scales are connected}
#'    \item{\code{"y"}, then only the "y" scales are connected}
#'    \item{\code{"both"},  both "x" and "y" scales are connected}
#'    \item{\code{"none"},  neither "x" nor "y" scales are connected.}
#'    For any other value, only the "y" scale is connected.
#'    }
#'    }
#' \item{\code{layout = "grid":}  Across all facets, when \code{connectedScales} is
#'    \itemize{
#'    \item{\code{"cross"}, then only the scales in the same row and the same column are connected}
#'    \item{\code{"row"}, then both "x" and "y" scales of facets in the same row are connected}
#'    \item{\code{"column"}, then both "x" and "y" scales of facets in the same column are connected}
#'    \item{\code{"x"}, then all of the "x"  scales are connected (regardless of column)}
#'    \item{\code{"y"}, then all of the "y" scales are connected (regardless of row)}
#'    \item{\code{"both"},  both "x" and "y" scales are connected in all facets}
#'    \item{\code{"none"},  neither "x" nor "y" scales are connected in any facets.}
#'    }
#'    }
#'  }
#' @param ... named arguments to modify the `loon` widget states
l_facet <- function(widget,
                    by,
                    on,
                    layout = c("grid", "wrap", "separate"),
                    ...) {
    UseMethod("l_facet", widget)
}

#' @rdname l_facet
#' @param by loon plot can be separated by some variables into mutiple panels.
#' This argument can take a \code{vector}, a \code{list} of same lengths or a \code{data.frame} as input.
#' @param on if the \code{by} is a formula, an optional data frame containing the variables in the \code{by}.
#' If variables in \code{by} is not found in data, the variables are taken from environment(formula),
#' typically the environment from which the function is called.
#' @param layout layout facets as \code{'grid'}, \code{'wrap'} or \code{'separate'}
#' @param connectedScales Determines how the scales of the facets are to be connected depending
#' on which \code{layout} is used.  For each value of \code{layout}, the scales are connected
#' as follows:
#' \itemize{
#' \item{\code{layout = "wrap":}  Across all facets, when \code{connectedScales} is
#'    \itemize{
#'    \item{\code{"x"}, then  only the "x"  scales are connected}
#'    \item{\code{"y"}, then only the "y" scales are connected}
#'    \item{\code{"both"},  both "x" and "y" scales are connected}
#'    \item{\code{"none"},  neither "x" nor "y" scales are connected.}
#'    For any other value, only the "y" scale is connected.
#'    }
#'    }
#' \item{\code{layout = "grid":}  Across all facets, when \code{connectedScales} is
#'    \itemize{
#'    \item{\code{"cross"}, then only the scales in the same row and the same column are connected}
#'    \item{\code{"row"}, then both "x" and "y" scales of facets in the same row are connected}
#'    \item{\code{"column"}, then both "x" and "y" scales of facets in the same column are connected}
#'    \item{\code{"x"}, then all of the "x"  scales are connected (regardless of column)}
#'    \item{\code{"y"}, then all of the "y" scales are connected (regardless of row)}
#'    \item{\code{"both"},  both "x" and "y" scales are connected in all facets}
#'    \item{\code{"none"},  neither "x" nor "y" scales are connected in any facets.}
#'    }
#'    }
#'  }
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
#' if(interactive()) {
#'   library(maps)
#'   p <- with(quakes, l_plot(long, lat, linkingGroup = "quakes"))
#'   p["color"][quakes$mag < 5 & quakes$mag >= 4] <- "lightgreen"
#'   p["color"][quakes$mag < 6 & quakes$mag >= 5] <- "lightblue"
#'   p["color"][quakes$mag >= 6] <- "firebrick"
#'   # A Fiji map
#'   NZFijiMap <- map("world2", regions = c("New Zealand", "Fiji"), plot = FALSE)
#'   l_layer(p, NZFijiMap,
#'           label = "New Zealand and Fiji",
#'           color = "forestgreen",
#'           index = "end")
#'   fp <- l_facet(p, by = "color", layout = "grid",
#'                 linkingGroup = "quakes")
#'
#'   size <- c(rep(50, 2), rep(25, 2), rep(50, 2))
#'   color <- c(rep("red", 3), rep("green", 3))
#'   p <- l_plot(x = 1:6, y = 1:6,
#'               size = size,
#'               color = color)
#'   g <- l_glyph_add_text(p, text = 1:6)
#'   p['glyph'] <- g
#'   on <- data.frame(Factor1 = c(rep("A", 3), rep("B", 3)),
#'                    Factor2 = rep(c("C", "D"), 3))
#'   cbind(on, size = size, color = color)
#'   fp <- l_facet(p, by = Factor1 ~ Factor2, on = on)
#' }
#'
l_facet.loon <- function(widget,
                         by,
                         on,
                         layout = c("grid", "wrap", "separate"),
                         connectedScales = c("cross", "row", "column", "both", "x", "y", "none"),
                         linkingGroup,
                         nrow = NULL,
                         ncol = NULL,
                         inheritLayers = TRUE,
                         labelLocation = c("top", "right"),
                         labelBackground = "gray80",
                         labelForeground = "black",
                         labelBorderwidth = 2,
                         labelRelief = c("groove", "flat", "raised", "sunken", "ridge", "solid"),
                         parent = NULL,
                         ...) {

    loon::l_isLoonWidget(widget) || stop(widget, " does not exist")
    if(missing(by)) return(widget)
    layout <- match.arg(layout)

    separate <- layout == "separate"

    facets <- get_facets(widget, by, on,
                         parent = parent,
                         linkingGroup,
                         inheritLayers = inheritLayers,
                         bySubstitute = substitute(by),
                         separate = separate,
                         ...)

    if(!is.list(facets)) {
      message(deparse(substitute(widget)),
              " cannot be divided into multiple facets by ",
              deparse(substitute(by)))
      return(facets)
    }

    # synchronize
    connectedScales <- match.arg(connectedScales)
    swapAxes <- widget['swapAxes']

    loonrange <- loonranges(widget)
    xrange <- loonrange$xrange
    yrange <- loonrange$yrange

    if(swapAxes) {
        connectedScales <- switch(connectedScales,
                                  "x" = "y",
                                  "y" = "x",
                                  {
                                      connectedScales
                                  })
    }

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
                                   by = by,
                                   parent = child,
                                   xlabel = widget['xlabel'],
                                   ylabel = widget['ylabel'],
                                   title = widget['title'],
                                   swapAxes = swapAxes,
                                   labelLocation = labelLocation,
                                   labelBackground = labelBackground,
                                   labelForeground = labelForeground,
                                   labelBorderwidth = labelBorderwidth,
                                   labelRelief = match.arg(labelRelief),
                                   new.toplevel = facets$new.toplevel)

        layout_synchronizeSetting(plots,
                                  connectedScales = connectedScales,
                                  xrange = xrange,
                                  yrange = yrange,
                                  child = child,
                                  zoomX = widget['zoomX'], zoomY = widget['zoomY'])

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
                                   swapAxes = swapAxes,
                                   nrow = nrow,
                                   ncol = ncol,
                                   labelLocation = labelLocation,
                                   labelBackground = labelBackground,
                                   labelForeground = labelForeground,
                                   labelBorderwidth = labelBorderwidth,
                                   labelRelief = match.arg(labelRelief),
                                   new.toplevel = facets$new.toplevel)

        if(swapAxes) {
            connectedScales <- switch(connectedScales,
                                      "row" = "column",
                                      "column" = "row", {
                                          connectedScales
                                      })
        }

        layout_synchronizeSetting(plots, child = child, connectedScales = connectedScales,
                                  xrange = xrange, yrange = yrange,
                                  zoomX = widget['zoomX'], zoomY = widget['zoomY'])

        plots <- structure(
            plots,
            class = c("l_facet_wrap", "l_facet", "l_compound", "loon")
        )
    } else stop("Unknown layout type")

    # forbidden swapAxes and showLabels
    swap_forbiddenSetting(plots,
                          child = child,
                          swapAxes = swapAxes)

    # synchronize scales
    linkOneDimensionalStates(plots, oneDimensionalStates = c("showScales", "showLabels", "showGuides"))

    return(plots)
}

#' @rdname l_facet
#' @export
#' @examples
#' if(interactive()) {
#'
#' # serialaxes facets
#' s <- l_serialaxes(iris[, -5], color = iris$Species)
#' fs <- l_facet(s, layout = "wrap", by = iris$Species)
#' # The linkingGroup can be printed or accessed by
#' l_configure(s, linkingGroup = fs[[1]]['linkingGroup'], sync = "pull")
#' }
l_facet.l_serialaxes <- function(widget,
                                 by,
                                 on,
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

    facets <- get_facets(widget, by, on,
                         parent = parent,
                         linkingGroup,
                         bySubstitute = substitute(by),
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
                                   by = by,
                                   parent = child,
                                   xlabel = "",
                                   ylabel = "",
                                   title = widget['title'],
                                   labelLocation = labelLocation,
                                   labelBackground = labelBackground,
                                   labelForeground = labelForeground,
                                   labelBorderwidth = labelBorderwidth,
                                   labelRelief = match.arg(labelRelief),
                                   new.toplevel = facets$new.toplevel)

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
                                   labelRelief = match.arg(labelRelief),
                                   new.toplevel = facets$new.toplevel)

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

l_byArgs <- function() {
    c("nrow",
      "ncol",
      "byrow",
      "labelLocation",
      "labelBackground",
      "labelForeground",
      "labelBorderwidth",
      "labelRelief")
}


######################################## facets helper function ########################################
# get widget ranges
loonranges <- function(widget, f = 0.05) {
    # if(inherits(widget, "l_plot") || inherits(widget, "l_graph")) {
    #     xrange <- extendrange(widget["x"], f = f)
    #     yrange <- extendrange(widget["y"], f = f)
    # } else if(inherits(widget, "l_hist")) {
    #
    #     bins <- l_getBinData(widget)
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
               if(connectedScales == "x" || connectedScales == "both" || connectedScales == "row" || connectedScales == "cross") {
                   if(diff(xrange) != 0) {
                       l_configure(p,
                                   panX = xrange[1],
                                   deltaX = zoomX * diff(xrange),
                                   zoomX = zoomX)
                   }
               }
               if(connectedScales == "y" || connectedScales == "both" || connectedScales == "column" || connectedScales == "cross") {
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

swap_forbiddenSetting <- function(plots, child, swapAxes = FALSE) {
    undoStateChanges <- function(W) {
        l_configure(W, swapAxes = swapAxes)
    }
    lapply(plots,
           function(p) {
               undoStateChanges(p)
               tcl(p, 'systembind', 'state', 'add',
                   c('swapAxes'),
                   undoStateChanges)
           })
    callbackFunctions$state[[paste(child,"undoStateChanges", sep="_")]] <- undoStateChanges
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
