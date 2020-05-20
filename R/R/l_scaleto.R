
l_scaleto <- function(widget, ...) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'scaleto', ...)
    invisible()
}


#' @title Change Plot Region to Display All Plot Data
#'
#' @description The function modifies the \code{zoomX}, \code{zoomY},
#'   \code{panX}, and \code{panY} so that all elements in the plot are
#'   displayed.
#'
#' @template param_widget
#'
#' @export
l_scaleto_world <- function(widget) {
    UseMethod("l_scaleto_world", widget)
}
#' @export
l_scaleto_world.default <- function(widget) {
    invisible()
}
#' @export
l_scaleto_world.l_plot <- function(widget) {
    l_scaleto(widget, "world")
}
#' @export
l_scaleto_world.l_graph <- function(widget) {
    l_scaleto(widget, "world")
}
#' @export
l_scaleto_world.l_hist <- function(widget) {
    l_scaleto(widget, "world")
}
#' @export
l_scaleto_world.l_compound <- function(widget) {
    lapply(widget,
           function(w) {
               l_scaleto_world(w)
           })
    invisible()
}

#' @title Change Plot Region to Display the All Data of the Model Layer
#'
#' @description The function modifies the \code{zoomX}, \code{zoomY},
#'   \code{panX}, and \code{panY} so that all elements in the model layer of the
#'   plot are displayed.
#'
#' @template param_widget
#' @export
l_scaleto_plot <- function(widget) {
    UseMethod("l_scaleto_plot", widget)
}
#' @export
l_scaleto_plot.default <- function(widget) {
    invisible()
}
#' @export
l_scaleto_plot.l_plot <- function(widget) {
    l_scaleto(widget, "plot")
}
#' @export
l_scaleto_plot.l_graph <- function(widget) {
    l_scaleto(widget, "plot")
}
#' @export
l_scaleto_plot.l_hist <- function(widget) {
    l_scaleto(widget, "plot")
}
#' @export
l_scaleto_plot.l_compound <- function(widget) {
    lapply(widget,
           function(w) {
               l_scaleto_plot(w)
           })
    invisible()
}

#' @title Change Plot Region to Display All Selected Data
#'
#' @description The function modifies the \code{zoomX}, \code{zoomY},
#'   \code{panX}, and \code{panY} so that all selected data points are
#'   displayed.
#'
#' @template param_widget
#' @export
l_scaleto_selected <- function(widget) {
    UseMethod("l_scaleto_selected", widget)
}
#' @export
l_scaleto_selected.default <- function(widget) {
    invisible()
}
#' @export
l_scaleto_selected.l_plot <- function(widget) {
    l_scaleto(widget, "selected")
}
#' @export
l_scaleto_selected.l_graph <- function(widget) {
    l_scaleto(widget, "selected")
}
#' @export
l_scaleto_selected.l_hist <- function(widget) {

    if(!any(l_cget(widget, 'selected'))) {
        warning("No points selected", call. = FALSE)
        return(invisible())
    }

    bins <- l_getBinData(widget)
    xlim <- c()
    ylim <- c(0)

    lapply(bins,
           function(bin) {
               if(bin$count$selected > 0) {
                   xlim <<- c(xlim, bin$x0, bin$x1)
                   ylim <<- c(ylim, bin$count$selected)
               }
           })

    xlim <- grDevices::extendrange(xlim)
    ylim <- grDevices::extendrange(ylim)

    l_configure(widget,
                panX = xlim[1],
                panY = ylim[1],
                zoomX = l_cget(widget, 'deltaX')/diff(xlim),
                zoomY = l_cget(widget, 'deltaY')/diff(ylim))
    invisible()
}
#' @export
l_scaleto_selected.l_compound <- function(widget) {
    lapply(widget,
           function(w) {
               l_scaleto_selected(w)
           })
    invisible()
}

#' @title Change Plot Region to Display All Active Data
#'
#' @description The function modifies the \code{zoomX}, \code{zoomY},
#'   \code{panX}, and \code{panY} so that all active data points are displayed.
#'
#' @template param_widget
#'
#' @export
l_scaleto_active <- function(widget) {
    UseMethod("l_scaleto_active", widget)
}
#' @export
l_scaleto_active.default <- function(widget) {
    invisible()
}
#' @export
l_scaleto_active.l_plot <- function(widget) {
    l_scaleto(widget, "active")
}
#' @export
l_scaleto_active.l_graph <- function(widget) {
    l_scaleto(widget, "active")
}
#' @export
l_scaleto_active.l_hist <- function(widget) {

    bins <- l_getBinData(widget)
    xlim <- c()
    ylim <- c(0)

    lapply(bins,
           function(bin) {
               xlim <<- c(xlim, bin$x0, bin$x1)
               ylim <<- c(ylim, bin$count$all)
           })

    xlim <- grDevices::extendrange(xlim)
    ylim <- grDevices::extendrange(ylim)

    l_configure(widget,
                panX = xlim[1],
                panY = ylim[1],
                zoomX = l_cget(widget, 'deltaX')/diff(xlim),
                zoomY = l_cget(widget, 'deltaY')/diff(ylim))
    invisible()
}
#' @export
l_scaleto_active.l_compound <- function(widget) {
    lapply(widget,
           function(w) {
               l_scaleto_active(w)
           })
    invisible()
}

#' @title Change Plot Region to Display All Elements of a Particular Layer
#'
#' @description The function modifies the \code{zoomX}, \code{zoomY},
#'   \code{panX}, and \code{panY} so that all elements of a particular layer are
#'   displayed.
#'
#' @template param_target
#' @param layer layer id
#'
#' @seealso \code{\link{l_layer_ids}}
#'
#' @export
l_scaleto_layer <- function(target, layer) {
    if (is(target, "l_layer")) {
        layer <- target
        target <- attr(layer, "widget")
    }
    l_scaleto(target, "layer", layer)
}

