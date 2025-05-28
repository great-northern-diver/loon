#' @title Layer polygons in SpatialPolygonDataFrame
#' @template templ_sp
#' @template param_asSingleLayer
#'
#' @export
l_layer.SpatialPolygonsDataFrame <- function(widget, x, asSingleLayer=TRUE, ...) {
    if (asSingleLayer) {
        xy <- spunlist(spAsList(x))
        structure(l_layer_polygons(widget,
                                   x=sapply(xy, function(xy_i)xy_i$x),
                                   y=sapply(xy, function(xy_i)xy_i$y), ...),
                  hole=as.vector(vapply(xy, function(xy_i)xy_i$isHole, logical(1))),
                  NAME=names(xy))
    } else {
        sapply(x@polygons[x@plotOrder], function(m) {
            l_layer(widget, m, asSingleLayer=asSingleLayer, ...)
        })
    }
}


#' @title Layer polygons in SpatialPolygons object
#' @template templ_sp
#' @template param_asSingleLayer
#'
#' @export
l_layer.SpatialPolygons <- function(widget, x, asSingleLayer=TRUE, ...) {
    if (asSingleLayer) {
        xy <- spunlist(spAsList(x))
        structure(l_layer_polygons(widget,
                                   x=sapply(xy, function(xy_i)xy_i$x),
                                   y=sapply(xy, function(xy_i)xy_i$y), ...),
                  hole=sapply(xy, function(xy_i)xy_i$isHole))
    } else {
        sapply(x@polygons[x@plotOrder], function(m) {
            l_layer(widget, m, asSingleLayer=asSingleLayer, ...)
        })
    }
}

#' @title Layer polygons in Polygons object
#'
#' @template templ_sp
#' @template param_asSingleLayer
#' @export
l_layer.Polygons <- function(widget, x, asSingleLayer=TRUE, ...) {
    #if(attr(x, "package") != "sp")
    #    stop("Polygons method is for objects of the sp package.")

    if (asSingleLayer) {
        xy <- spunlist(spAsList(x))
        structure(l_layer_polygons(widget,
                                   x=sapply(xy, function(xy_i)xy_i$x),
                                   y=sapply(xy, function(xy_i)xy_i$y), ...),
                  hole=sapply(xy, function(xy_i)xy_i$isHole))
    } else {
        sapply(x@Polygons[x@plotOrder], function(m) {
            l_layer(widget, m, ...)
        })
    }
}

#' @title Layer polygon in Polygon object
#' @template templ_sp
#'
#' @export
l_layer.Polygon <- function(widget, x, ...) {
    #if(attr(x, "package") != "sp")
    #   stop("Polygon method iSpatialPolygonsDataFrames for objects of the sp package.")

    l_layer_polygon(widget, x=x@coords[,1], y=x@coords[,2], ...)

    #if (x@hole) {
    #    l_layer_polygon(widget, x=x@coords[,1], y=x@coords[,2],
    #                    linecolor='black', color="", ...)
    #} else {
    #    ## fill
    #    l_layer_pollength(y.tmp)ygon(widget, x=x@coords[,1], y=x@coords[,2],
    #                    color="gray80", linecolor='', ...)
    #}
}

#' @title Layer lines in SpatialLinesDataFrame object
#' @template templ_sp
#' @template param_asSingleLayer
#'
#' @export
l_layer.SpatialLinesDataFrame <- function(widget, x, asSingleLayer=TRUE, ...) {
    if (asSingleLayer) {
        xy <- spunlist(spAsList(x))
        structure(l_layer_lines(widget,
                                   x=sapply(xy, function(xy_i)xy_i$x),
                                   y=sapply(xy, function(xy_i)xy_i$y), ...),
                  NAME=names(xy))
    } else {
        sapply(x@lines, function(m) {
            l_layer(widget, m, asSingleLayer=asSingleLayer, ...)
        })
    }
}



#' @title Layer lines in SpatialLines object
#' @template templ_sp
#' @template param_asSingleLayer
#'
#' @export
l_layer.SpatialLines <- function(widget, x, asSingleLayer=TRUE, ...) {
    if (asSingleLayer) {
        xy <- spunlist(spAsList(x))
        structure(l_layer_lines(widget,
                                   x=sapply(xy, function(xy_i)xy_i$x),
                                   y=sapply(xy, function(xy_i)xy_i$y), ...),
                  hole=sapply(xy, function(xy_i)xy_i$isHole))
    } else {
        sapply(x@lines, function(m) {
            l_layer(widget, m, asSingleLayer=asSingleLayer, ...)
        })
    }
}

#' @title Layer lines in Lines object
#' @template templ_sp
#' @template param_asSingleLayer
#'
#' @export
l_layer.Lines <- function(widget, x, asSingleLayer=TRUE, ...) {
    #if(attr(x, "package") != "sp")
    #    stop("Lines method is for objects of the sp package.")

    if (asSingleLayer) {
        xy <- spunlist(spAsList(x))
        structure(l_layer_lines(widget,
                                   x=sapply(xy, function(xy_i)xy_i$x),
                                   y=sapply(xy, function(xy_i)xy_i$y), ...),
                  hole=sapply(xy, function(xy_i)xy_i$isHole))
    } else {
        sapply(x@Lines, function(m) {
            l_layer(widget, m, ...)
        })
    }
}

#' @title Layer line in Line object
#' @template templ_sp
#'
#' @export
l_layer.Line <- function(widget, x, ...) {
    #if(attr(x, "package") != "sp")
    #   stop("Line method iSpatialLinesDataFrames for objects of the sp package.")

    l_layer_line(widget, x=x@coords[,1], y=x@coords[,2], ...)
}



#' @title Layer points in SpatialPointsDataFrame object
#' @template templ_sp
#'
#' @param asMainLayer if \code{TRUE} and the widget is a scatterplot widget,
#'   then points can be chosen to be added to the \code{'model'} layer
#'
#' @export
l_layer.SpatialPointsDataFrame <- function(widget, x, asMainLayer=FALSE, ...) {
    if (asMainLayer && l_layer_getType(widget, 'model') == 'scatterplot') {
        if(!as.logical(tcl('info', 'object', 'isa', 'typeof',
                           widget, '::loon::classes::Scatterplot_Model'))) {
            stop("widget is not a scatterplot.")
        }
        l_configure(widget, x=x@coords[,1], y=x@coords[,2], ...)
        layer <- widget
    } else {
        layer <- l_layer_points(widget, x=x@coords[,1], y=x@coords[,2], ...)
    }

    if(!is.null(x@data$NAME)) {
        attr(layer, "NAME") <- as.character(x@data$NAME)
    } else if (!is.null(x@data$name)) {
        attr(layer, "NAME") <- as.character(x@data$name)
    } else {
        attr(layer, "NAME") <- NULL
    }

    return(layer)
}

#' @title Layer points in SpatialPoints object
#' @template templ_sp
#' @inheritParams l_layer.SpatialPointsDataFrame
#'
#' @export
l_layer.SpatialPoints <- function(widget, x, asMainLayer=FALSE, ...) {
    if (asMainLayer) {
        if(!as.logical(tcl('info', 'object', 'isa', 'typeof',
                           widget, '::loon::classes::Scatterplot_Model'))) {
            stop("widget is not a scatterplot.")
        }
        l_configure(widget, x=x@coords[,1], y=x@coords[,2], ...)
        layer <- widget
    } else {
        layer <- l_layer_points(widget, x=x@coords[,1], y=x@coords[,2], ...)
    }
    return(layer)
}

## Helper Functions

#' @title Create a list of polygons or lines from a spatial data object from the \code{sp} package.
#'
#' @description
#' \code{spAsList} is a helper function that should rarely be called directly by the user.
#' It is an \code{S3} generic function which takes the spatial data object and returns its
#' components (polygons, lines, et cetera) in a list. Each element could itself be a list.
#' Different \code{S3} methods are implemented for various spatial data types.
#'
#' @param x An \code{sp} spatial data object.
#'
#' @return A list of the relevant components of the spatial data object.
#'
#' @seealso \code{\link{spunlist}}
#'
#' @export
#' @keywords internal
#'
#' @examples
#'
#' \dontrun{
#' library(sp)
#' library(rworldmap)
#' world <- getMap(resolution = "coarse")
#' class(world)
#' isS4(world)
#' xy <- spAsList(world)
#'
#' names(xy)
#' # because tree has same depth for every leaf unlist is ok
#' # This is not true otherwise.
#' uxy <- spunlist(xy)
#' unlist(xy, recursive=FALSE)
#' names(uxy)
#'
#' # here, unlist would be wrong.
#' }
#'
spAsList <- function(x) {
    UseMethod("spAsList")
}


#' @export
#' @keywords internal
spAsList.default <- function(x) {
    stop("x is not of class SpatialPolygonsDataFrame,
         SpatialPolygons, Polygons, or Polygon")
}

#' @export
#' @keywords internal
spAsList.SpatialPolygonsDataFrame <- function(x) {
    o <- lapply(x@polygons[x@plotOrder], function(m) {
        spAsList(m)
    })

    if(!is.null(x@data$NAME)) {
        names(o) <- as.character(x@data$NAME)[x@plotOrder]
    } else if (!is.null(x@data$name)) {
        names(o) <- as.character(x@data$name)[x@plotOrder]
    }
    o
}

#' @export
#' @keywords internal
spAsList.SpatialPolygons <- function(x) {
    lapply(x@polygons[x@plotOrder], function(m) {
        spAsList(m)
    })
}

#' @export
#' @keywords internal
spAsList.Polygons <- function(x) {
    lapply(x@Polygons[x@plotOrder], function(m) {
        spAsList(m)
    })
}

#' @export
#' @keywords internal
spAsList.Polygon <- function(x) {
    list(x=x@coords[,1], y=x@coords[,2], isHole=x@hole)
}


## Lines
##
#' @export
#' @keywords internal
spAsList.SpatialLinesDataFrame <- function(x) {
    o <- lapply(x@lines, function(m) {
        spAsList(m)
    })

    if(!is.null(x@data$NAME)) {
        names(o) <- as.character(x@data$NAME)
    } else if (!is.null(x@data$name)) {
        names(o) <- as.character(x@data$name)
    }
    o
}


#' @export
#' @keywords internal
spAsList.SpatialLines <- function(x) {
    lapply(x@Lines, function(m) {
        spAsList(m)
    })
}

#' @export
#' @keywords internal
spAsList.Lines <- function(x) {
    lapply(x@Lines, function(m) {
        spAsList(m)
    })
}

#' @export
#' @keywords internal
spAsList.Line <- function(x) {
    list(x=x@coords[,1], y=x@coords[,2])
}


#' @title Create a flat list of polygon specifications
#' from the list of \code{sp} components returned by \code{spAsList}
#'
#' @description
#' \code{spunlist} is a simple helper function taking the output
#' from \code{spAsList}. It is a helper function that should
#' rarely be called directly by the user.
#' It is not the same as the base \code{unlist}.
#'
#' @param x An \code{list} spatial data object.
#'
#' @return An appropriately flattened list of the relevant components of the spatial data object.
#'
#' @seealso \code{\link{spAsList}}
#'
#' @export
#' @keywords internal
#'
#' @examples
#'
#' a <- list(list(x=1:2, y=1:2),
#'           list(list(x=1:3, y=1:3),
#'                list(x=1:4, y=1:4)))
#' spunlist(a)
#'
#' # Compare to
#' unlist(a, recursive=TRUE)
#' # or to
#' unlist(a, recursive=FALSE)
#'
spunlist <- function(x) {
    isLeaf <- function(x) {
        length(x) <= 3 && !is.null(names(x)) && all(names(x)[1:2] == c('x','y'))
    }
    if(isLeaf(x)) {
        y <- list(x)
    } else {
        y <- list()
        if (!is.null(names)) {
            ## Propagate Names
            nam <- names(x)
            for (i in seq_along(x)) {
                old_nam <- names(y)
                if(isLeaf(x)) {
                    # leaf
                    y <- append(y, list(x[[i]]))
                    n.tmp <- 1
                } else {
                    # parent
                    y.tmp <- spunlist(x[[i]])
                    n.tmp <- length(y.tmp)
                    y <- append(y, y.tmp)
                }
                names(y) <- c(old_nam, rep(nam[i], n.tmp))
            }
        } else {
            for (x_i in x) {
                if(isLeaf(x)) {
                    # leaf
                    y <- append(y, list(x_i))
                } else {
                    # parent
                    y <- append(y, spunlist(x_i))
                }
            }
        }
    }
    y
}
