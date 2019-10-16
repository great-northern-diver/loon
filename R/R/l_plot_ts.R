#' @title Draw a decomposed time series loon plot
#'
#' @description \code{l_plot_ts} is a generic function for creating a decomposed time series plot. It
#' is mainly used in \code{l_plot.decomposed.ts} and \code{l_plot.stl}
#' @family time series decomposition plotting functions
#' @param x Either an \code{stl} object or a \code{decomposed.ts} object.
#' @param color points colour of all time series.
#'        If \code{NULL} (the default) \code{color} will be \code{l_getOption("foreground")}.
#' @param size points size of all time series. Default value is 1.
#' @param lcolor line colour of all time series. If \code{NULL} (the default) \code{lcolor} will be \code{l_getOption("foreground")}.
#' @param linewidth line width of all time series (incl. original and decomposed components. Default is 1.
#' @param xlabel the labels for the x axes.  This is a length four character vector one for each: of the original
#' time series, the trend component, the seasonality component, and the remainder. If of length 1, the label is repeated; if \code{NULL}, \code{xlabel} is "time".
#' @param ylabel the labels for the vertical axes.  This is a length four character vector one for each: of the original
#' time series, the trend component, the seasonality component, and the remainder. If \code{NULL}, the default,
#' \code{ylabel} will be \code{c("data", "trend", "seasonality", "remainder")}; if a character vector of length 1, the label is repeated four times.
#' @param title an overall title for the entire display. If \code{NULL} (the default), the title will be "Seasonal Trend Analysis".
#' @param tk_title provides an alternative window name to Tk's \code{wm title}.  If \code{NULL}, \code{stl} will be used.
#' @param linkingGroup name of linking group.
#'        If \code{NULL}, one is created from the data name and class associated with \code{stlOrDecomposedTS}.
#' @param showScales a logical as to whether to display the scales on all axes, default is TRUE.
#' @param showGuides a logical as to whether to display background guide lines on all plots, default is TRUE.
#' @param showLabels a logical as to whether to display axes labels on all plots, default is TRUE.
#' @param ... keyword value pairs passed off to \code{l_plot()} which constructs each loon scatterplot component.
#'
#'
#' @return A structure of class \code{"l_ts"} containing four loon plots each representing a part of the decomposition
#' by name: "original", "trend", "seasonal", and "remainder".
#'
#' @seealso \code{\link{l_plot.stl}}, \code{\link{l_plot.decomposed.ts}},
#' \code{\link{stl}}, or \code{\link{decompose}}.
#'
#'

l_plot_ts <- function(x,
                      color = NULL, size = 1,
                      lcolor = NULL, linewidth = 1,
                      xlabel = NULL,  ylabel = NULL,
                      title = NULL, tk_title = NULL,
                      linkingGroup = NULL,
                      showScales=TRUE,
                      showGuides=TRUE,
                      showLabels=TRUE,
                      ...){

    if (is.null(lcolor)) lcolor <- getOption("foreground")
    if (is.null(color)) color <- getOption("foreground")

    stlOrDecomposedTS <- x  # Just to remind us about what x is

    nameOfData <- strsplit(toString(stlOrDecomposedTS$call), ", ")[[1]][2]
    if (is.na(nameOfData)) nameOfData <- "a time series"
    if (is.null(linkingGroup)) linkingGroup <- paste(nameOfData, class(stlOrDecomposedTS))

    if(inherits(stlOrDecomposedTS, "decomposed.ts")){
        data <- stlOrDecomposedTS$x

        if(is.null(data)){
            data <- with(stlOrDecomposedTS,
                         if (type == "additive") {
                             random + trend + seasonal
                         } else {
                             random * trend * seasonal})
        }

        # extract trend, seasonal and remainder
        xy.raw <- xy.coords(data)

        # get rid of the index of na
        index.trend <- which(is.na(stlOrDecomposedTS$trend)== FALSE)
        index.random <- which(is.na(stlOrDecomposedTS$random)== FALSE)

        xy.trend <- list(x = xy.raw$x[index.trend], y = stlOrDecomposedTS$trend[index.trend])
        xy.remainder <- list(x = xy.raw$x[index.random], y = stlOrDecomposedTS$random[index.random])
        xy.seasonal <- list(x = xy.raw$x, y = stlOrDecomposedTS$seasonal)
    } else if(inherits(stlOrDecomposedTS, "stl")){
        stl <- stlOrDecomposedTS$time.series

        # extract trend, seasonal and remainder
        xy.trend <- xy.coords(stl[,"trend"])
        xy.seasonal <- xy.coords(stl[,"seasonal"])
        xy.remainder <- xy.coords(stl[,"remainder"])

        # combine to get original data
        ncomp <- ncol(stl)
        data <- drop(stl %*% rep(1, ncomp))
        xy.raw <- list(x = xy.trend$x, y =data)

    }

    if(is.null(xlabel) == TRUE ){
        xlabel <- rep("time", 4)
    } else {
        if(length(xlabel) == 1) {
            xlabel <- rep(xlabel, 4)
        } else {
            if(length(xlabel) != 4){
                warning("The length of xlabel should be 4, see Arguments xlabel and ylabel")
            }
        }
    }

    if(is.null(ylabel)){
        ylabel <- c("data", "trend", "seasonality", "remainder")
    } else {
        if(length(ylabel) == 1) {
            ylabel <- rep(ylabel, 4)
        } else {
            if(length(ylabel) != 4){
                warning(paste0("The length of ylabel must be 4", "
                           to match data, trend, seasonality, and remainder")
                )
            }
        }
    }

    decompType <- if(inherits(stlOrDecomposedTS, "stl")) "loess" else "moving averages"
    if(is.null(tk_title)){tk_title <- paste0(decompType, " decomposition of ", nameOfData)}
    if(is.null(title)){title <- paste0("Decomposition (", decompType,") of ", nameOfData)
    }

    tt <- tktoplevel()
    tktitle(tt) <- tk_title

    p1 <- l_plot(parent = tt,
                 x = xy.raw$x,
                 y = xy.raw$y,
                 color = color, size = size,
                 ylabel = ylabel[1],
                 xlabel = xlabel[1],
                 title = title,
                 linkingGroup = linkingGroup,
                 showScales = showScales,
                 showGuides = showGuides,
                 showLabels = showLabels,
                 ... )

    l1 <- l_layer_line(p1,
                       x = xy.raw$x,
                       y= xy.raw$y,
                       color= lcolor,
                       linewidth = linewidth, index="end")

    p2 <- l_plot(parent = tt,
                 x = xy.trend$x,
                 y = xy.trend$y,
                 color = color, size = size,
                 ylabel = ylabel[2],
                 xlabel = xlabel[2],
                 linkingGroup = linkingGroup,
                 showScales = showScales,
                 showGuides = showGuides,
                 showLabels = showLabels,
                 ...)

    l2 <- l_layer_line(p2,
                       x= xy.trend$x,
                       y= xy.trend$y,
                       color=lcolor, linewidth = linewidth,
                       index="end")

    p3 <- l_plot(parent = tt,
                 x = xy.seasonal$x,
                 y = xy.seasonal$y,
                 color = color, size=size,
                 ylabel = ylabel[3],
                 xlabel = xlabel[3],
                 linkingGroup = linkingGroup,
                 showScales = showScales,
                 showGuides = showGuides,
                 showLabels = showLabels,
                 ...)

    l3 <- l_layer_line(p3,
                       x = xy.seasonal$x,
                       y = xy.seasonal$y,
                       color = lcolor, linewidth = linewidth ,
                       index="end")

    p4 <- l_plot(parent = tt,
                 x = xy.remainder$x,
                 y = xy.remainder$y,
                 color = color, size=size,
                 ylabel = ylabel[4],
                 xlabel = xlabel[4],
                 linkingGroup = linkingGroup,
                 showScales = showScales,
                 showGuides = showGuides,
                 showLabels = showLabels,
                 ...)
    l4 <- l_layer_line(p4,
                       x = xy.remainder$x,
                       y = xy.remainder$y,
                       color = lcolor, linewidth = linewidth,
                       index="end")



    ## make the canvas resize to fairly small

    for (p in c(p1,p2,p3,p4)) {
        if(p == p1){
            tkconfigure(paste(p,".canvas", sep=''), width=500, height=190)
        }else{
            tkconfigure(paste(p,".canvas", sep=''), width=500, height=150)
        }
    }

    tkpack(p1, p2, p3, p4,  expand = TRUE, fill = "both")


    ## Bind so that they show the same x range
    l_bind_state(p1, c("panX", "zoomX"), function(W)updateZoomPan(W))
    l_bind_state(p2, c("panX", "zoomX"), function(W)updateZoomPan(W))
    l_bind_state(p3, c("panX", "zoomX"), function(W)updateZoomPan(W))
    l_bind_state(p4, c("panX", "zoomX"), function(W)updateZoomPan(W))

    busy <- FALSE
    updateZoomPan <- function(W) {
        if (!busy) {
            busy <<- TRUE
            widget <- l_create_handle(W)
            zoomX <- widget['zoomX'] #l_cget(widget, "zoomX")
            panX <- widget['panX'] #l_cget(widget, "panX")

            lapply(list(p1, p2, p3, p4),
                   function(p) {
                       l_configure(p, panX = panX, zoomX = zoomX)
                   })

            busy <<- FALSE
            tcl('update', 'idletasks')
        }
    }

    structure(
        list(original = p1, trend = p2, seasonal = p3, remainder = p4),
        class = c("l_ts", "l_compound", "loon")
    )
}


#' @rdname l_getPlots
#'
#' @export
l_getPlots.l_ts <- function(target){
    # throw errors if elements of compound are a not loon widget
    lapply(target,
           function(tar){l_throwErrorIfNotLoonWidget(tar) }
    )
    target
}

#' @rdname l_getLocations
#'
#' @export
l_getLocations.l_ts <- function(target){
    nPlots <- length(target)
    list(
        nrow = nPlots,
        ncol = 1,
        layout_matrix = matrix(1:nPlots, nrow = nPlots, ncol =1),
        heights = c(1.3, rep(1, nPlots-1)),
        widths = 1
    )
}
