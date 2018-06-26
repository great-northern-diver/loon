#' @title Draw a decomposed time series loon plot
#'
#' @description \code{l_plot_ts} is a generic function for creating a decomposed time seires plot. It
#' is mainly used in \code{l_plot.decomposed.ts} and \code{l_plot.stl}
#'
#' @inheritParams graphics::plot
#' @param timeSeries It can be a \code{stl} object or \code{decomposed.ts} object
#' @param pointsCol points colour
#' @param size points size
#' @param ylabel a length four vector, corresponding y label of original time seires, trend, seasonality
#' and remainder
#' @param xlabel a length four vector, corresponding x label of original time seires, trend, seasonality
#' and remainder
#' @param title an overall title of loon plot
#' @param tk_title provides an alternate interface to Tk's \code{wm title}
#' @param linkingGroup link groups
#' @param linesCol line colour
#' @param linewidth line width
#'
#'
#'
#' @seealso \code{\link{l_plot.stl}} and \code{\link{l_plot.decomposed.ts}}
#'
#'

l_plotForts <- function(timeSeries, pointsCol, size, ylabel, xlabel,
                        title, tk_title, linkingGroup,
                        linesCol, linewidth){
    
    if(class(timeSeries) == "decomposed.ts"){
        data <- timeSeries$x
        
        if(is.null(data)){
            data <- with(timeSeries, if (type == "additive") {random + trend + seasonal
            }else {random * trend * seasonal})
        }
        
        # extract trend, seasonal and remainder
        xy.raw <- xy.coords(data)
        
        # get rid of the index of na
        index.trend <- which(is.na(timeSeries$trend)== F)
        index.random <- which(is.na(timeSeries$random)== F)
        
        xy.trend <- list(x = xy.raw$x[index.trend], y = timeSeries$trend[index.trend])
        xy.remainder <- list(x = xy.raw$x[index.random], y = timeSeries$random[index.random])
        xy.seasonal <- list(x = xy.raw$x, y = timeSeries$seasonal)
        getDataName <- NULL
    }else if(class(timeSeries) == "stl"){
        stl <- timeSeries$time.series
        
        # extract trend, seasonal and remainder
        xy.trend <- xy.coords(stl[,"trend"])
        xy.seasonal <- xy.coords(stl[,"seasonal"])
        xy.remainder <- xy.coords(stl[,"remainder"])
        
        # combine to get original data
        ncomp <- ncol(stl)
        data <- drop(stl %*% rep(1, ncomp))
        xy.raw <- list(x = xy.trend$x, y =data)
        getDataName <- strsplit(toString(timeSeries$call), ", ")[[1]][2]
    }
    
    if(is.null(xlabel) == TRUE ){
        xlabel <- rep("Times", 4)
    }else{
        if(length(xlabel) != 4){
            warning("The length of xlabel should be 4, see Arguments xlabel and ylabel")
        }
    }
    
    if(is.null(ylabel) == TRUE ){
        ylabel <- c( paste(getDataName, "data"), 
                     paste(getDataName, "trend"), 
                     paste(getDataName, "seasonality"),
                     paste(getDataName, "remainder"))
    }else{
        if(length(xlabel) != 4){
            warning("The length of ylabel should be 4, see Arguments xlabel and ylabel")
        }
    }
    
    if(is.null(tk_title)){tk_title <- class(timeSeries) }
    if(is.null(title)){title <- "Seasonal Trend Analysis"}
    
    tt <- tktoplevel()
    tktitle(tt) <- tk_title
    
    p1 <- l_plot(parent = tt, x = xy.raw$x, y= xy.raw$y, color = pointsCol, size = size,
                 ylabel = ylabel[1], xlabel = xlabel[1], title = title,
                 linkingGroup = linkingGroup, showScales=TRUE, showGuides=TRUE, showLabels=TRUE)
    
    l1 <- l_layer_line(p1, x = xy.raw$x, y= xy.raw$y,  color= linesCol,
                       linewidth= linewidth, index="end")
    
    p2 <- l_plot(parent = tt, x = xy.trend$x, y = xy.trend$y, color = pointsCol, size = size,
                 ylabel = ylabel[2], xlabel = xlabel[2],
                 linkingGroup = linkingGroup, showScales=TRUE, showGuides=TRUE, showLabels=TRUE)
    
    l2 <- l_layer_line(p2, x= xy.trend$x, y= xy.trend$y, color=linesCol,
                       linewidth = linewidth,index="end")
    
    p3 <- l_plot(parent = tt, x = xy.seasonal$x, y = xy.seasonal$y, color = pointsCol, size=size,
                 ylabel = ylabel[3], xlabel = xlabel[3],
                 linkingGroup = linkingGroup, showScales=TRUE, showGuides=TRUE, showLabels=TRUE)
    
    l3 <- l_layer_line(p3, x = xy.seasonal$x, y = xy.seasonal$y, color = linesCol,
                       linewidth = linewidth , index="end")
    
    p4 <- l_plot(parent = tt, x = xy.remainder$x, y = xy.remainder$y,color = pointsCol, size=size,
                 ylabel = ylabel[4], xlabel=xlabel[4],
                 linkingGroup = linkingGroup, showScales=TRUE, showGuides=TRUE, showLabels=TRUE)
    l4 <- l_layer_line(p4, x = xy.remainder$x, y = xy.remainder$y, color = linesCol,
                       linewidth = linewidth, index="end")
    
    
    
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
    updateZoomPan <- function(widget) {
        if (!busy) {
            busy <- TRUE
            zoomX <- l_cget(widget, "zoomX")
            panX <- l_cget(widget, "panX")
            
            for (w in c(p1, p2, p3, p4)) {
                l_configure(w, panX=panX, zoomX=zoomX)
            }
            
            busy <- FALSE
        }
    }
    
    structure(
        list(p1 = p1, p2 = p2, p3 = p3, p4 = p4), 
        class = c("l_ts", "loon")
    )
}




