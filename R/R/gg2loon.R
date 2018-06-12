#' @title Create a loon plot from a ggplot2 object
#' 
#' @param ggplotObject a ggplot object
#' 
#' @return a loon widget
#' 
#' @import ggplot2
#' 
#' @export
#' 
#' @examples
#' 
#'  library(ggplot2)
#'  p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()
#'  p
#'  g <- gg2loon(p)
#'  
#'  df <- data.frame(x = 1:3, y = 1:3, colour = c(1,3,5))
#'  xgrid <- with(df, seq(min(x), max(x), length = 50))
#'  interp <- data.frame(
#'    x = xgrid,
#'    y = approx(df$x, df$y, xout = xgrid)$y,
#'    colour = approx(df$x, df$colour, xout = xgrid)$y)
#'    
#'  p <- ggplot(interp, aes(x, y, colour = colour)) + 
#'    geom_line(size = 2) +
#'    geom_point(data = df, size = 5)
#'  p
#'  g <- gg2loon(p)

gg2loon <- function(ggplotObject, ... ){
  # lables
  ggLabels <- list(
    title  = if( is.null(ggplotObject$labels$title) ) "" else ggplotObject$labels$title , 
    xlabel = if( is.null(ggplotObject$labels$x) ) ""  else ggplotObject$labels$x , 
    ylabel = if( is.null(ggplotObject$labels$y) ) ""  else ggplotObject$labels$y 
  )
  # ggplot_build  
  buildggplotObject <-  ggBuild2Loon(ggplotObject)
  # layout
  ggLayout <- buildggplotObject$layout$layout
  # panel_params
  ggplotPanel_params <- buildggplotObject$layout$panel_params
  # number of panels
  panelNum <- dim(ggLayout)[1]
  
  tt <- tktoplevel()
  # length layers
  len_layers <- length(ggplotObject$layers)
  # default zoomX and zoomY in loon
  
  zoomX <- zoomY <- 5/6
  p <- lapply(1:panelNum, function(i){
    # panel scale
    panX <- ggplotPanel_params[[i]]$x.range[1]
    panY <- ggplotPanel_params[[i]]$y.range[1]
    deltaX <- diff(ggplotPanel_params[[i]]$x.range) * zoomX
    deltaY <- diff(ggplotPanel_params[[i]]$y.range) * zoomY 
    # subtitle
    if(dim(ggLayout)[2] == 6){
      if(ggLabels$title == "") title <- as.character(ggLayout[i, 4]) else 
        title <- paste(c(ggLabels$title, ":", as.character(ggLayout[i, 4]) ), collapse = "")
    }else title <- ggLabels$title
    
    if(len_layers != 0) {
      layerNames <- sapply(1:len_layers, function(j) {
        className <- class(ggplotObject$layers[[j]]$geom) 
        className[-which(className %in% c("ggproto"  ,"gg" ,"Geom"))]
      })
      # take the point layer as scatterplot ("model" in loon)
      pointsLayerId <- which(sapply(layerNames, function(l){"GeomPoint" %in% l}) == TRUE)
      if( length(pointsLayerId) != 0 ){
        # combine points layers
        pointData <- lapply(pointsLayerId, function(l){
          Layerl <- buildggplotObject$data[[l]]
          data <- Layerl[Layerl$PANEL == i, ]
          x <- data$x
          y <- data$y
          label <- data$label
          color <- sapply(1:dim(data)[1], function(j){
            if(data$shape[j] %in% 21:24 ){
              hex6to12(data$fill[j])
            }else {
              hex6to12(data$colour[j])
            }
          } )
          glyph <- pch_to_glyph(data$shape, data$alpha)
          size <- as_loon_size( data$size , "points" )
          data.frame(x = x, y = y, label = label, color = color, glyph = glyph, size = size)
        })
        pointData <- do.call(rbind, pointData)
        loonPlot <- l_plot(parent = tt, x = pointData$x, y = pointData$y, size = pointData$size, 
                           title = title, color = as.character( pointData$color), 
                           glyph = as.character( pointData$glyph), 
                           xlabel = ggLabels$xlabel, ylabel = ggLabels$ylabel, 
                           itemLabel = as.character( pointData$label ),
                           showGuides = TRUE, showScales = TRUE, showLabels = TRUE, showItemLabels = TRUE)
        
        loon_layers <- sapply(1:len_layers, function(j){
          if( ! j %in% pointsLayerId ){
            loonLayer(widget = loonPlot, layerGeom = ggplotObject$layers[[j]], 
                      data =  buildggplotObject$data[[j]][buildggplotObject$data[[j]]$PANEL == i, ], 
                      ggplotPanel_params = ggplotPanel_params[[i]]
            )
          }
        })
        # recover the points layer to the original position
        if(length(pointsLayerId) != len_layers){
          otherLayerId <- (1:len_layers)[-pointsLayerId]
          minOtherLayerId <- min(otherLayerId)
          maxPointsLayerId <- max(pointsLayerId)
          if(maxPointsLayerId > minOtherLayerId){
            modelLayerup <- sapply( 1:length(which(otherLayerId < maxPointsLayerId) == T), function(j){
              l_layer_raise(loonPlot, "model")
            })
          }
        }
      } else {
        loonPlot <- l_plot(parent = tt, title = title, 
                           xlabel = ggLabels$xlabel, ylabel = ggLabels$ylabel, 
                           showGuides = TRUE, showScales = TRUE, showLabels = TRUE)
        
        loon_layers <- sapply(1:len_layers, function(j){
          loonLayer(widget = loonPlot, layerGeom = ggplotObject$layers[[j]], 
                    data =  buildggplotObject$data[[j]][buildggplotObject$data[[j]]$PANEL == i, ], 
                    ggplotPanel_params = ggplotPanel_params[[i]])
        })
      }
    } else loonPlot <- l_plot(parent = tt, title = title, 
                              xlabel = ggLabels$xlabel, ylabel = ggLabels$ylabel, 
                              showGuides = TRUE, showScales = TRUE, showLabels = TRUE, ... ) 

    
    tkgrid(loonPlot, row = ggLayout[i,]$ROW, 
           column=ggLayout[i,]$COL, sticky="nesw")
    tkgrid.columnconfigure(tt, ggLayout[i,]$COL, weight=1)
    tkgrid.rowconfigure(tt, ggLayout[i,]$ROW, weight=1)
    l_configure(loonPlot, panX=panX, panY=panY, deltaX= deltaX, 
                deltaY=deltaY, zoomX = zoomX, zoomY = zoomY)
    loonPlot
  })
  
  class(p) <- c("gg2loon", "loon")
  p
  
}


loonLayer <- function(widget, layerGeom, data, ggplotPanel_params){
  UseMethod("loonLayer", layerGeom$geom)
}

########################################### basic layers ###########################################
# 1. l_layer_lines does not work well on "dash" (l_layer_line is fine) 
# 2. arrow for every l_layer_line and l_layer_lines need to be added
# 3. legend
# 4. transparent color (maybe impossible to do)
# 5. specfic TODOs (before some loonLayer functions)
# 6. ggplot_build: need to rebuild for some specific data (eg: ts data)
# 7. geom_histogram: transform to l_hist() or just leave it as l_plot() adding l_layer_rectangles()
# 8. bar labels
loonLayer.GeomPoint <- function(widget, layerGeom, data, ggplotPanel_params){
  x <- data$x
  y <- data$y
  pointsColor <- if(!is.null(data$shape) ){
    sapply(1:dim(data)[1], function(j){
      if(data$shape[j] %in% 21:24){
        hex6to12(data$fill[j])
      }else {
        hex6to12(data$colour[j]) }
    } ) 
  } else hex6to12(data$colour)
  pointsSize <- as_loon_size( data$size, "points" )
  
  l_layer_points(widget, x = x, y = y, color = pointsColor, 
                 size = pointsSize)
}


loonLayer.GeomRect <- function(widget, layerGeom, data, ggplotPanel_params){
  n <- dim(data)[1]
  fillColor <- hex6to12(data$fill)
  linesColor <- hex6to12(data$colour)
  linesWidth <- as_loon_size(data$size, "lines")
  
  xrange <- ggplotPanel_params$x.range
  yrange <- ggplotPanel_params$y.range
    
  if(n == 1){
    l_layer_rectangle(
      widget, x = data$x, y = data$y,
      color = fillColor, 
      linecolor = linesColor,
      linewidth = linesWidth
    )
  }else{
    x <- lapply(1:dim(data)[1], function(i){
      c(if(is.infinite(data[i,]$xmin)) xrange[1] else data[i,]$xmin,
        if(is.infinite(data[i,]$xmax)) xrange[2] else data[i,]$xmax)
    })
    y <- lapply(1:dim(data)[1], function(i){
      c(if(is.infinite(data[i,]$ymin)) yrange[1] else data[i,]$ymin,
        if(is.infinite(data[i,]$ymax)) yrange[2] else data[i,]$ymax)
    })
    
    l_layer_rectangles(
      widget, x = x, y = y,
      color = fillColor,
      linecolor = linesColor,
      linewidth = linesWidth
    )
  }
}


loonLayer.GeomPolygon <- function(widget, layerGeom, data, ggplotPanel_params){
  
  uniGroup <- unique(data$group)
  
  fillColor <- hex6to12(data$fill)
  linesColor <- hex6to12(data$colour)
  linesWidth <- as_loon_size(data$size, "lines")
  
  if(length(uniGroup) == 1){
    l_layer_polygon(
      widget, x = data$x, y = data$y,
      color = fillColor[1], linecolor = linesColor[1], linewidth = linesWidth[1]
    )
  }else{
    x <- lapply(1:length(uniGroup), function(i){
      data$x[data$group == uniGroup[i]]
    })
    y <- lapply(1:length(uniGroup), function(i){
      data$y[data$group == uniGroup[i]]
    })
    fillColor <- sapply(1:length(uniGroup), function(i){
      fillColor[data$group == uniGroup[i]][1]
    })
    linesColor <- sapply(1:length(uniGroup), function(i){
      linesColor[data$group == uniGroup[i]][1]
    })
    linesWidth <- sapply(1:length(uniGroup), function(i){
      linesWidth[data$group == uniGroup[i]][1]
    })
    l_layer_polygons(
      widget, x = x, y = y,
      color = fillColor, linecolor = linesColor, linewidth = linesWidth
    )
  }
}

# TODO overlap
loonLayer.GeomText <- function(widget, layerGeom, data, ggplotPanel_params){
  textsSize <- as_loon_size(data$size, "texts")
  textsColor <- hex6to12(data$colour)
  textAnchor <- as_loon_hvjust(hjust = data$hjust, vjust = data$vjust)
  if(dim(data)[1] == 1){
    l_layer_text(
      widget, x = data$x, y = data$y, text = as.character( data$label ), size = textsSize,
      angle = data$angle, color =  textsColor, anchor = textAnchor, justify = "left")
  }else{
    l_layer_texts(
      widget, x = data$x, y = data$y, text = as.character( data$label ), size = textsSize,
      angle = data$angle, color = textsColor, anchor = textAnchor, justify = "left")
  }
}

# TODO draws a rectangle behind the text
loonLayer.GeomLabel <- function(widget, layerGeom, data, ggplotPanel_params){
  loonLayer.GeomText(widget, layerGeom, data, ggplotPanel_params)
}

loonLayer.GeomVline <- function(widget, layerGeom, data, ggplotPanel_params){
  n <- dim(data)[1]
  linesWidth <- as_loon_size(data$size, "lines")
  linesColor <- hex6to12(data$colour)
  yrange <- ggplotPanel_params$y.range
  
  if(n == 1){
    
    linesDash <- as_loon_dash(data$linetype)
    x <- rep(data$xintercept, 2)
    y <- yrange
    l_layer_line(widget, x = x, y = y, linewidth = linesWidth,
                 color = linesColor, dash = linesDash[[1]])
  }else{
    x <- lapply(1:n, function(i){
      rep(data[i,]$xintercept, 2)
    })
    y <- lapply(1:n, function(i){
      yrange
    })
    l_layer_lines(widget, x = x, y = y, linewidth = linesWidth,
                  color = linesColor)
  }
}

loonLayer.GeomHline <- function(widget, layerGeom, data, ggplotPanel_params){
  n <- dim(data)[1]
  linesWidth <- as_loon_size(data$size, "lines")
  linesColor <- hex6to12(data$colour)
  xrange <- ggplotPanel_params$x.range
  linesDash <- as_loon_dash(data$linetype)
  
  if(n == 1){
    y <- rep(data$yintercept, 2)
    x <- xrange
    l_layer_line(widget, x = x, y = y, linewidth = linesWidth,
                 color = linesColor, dash = linesDash[[1]])
  }else{
    y <- lapply(1:n, function(i){
      rep(data[i,]$yintercept, 2)
    })
    x <- lapply(1:n, function(i){
      xrange
    })
    l_layer_lines(widget, x = x, y = y, linewidth = linesWidth,
                  color = linesColor)
  }
}

loonLayer.GeomAbline <- function(widget, layerGeom, data, ggplotPanel_params){
  n <- dim(data)[1]
  linesWidth <- as_loon_size(data$size, "lines")
  linesColor <- hex6to12(data$colour)
  xrange <- ggplotPanel_params$x.range
  yrange <- ggplotPanel_params$y.range
  linesDash <- as_loon_dash(data$linetype)
  if(n == 1){

    x <- xrange
    if(data$slope > 0) {
      y <- c(data$intercept + data$slope * x[1], data$intercept + data$slope * x[2])
      if(y[1] < yrange[1] ) {
        x[1] <- (yrange[1] -data$intercept)/ data$slope
        y[1] <- yrange[1]
      }
      if(y[2] > yrange[2]){
        x[2] <- (yrange[2] -data$intercept)/ data$slope
        y[2] <- yrange[2]
      }
    } else {
      y <- c(data$intercept + data$slope * x[2], data$intercept + data$slope * x[1])
      if(y[1] < yrange[1] ) {
        x[2] <- (yrange[1] -data$intercept)/ data$slope
        y[1] <- yrange[1]
      }
      if(y[2] > yrange[2]){
        x[1] <- (yrange[2] -data$intercept)/ data$slope
        y[2] <- yrange[2]
      }
    }

    l_layer_line(widget, x = x, y = y, linewidth = linesWidth,
                 color = linesColor, dash = linesDash[[1]])
  }else{

    x <- lapply(1:n, function(i){
      xrange
    })
    
    y <- lapply(1:n, function(i){
      if(data[i, ]$slope > 0) {
        ylim <- c(data[i, ]$intercept + data[i, ]$slope * xrange[1],   
                  data[i, ]$intercept + data[i, ]$slope * xrange[2])
        if(ylim[1] < yrange[1] ) {
          x[[i]][1] <- (yrange[1] -data[i, ]$intercept)/ data[i, ]$slope
          ylim[1] <- yrange[1]
        }
        if(ylim[2] > yrange[2]){
          x[[i]][2] <- (yrange[2] -data[i, ]$intercept)/ data[i, ]$slope
          ylim[2] <- yrange[2]
        }
      } else {
        ylim <- c(data[i, ]$intercept + data[i, ]$slope * x[2], 
                  data[i, ]$intercept + data[i, ]$slope * x[1])
        if(ylim[1] < yrange[1] ) {
          x[[i]][2] <- (yrange[1] -data[i, ]$intercept)/ data[i, ]$slope
          ylim[1] <- yrange[1]
        }
        if(ylim[2] > yrange[2]){
          x[[i]][1] <- (yrange[2] -data[i, ]$intercept)/ data[i, ]$slope
          ylim[2] <- yrange[2]
        }
      }
      ylim
    })
    
    l_layer_lines(widget, x = x, y = y, linewidth = linesWidth,
                  color = linesColor)
  }
}

loonLayer.GeomSegment <- function(widget, layerGeom, data, ggplotPanel_params){
  n <- dim(data)[1]
  linesWidth <- as_loon_size(data$size, "lines")
  linesColor <- hex6to12(data$colour)
  linesDash <- as_loon_dash(data$linetype)
  
  if(n == 1){
    x <- c(data$x, data$xend)
    y <- c(data$y, data$yend)
    
    l_layer_line(widget, x = x, y = y, linewidth = linesWidth,
                 color = linesColor, dash = linesDash[[1]])
  }else{
    x <- lapply(1:n, function(i){
      c(data[i,]$x, data[i,]$xend)
    })
    y <- lapply(1:n, function(i){
      c(data[i,]$y, data[i,]$yend)
    })
    l_layer_lines(widget, x = x, y = y, linewidth = linesWidth,
                  color = linesColor)
  }
}

# TODO
loonLayer.GeomCurve <- function(widget, layerGeom, data, ggplotPanel_params){
  NULL
}

loonLayer.GeomDotplot <- function(widget, layerGeom, data, ggplotPanel_params){
  uniGroup <- unique(data$group)
  fillColor <- hex6to12(data$fill)
  lineColor <- hex6to12(data$colour)
  
  stackRatio <- layerGeom$geom_params$stackratio
  countId <- data$countidx
  n <- dim(data)[1]
  stackPos <- sapply(1:n, function(i) {
    if(i == 1) data$stackpos[i] else {
      data$stackpos[i - countId[i] + 1] + (countId[i] - 1) * stackRatio
    }
  })
  
  dotsGroup <- l_layer_group(widget, "dots")
  radius <- data$binwidth[1]/2
  if(layerGeom$geom_params$binaxis == "y"){
    lapply(1:n, function(i){
      xradius <- diff(ggplotPanel_params$x.range)/diff(ggplotPanel_params$y.range)*radius
      l_layer_oval(widget, parent = dotsGroup,
                   x = c(data$x[i] + stackPos[i] * 2 * xradius - xradius,  
                         data$x[i] + stackPos[i] * 2 * xradius + xradius), 
                   y = c(data$y[i] - radius, data$y[i] + radius),color =  fillColor[i], 
                   linecolor = lineColor[i])
    })
  } else  {
    yradius <- diff(ggplotPanel_params$y.range)/diff(ggplotPanel_params$x.range)*radius
    lapply(1:n, function(i){
      l_layer_oval(widget, parent = dotsGroup,
                   x = c(data$x[i] - radius, data$x[i] + radius), 
                   y = c(data$y[i] - yradius + stackPos[i] * 2 * yradius, 
                         data$y[i] + yradius + stackPos[i] * 2 * yradius),color =  fillColor[i], 
                   linecolor = lineColor[i])
    })
  }
}
########################################### advanced layers ###########################################

loonLayer.GeomLinerange <- function(widget, layerGeom, data, ggplotPanel_params){
  n <- dim(data)[1]
  data$xend <- NA
  data$yend <- NA
  for (i in 1:n){
    data[i, ]$xend <- data[i, ]$x
    data[i, ]$y <- data[i, ]$ymin
    data[i, ]$yend <- data[i, ]$ymax
  }
  loonLayer.GeomSegment(widget, layerGeom, data, ggplotPanel_params)
}

loonLayer.GeomPointrange <- function(widget, layerGeom, data, ggplotPanel_params){
  loonLayer.GeomLinerange(widget, layerGeom, data, ggplotPanel_params)
  loonLayer.GeomPoint(widget, layerGeom, data, ggplotPanel_params)
}

loonLayer.GeomCrossbar <- function(widget, layerGeom, data, ggplotPanel_params){
  loonLayer.GeomRect(widget, layerGeom, data, ggplotPanel_params)
  n <- dim(data)[1]
  data$size <- 3 * data$size
  data$xend <- NA
  data$yend <- NA
  for (i in 1:n){
    data[i, ]$x <- data[i, ]$xmin
    data[i, ]$xend <- data[i, ]$xmax
    data[i, ]$yend <- data[i, ]$y
  }
  loonLayer.GeomSegment(widget, layerGeom, data, ggplotPanel_params)
}

loonLayer.GeomErrorbar <- function(widget, layerGeom, data, ggplotPanel_params){
  loonLayer.GeomLinerange(widget, layerGeom, data, ggplotPanel_params)
  n <- dim(data)[1]
  newdata <- data[rep(1:n, each = 2), ]
  newdata$xend <- NA
  newdata$yend <- NA
  for(i in 1: (2*n) ){
    newdata[i,]$x <- newdata[i,]$xmin
    newdata[i,]$xend <- newdata[i,]$xmax
    if(i %% 2 != 0){
      newdata[i,]$y <- newdata[i,]$yend <- newdata[i,]$ymin
    } else {
      newdata[i,]$y <- newdata[i,]$yend <- newdata[i,]$ymax
    }
  }
  loonLayer.GeomSegment(widget, layerGeom, newdata, ggplotPanel_params)
}

loonLayer.GeomErrorbarh <- function(widget, layerGeom, data, ggplotPanel_params){
  n <- dim(data)[1]
  newdata <- data[rep(1:n, each = 3), ]
  newdata$xend <- NA
  newdata$yend <- NA
  for(i in 1: (3*n) ){
    if(i %% 3 == 0){
      newdata[i,]$x <- newdata[i,]$xend <- newdata[i,]$xmin
      newdata[i,]$y <- newdata[i,]$ymin
      newdata[i,]$yend <- newdata[i,]$ymax
    } else if(i %% 3 == 1){
      newdata[i,]$yend <- newdata[i,]$y
      newdata[i,]$x <- newdata[i,]$xmin
      newdata[i,]$xend <- newdata[i,]$xmax
    } else {
      newdata[i,]$x <- newdata[i,]$xend <- newdata[i,]$xmax
      newdata[i,]$y <- newdata[i,]$ymin
      newdata[i,]$yend <- newdata[i,]$ymax
    }
  }
  loonLayer.GeomSegment(widget, layerGeom, newdata, ggplotPanel_params)
}

loonLayer.GeomPath <- function(widget, layerGeom, data, ggplotPanel_params){
  # lines group
  linesGroup <- l_layer_group(widget, "Lines")
  
  uniGroup <- unique(data$group)
  lapply(1:length(uniGroup), function(i){
    groupData <- data[data$group == uniGroup[i], ]
    linesColor <- hex6to12(groupData$colour)
    len_uni_col <- length(unique(groupData$colour))
    
    linesWidth <- as_loon_size(groupData$size, "lines")
    linesDash <- as_loon_dash(groupData$linetype)
    # a single line with a single color
    if(len_uni_col == 1){
      l_layer_line(
        widget, x = groupData$x, y = groupData$y, linewidth = linesWidth[1],
        color = linesColor[1], dash = linesDash[[1]], parent = linesGroup
      )
    } else {  # a line with different colors(gradual colors)
      n <- dim(groupData)[1]
      len <- ceiling( 1000/(n-1) )
      for( j in 1: (n - 1) ){
        new <- groupData[rep(j,len), ]
        new$x <- seq( groupData[j,]$x, groupData[j+1,]$x, length.out = len)
        new$y <- seq( groupData[j,]$y, groupData[j+1,]$y, length.out = len)
        if(j == 1) newdata <- new else newdata <- rbind(newdata, new)
      }
      loonLayer.GeomPoint(widget, layerGeom, newdata, ggplotPanel_params)
    }
  })
}

loonLayer.GeomRibbon <- function(widget, layerGeom, data, ggplotPanel_params){
  uniGroup <- unique(data$group)
  newdata <- do.call(rbind, lapply(1:length(uniGroup), function(i){
    d <- data[data$group == uniGroup[i], ]
    nd <- rbind(d, d)
    nd$x <- c(d$x, rev(d$x) )
    nd$y <- c(d$ymin, rev(d$ymax) )
    nd
  }))
  loonLayer.GeomPolygon(widget, layerGeom, newdata, ggplotPanel_params)
}

loonLayer.GeomSmooth <- function(widget, layerGeom, data, ggplotPanel_params){
  
  if(!is.null(data$se)){
    # the only difference bewteen loonLayer.GeomRibbon is the polygon border colour. NA is set here
    newdata <- rbind(data, data)
    newdata$x <- c(data$x, rev(data$x) )
    newdata$y <- c(data$ymin, rev(data$ymax) )
    newdata$colour <- NA
    loonLayer.GeomPolygon(widget, layerGeom, newdata, ggplotPanel_params)
  }
  loonLayer.GeomPath(widget, layerGeom, data, ggplotPanel_params)
}

loonLayer.GeomStep <- function(widget, layerGeom, data, ggplotPanel_params){
  
  direction <- layerGeom$geom_params$direction
  uniGroup <- unique(data$group)
  newdata <- do.call(rbind, lapply(1:length(uniGroup), function(i){
    d <- data[data$group == uniGroup[i], ]
    # n must be larger than 2 (line)
    n <- dim(d)[1]
    # stepNewAddedMatrix is the new matrix which would be added on d
    stepNewAddedMatrix <- d[-n, ]
    newOrder <- c(1:n, (1:(n-1) + 0.5))
    if(direction == "hv"){
      for(j in 1: (n-1)){
        stepNewAddedMatrix[j, ]$x <- d[j+1, ]$x
        stepNewAddedMatrix[j, ]$y <- d[j, ]$y
      }
    }else{
      for(j in 1: (n-1)){
        stepNewAddedMatrix[j, ]$x <- d[j, ]$x
        stepNewAddedMatrix[j, ]$y <- d[j+1, ]$y
      }
    }
    rbind(d, stepNewAddedMatrix)[order(newOrder) ,]
  }))
  
  loonLayer.GeomPath(widget, layerGeom, newdata, ggplotPanel_params)
}

loonLayer.GeomRaster <- function(widget, layerGeom, data, ggplotPanel_params){
  loonLayer.GeomRect(widget, layerGeom, data, ggplotPanel_params)
}
########################################### helper function ###########################################


hex6to12 <- function(col){
  if(is.null(col) ) {""} else{
    num <- length(col)
    sapply(1:num, function(i){
      if(is.na(col[i]) | col[i] == "NA") ""
      else{
        # ARGB is 8 digits, with last two representing transparency. 
        # We have to erase last two digits (TK color codes do not include transparency information)
        splitCol <- unlist(strsplit(col[i], split = ""))
        if("#" %in% splitCol & length(splitCol) > 7 ) l_hexcolor(paste(splitCol[1:7], collapse = "")) 
        else if("#" %in% splitCol & length(splitCol) < 7) ""
        else l_hexcolor(col[i]) 
      } 
    }
    ) 
  }
}

pch_to_glyph <- function(pch, alpha) {
  len <- length(pch)
  
  switchPch <- function(pch){
    switch(
      as.character(pch),
      "16" = "circle" ,
      "1" = "ocircle",
      "21" = "ccircle",
      "15" = "square",
      "0" = "osquare",
      "22" = "csquare",
      "17" = "triangle",
      "2" = "otriangle",
      "24" = "ctriangle",
      "18" = "diamond",
      "5" = "odiamond",
      "23" = "cdiamond",
      {
        # warning("pch type ", glyph, " will be mapped to circle")
        "circle"
      }
    )
  }
  
  vapply(1:len, function(i) {
    if(is.na(alpha[i])){
      switchPch(pch[i]) 
    } else {
      if(alpha[i] < 0.5 ){
        switch(
          as.character( pch[i] ),
          "16" = "ocircle" ,
          "1" = "ocircle",
          "21" = "ocircle",
          "15" = "osquare",
          "0" = "osquare",
          "22" = "osquare",
          "17" = "otriangle",
          "2" = "otriangle",
          "24" = "otriangle",
          "18" = "odiamond",
          "5" = "odiamond",
          "23" = "odiamond",
          {
            # warning("pch type ", glyph, " will be mapped to circle")
            "ocircle"
          }
        )
      } else {
        switchPch(pch[i]) 
      }
    }
  }, character(1))
}



as_loon_size <- function(s, type) {
  if(is.null(s) ) 1 else
    switch(type, 
           "points" = ceiling( s^2), 
           "lines" = 2 * s, 
           "texts" = ceiling(s^2 / 1.5), 
           {
             # unspecified type
             "" 
           }
    )
}

as_loon_hvjust <- function(hjust, vjust) {
  if(length(hjust) != length(vjust) ) NULL
  else {
    len <- length(hjust)
    sapply(1:len, function(i){
      if(hjust[i] == 0.5 & vjust[i] == 0.5) "center" 
      else if(hjust[i] == 0.5 & vjust[i] > 0.5) "n"
      else if(hjust[i] > 0.5 & vjust[i] > 0.5) "ne"
      else if(hjust[i] > 0.5 & vjust[i] == 0.5) "e"
      else if(hjust[i] > 0.5 & vjust[i] < 0.5) "se"
      else if(hjust[i] == 0.5 & vjust[i] < 0.5) "s"
      else if(hjust[i] < 0.5 & vjust[i] < 0.5) "sw"
      else if(hjust[i] < 0.5 & vjust[i] == 0.5) "w"
      else if(hjust[i] < 0.5 & vjust[i] > 0.5) "nw"
    })
  }
}

as_loon_dash <- function(linetype){
  
  lapply(linetype, function(l){
    if(l == 1 | is.na(l) ) "" 
    else if(l == 2) rep(1, 4)
    else if(l == 3) rep(1,2)
    else rep(1, length(l))
  })
  
}

ggBuild2Loon <- function(ggplotObject){
  len_layers <- length(ggplotObject$layers)
  ggBuild <- ggplot2::ggplot_build(ggplotObject)
  # check data.frame
  input <- ggplotObject$data
  if(!is.data.frame(input)) input <- as.data.frame(input)
  # itemLabel
  label <- row.names(input)
  if(len_layers != 0){
    layerNames <- sapply(1:len_layers, function(j) {
      className <- class(ggplotObject$layers[[j]]$geom) 
      className[-which(className %in% c("ggproto"  ,"gg" ,"Geom"))]
    })
    pointsLayerId <- which(sapply(layerNames, function(l){"GeomPoint" %in% l}) == TRUE)
    if( length(pointsLayerId) != 0 ){
      mapX <- as.character(ggplotObject$mapping$x[2])
      mapY <- as.character(ggplotObject$mapping$y[2])
      if(mapX %in% colnames(input)) xx <- input[,   mapX] 
      else {
        str <- strsplit(as.character(ggplotObject$mapping$x[2]), "[(]")[[1]]
        xx <- input[,   strsplit(str[length(str)], "[)]")[[1]][1] ] 
      } 
      if(mapY %in% colnames(input)) yy <- input[,   mapY] 
      else {
        str <- strsplit(as.character(ggplotObject$mapping$y[2]), "[(]")[[1]]
        yy <- input[,   strsplit(str[length(str)], "[)]")[[1]][1] ] 
      }
      input_xy <- data.frame(x = xx , 
                             y = yy)
      for(i in 1:length(pointsLayerId)){
        buildData  <- ggBuild$data[[pointsLayerId[i]]]
        itemLabel <- c()
        for(j in 1:dim(buildData)[1]){
          id <- which( input_xy$x %in% buildData[j,]$x== T & input_xy$y %in% buildData[j,]$y == T)[1]
          if(is.na(id)){
            itemLabel[j] <- NA
          } else {
            itemLabel[j] <- label[id]
            input_xy[id,]$x <- NA
            input_xy[id,]$y <- NA
          }
        }
        ggBuild$data[[pointsLayerId[i]]]$label <- itemLabel
      }
    }
  }
  ggBuild
} 
