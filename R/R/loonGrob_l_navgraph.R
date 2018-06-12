#' @export
#' 


loonGrob.l_navgraph <- function(widget, navigator_add = NULL){
  #### graphGrob
  graph <-  widget$graph
  graphGrob <- loonGrob(graph)
  
  #### navigatorGrob
  navigator <- widget$navigator
  
  if(is.null(navigator_add)) {
    gTree(children = gList(
      graphGrob,
      navigatorLinesScatterGrob(graph, navigator),
      navigatorPointsGrob(graph, navigator)
    ))
  } else{
    gTree(
      children = gList(
        graphGrob,
        navigatorLinesScatterGrob(graph, navigator),
        do.call(gList, 
                lapply(navigator_add, function(l){
                  navigatorLinesScatterGrob(graph, l)
                })
        ),
        navigatorPointsGrob(graph, navigator),
        do.call(gList, 
                lapply(navigator_add, function(l){
                  navigatorPointsGrob(graph, l)
                })
        )
      )
    )
  }
  
}

navigatorLinesScatterGrob <- function(graph, navigator){
  "l_navigator" %in% class(navigator) || stop("navigator does not seem to exist") 
  
  xlim <- c(graph['panX'], graph['panX'] + graph['deltaX']/graph['zoomX'])
  ylim <- c(graph['panY'], graph['panY'] + graph['deltaY']/graph['zoomY'])
  showScales <- graph['showScales']
  title <- graph['title']
  margins <- if (showScales) c(4.1, 5.1, 2.1, 2.1) else rep(2.1, 4)
  if (title != "" ) margins[3] <- 3.1
  
  vp <- vpStack(
    plotViewport(margins = margins),
    dataViewport(xscale = xlim, yscale = ylim)
  )
  
  x <- if(length(graph['xTemp']) == 0) graph['x'] else graph['xTemp']
  y <- if(length(graph['yTemp']) == 0) graph['y'] else graph['yTemp']
  node <- graph['nodes']
  
  color <- as_hex6color(navigator['color'])
  from <- navigator['from']
  to <- navigator['to']
  prop <- navigator['proportion']
  
  fromId <- sapply(1:length(from), function(i){which(node %in% from[i] == T)})
  toId <- sapply(1:length(to), function(i){which(node %in% to[i] == T)})
  
  if(length(from) == 0) NULL else if(length(from) == 1 & length(to) == 0) NULL 
  else {
    fromLinesGrob <- if(length(from) < 2) NULL else{
      do.call(gList,
              lapply(1:( length(from) - 1 ), function(i){
                linesGrob(unit( c(x[fromId[i]], x[fromId[i+1]]), "native"), 
                          unit( c(y[fromId[i]], y[fromId[i+1]]), "native"),
                          gp = gpar(col = NA, col = color, 
                                    lwd = 9), 
                          vp = vp) }
              )
      )
    }
    
    toLinesGrob <-  if(length(to) < 2) NULL else{
      do.call(gList, 
              lapply(1:( length(to) - 1 ), function(i){
                linesGrob(unit( c(x[toId[i]], x[toId[i+1]]), "native"), 
                          unit( c(y[toId[i]], y[toId[i+1]]), "native"),
                          gp = gpar(col = NA, col = color, 
                                    lwd = 3),
                          vp = vp) }
              )
      )
    }
    
    xTemp <- (1 - prop) * x[fromId[length(fromId)]] + prop * x[toId[1]]
    yTemp <- (1 - prop) * y[fromId[length(fromId)]] + prop * y[toId[1]]
    
    fromtoLinesGrob <- gList(linesGrob(unit(c(x[fromId[length(fromId)]], xTemp), "native"),
                                       unit(c(y[fromId[length(fromId)]], yTemp), "native"),
                                       gp = gpar(col = NA, col = color, lwd = 9),
                                       vp = vp ), 
                             linesGrob(unit(c(xTemp, x[toId[1]]), "native"),
                                       unit(c(yTemp, y[toId[1]]), "native"),
                                       gp = gpar(col = NA, col = color, lwd = 3),
                                       vp = vp )
    )
    states <- get_layer_states(target = graph, 
                               widget = graph, 
                               layerid = "root", 
                               type = l_layer_getType(graph, "model"))
    gList(
      fromLinesGrob,
      toLinesGrob,
      fromtoLinesGrob,
      scatterGrob(states, vp = vp)
    )
  }
}


navigatorPointsGrob <- function(graph, navigator){
  
  "l_navigator" %in% class(navigator) || stop("navigator does not seem to exist") 
  
  xlim <- c(graph['panX'], graph['panX'] + graph['deltaX']/graph['zoomX'])
  ylim <- c(graph['panY'], graph['panY'] + graph['deltaY']/graph['zoomY'])
  showScales <- graph['showScales']
  title <- graph['title']
  margins <- if (showScales) c(4.1, 5.1, 2.1, 2.1) else rep(2.1, 4)
  if (title != "" ) margins[3] <- 3.1
  
  vp <- vpStack(
    plotViewport(margins = margins),
    dataViewport(xscale = xlim, yscale = ylim)
  )
  
  x <- if(length(graph['xTemp']) == 0) graph['x'] else graph['xTemp']
  y <- if(length(graph['yTemp']) == 0) graph['y'] else graph['yTemp']
  node <- graph['nodes']
  
  color <- as_hex6color(navigator['color'])
  from <- navigator['from']
  to <- navigator['to']
  prop <- navigator['proportion']
  
  fromId <- sapply(1:length(from), function(i){which(node %in% from[i] == T)})
  toId <- sapply(1:length(to), function(i){which(node %in% to[i] == T)})
  
  if(length(from) == 0){
    pointsGrob(x = unit(0.1, "npc"), 
               y = unit(0.9, "npc"), 
               gp = gpar(fill = color, cex = 3.5),  
               pch = 21,
               vp = vp
    )
    
  } else if(length(from) == 1 & length(to) == 0) {
    pointsGrob(x = unit(x[fromId], "native"), 
               y = unit(y[fromId], "native"), 
               gp = gpar(fill = color, cex = 3.5),  
               pch = 21,
               vp = vp
    )
  } else {
    xTemp <- (1 - prop) * x[fromId[length(fromId)]] + prop * x[toId[1]]
    yTemp <- (1 - prop) * y[fromId[length(fromId)]] + prop * y[toId[1]]
    
    gList(
      pointsGrob(unit(xTemp, "native"), unit( yTemp, "native"),
                 gp = gpar(fill = color, cex = 3.5),pch = 21, 
                 vp = vp
      ),
      pointsGrob(unit(x[toId[length(toId)]], "native"), 
                 unit(y[toId[length(toId)]], "native"), 
                 gp = gpar(fill = color, cex = 0.8), pch = 21,
                 vp = vp
      )
    )
  }
}
