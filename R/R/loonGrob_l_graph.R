#' Create a graph grob
#' 
#' @param widget \code{l_graph} object of class inheriting from "l_graph"
#' @param navigators navigator objects, can be a single navigator or a list of navigators
#' 
#' @return a grid grob
#' 
#' @import grid
#' 
#' @export
#' 
#' @examples 
#' G <- completegraph(names(iris[,-5]))
#' LG <- linegraph(G)
#' g <- l_graph(LG)
#' nav0 <- l_navigator_add(g)
#' l_configure(nav0, label = 0)
#' con0 <- l_context_add_geodesic2d(navigator=nav0, data=iris[,-5])
#' nav1 <- l_navigator_add(g, from = "Sepal.Length:Petal.Width", 
#'   to = "Petal.Length:Petal.Width", proportion = 0.3)
#' l_configure(nav1, label = 1)
#' con1 <- l_context_add_geodesic2d(navigator=nav1, data=iris[,-5])
#' nav2 <- l_navigator_add(g, from = "Sepal.Length:Petal.Length", 
#'   to = "Sepal.Width:Petal.Length", proportion = 0.5)
#' l_configure(nav2, label = 2)
#' con2 <- l_context_add_geodesic2d(navigator=nav2, data=iris[,-5])
#' 
#' library(grid)
#' lgrob <- loonGrob(g, navigators =  list(nav0, nav1, nav2))
#' grid.newpage(); grid.draw(lgrob)

loonGrob.l_graph <- function(widget, navigators = NULL){
  
  graphGrob <- loonGrob.default(widget)
  if(is.null(navigators)){
    graphGrob
  }else{
    if(is.list(navigators)){
      gTree(
        children = gList(
          graphGrob,
          do.call(gList, 
                  lapply(navigators, function(l){
                    navigatorLinesScatterGrob(widget, l)
                  })
          ),
          do.call(gList, 
                  lapply(navigators, function(l){
                    navigatorPointsGrob(widget, l)
                  })
          )
        )
      )
    }else{
      gTree(children = gList(
        graphGrob,
        navigatorLinesScatterGrob(widget, navigators),
        navigatorPointsGrob(widget, navigators)
      ))
    }
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
    states <- get_layer_states(target = graph)
    gList(
      fromLinesGrob,
      toLinesGrob,
      fromtoLinesGrob,
      scatterGrob(states, vp = vp)
    )
  }
}

# size of navigator is arbitrary, which is as close as loon object.

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
  label <- navigator['label']
  
  fromId <- sapply(1:length(from), function(i){which(node %in% from[i] == T)})
  toId <- sapply(1:length(to), function(i){which(node %in% to[i] == T)})
  
  if(length(from) == 0){
    
    xx <- unit(0.1, "npc") 
    yy <- unit(0.9, "npc")
    gList(
      pointsGrob(xx, yy, 
                 gp = gpar(fill = color, cex = 3.5),  
                 pch = 21,
                 vp = vp
      ),
      if(length(label) != 0) {
        textGrob(paste(label, collapse = " "), xx, yy, 
                 gp = gpar(fill = "black", fontsize = 9),  
                 vp = vp)
      } else NULL
    )

    
  } else if(length(from) == 1 & length(to) == 0) {
    
    xx <- unit(x[fromId], "native")
    yy <- unit(y[fromId], "native")
    gList(
      pointsGrob(x = xx, 
                 y = yy, 
                 gp = gpar(fill = color, cex = 3.5),  
                 pch = 21,
                 vp = vp
      ),       
      if(length(label) != 0) {
        textGrob(paste(label, collapse = " "), xx, yy, 
                 gp = gpar(fill = "black", fontsize = 9),  
                 vp = vp)
      } else NULL
      
    )
  } else {
    xx <- unit( (1 - prop) * x[fromId[length(fromId)]] + prop * x[toId[1]], "native")
    yy <- unit( (1 - prop) * y[fromId[length(fromId)]] + prop * y[toId[1]], "native")
    
    gList(
      # 'to' dot
      pointsGrob(unit(x[toId[length(toId)]], "native"), 
                 unit(y[toId[length(toId)]], "native"), 
                 gp = gpar(fill = color, cex = 0.8, col = NA), pch = 21,
                 vp = vp
      ),
      # 'from' navigator
      pointsGrob(xx, yy,
                 gp = gpar(fill = color, cex = 3.5),pch = 21, 
                 vp = vp
      ),
      # 'text' on the navigator
      if(length(label) != 0) {
        textGrob(paste(label, collapse = " "), xx, yy, 
                 gp = gpar(fill = "black", fontsize = 9),  
                 vp = vp)
      } else NULL
    )
  }
}
