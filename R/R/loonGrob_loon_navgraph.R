#' @export
#' 


loonGrob.loon_navgraph <- function(widget){
  
  # scale and margins for setting viewport
  xlim <- c(widget$graph['panX'], widget$graph['panX'] + widget$graph['deltaX']/widget$graph['zoomX'])
  ylim <- c(widget$graph['panY'], widget$graph['panY'] + widget$graph['deltaY']/widget$graph['zoomY'])
  
  title <- widget$graph['title']
  showScales <- widget$graph['showScales']
  margins <- if (showScales) c(4.1, 5.1, 2.1, 2.1) else rep(2.1, 4)
  if (title != "" ) margins[3] <- 3.1
  
  vp <- vpStack(
    plotViewport(margins = margins),
    dataViewport(xscale = xlim, yscale = ylim)
  )
  
  graphGrob <- loonGrob(widget$graph)
  plotGrob <- loonGrob(widget$plot)
  
  navigator <- widget$navigator
  x <- widget$graph['x']
  y <- widget$graph['y']
  node <- widget$graph['nodes']
  color <- as_hex6color(navigator['color'])
  from <- navigator['from']
  to <- navigator['to']
  prop <- navigator['proportion']
  
  fromId <- sapply(1:length(from), function(i){which(node %in% from[i] == T)})
  toId <- sapply(1:length(to), function(i){which(node %in% to[i] == T)})
  
  if(length(from) == 1 & length(to) == 0) {
    navigatorGrob <- gTree(children = gList(
      graphGrob,
      pointsGrob(x = unit(x[fromId], "native"), 
                 y = unit(y[fromId], "native"), 
                 gp = gpar(fill = color, cex = 3.5),  
                 pch = 21,
                 vp = vp
      )
    ), name = "navigatorGrob")
  } else {
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
    
    states <- get_layer_states(target = widget$graph, 
                               widget = widget$graph, 
                               layerid = "root", 
                               type = l_layer_getType(widget$graph, "model"))
    
    navigatorGrob <- gTree(children = gList(
      graphGrob,
      fromLinesGrob,
      toLinesGrob,
      fromtoLinesGrob,
      scatterGrob(states, vp = vp),
      pointsGrob(unit(xTemp, "native"), unit( yTemp, "native"),
                 gp = gpar(fill = color, cex = 3.5),pch = 21, 
                 vp = vp
      ),
      pointsGrob(unit(x[toId[length(toId)]], "native"), 
                 unit(y[toId[length(toId)]], "native"), 
                 gp = gpar(fill = color, cex = 0.8), pch = 21,
                 vp = vp
      )
    ), name = "navigatorGrob")
  }
  
  gridExtra::arrangeGrob(grobs = gList(navigatorGrob, plotGrob), ncol = 2, name = "loon_navigator")
}
