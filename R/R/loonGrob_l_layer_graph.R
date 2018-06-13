loonGrob.l_layer_graph  <- function(target, layerid, states) {
    active <- states$active
    if(l_layer_isVisible(target, layerid) && 
       length(states$x[active])!=0 && 
       length(states$y[active])!=0  ){
        activeNode <- states$nodes[active]
        activeX <- states$x[active]
        activeY <- states$y[active]
        isActiveEdge <- states$activeEdge
        linesTextsGrobObject <- do.call(
            gList, 
            lapply(1:length(activeNode) , function(i) {
                nodeFrom <- activeNode[i]
                nodeFrom_EdgeId <- which (states$from[isActiveEdge] == nodeFrom)
                if(length(nodeFrom_EdgeId) != 0){
                    nodeTo <- states$to[isActiveEdge][nodeFrom_EdgeId]
                    nodeTo_CoordId <- which (activeNode %in% nodeTo == TRUE)
                    len_nodeTo <- length(nodeTo_CoordId)
                    cols <- states$colorEdge[isActiveEdge][nodeFrom_EdgeId]
                    x <- unit( c( rep(activeX[i], len_nodeTo), activeX[nodeTo_CoordId] ), "native")
                    y <- unit( c( rep(activeY[i], len_nodeTo), activeY[nodeTo_CoordId] ), "native")
                    polylineGrobObject <- polylineGrob(x, y,
                                                       id=rep(1:len_nodeTo, 2),
                                                       gp=gpar(col= cols, lwd=1))
                }else{polylineGrobObject <- NULL}
                if(states$showOrbit){
                    activeAngle <- states$orbitAngle[active]
                    orbitDistance <- states$orbitDistance
                    textsGrobObject <- textGrob(label = activeNode[i], 
                                                x = unit(activeX[i], "native") + 
                                                    unit(orbitDistance * cos(activeAngle[i]),"mm" ), 
                                                y = unit(activeY[i], "native") + 
                                                    unit(orbitDistance * sin(activeAngle[i]),"mm" ), 
                                                gp=gpar(fontsize= 8, col= "black"))
                    gList(polylineGrobObject, 
                          textsGrobObject)
                }else{polylineGrobObject}
            }))
        states$col <- states$color
        scatterGrobObject <- scatterGrob(states)
        gTree(children = 
                  gList(
                      linesTextsGrobObject,
                      scatterGrobObject)
        )
    }
}