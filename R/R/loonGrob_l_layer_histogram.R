#' @export
loonGrob.l_layer_histogram <- function(target, layerid, states) {
    active <- states$active
    swapAxes <- target['swapAxes']
    activeX <- if (swapAxes) as.numeric( states$y )[active]  else as.numeric( states$x )[active] 
    if(l_layer_isVisible(target, layerid) & length(activeX)!=0  ){
        
        n <- length(activeX) 
        activeSelected <- states$selected[active]
        activeColor <- states$color[active]
        sel_color <- as.character(.Tcl("set loon::Options(select-color)"))
        binId <- list()
        minActivex <- min(activeX)
        maxActivex <- max(activeX)
        i <- 1
        binwidth <- states$binwidth
        binX <- c()
        while(minActivex + (i-1) * binwidth <= maxActivex){
            left <- minActivex + (i - 1) * binwidth
            right <- minActivex + i * binwidth
            binId[[i]] <- which( (activeX <right & activeX >= left ) == TRUE)
            binX <- c(binX, left, right)
            i <- i + 1
        } 
        
        binHeight <- if(states$yshows == "frequency") {sapply(binId, "length") 
        }else {sapply(binId, "length") / (n * binwidth)}
        
        
        binX <- unique(binX)
        
        gTree(children = do.call (
            gList,
            lapply(1:length(binHeight) , function(i){
                if(binHeight[i] != 0){
                    if(!swapAxes){
                        x <- unit(mean(c(binX[i], binX[i + 1])), "native")
                        y <- unit(mean(c(0, binHeight[i])), "native")
                        
                        width <- unit(binwidth, "native")
                        height <- unit(binHeight[i], "native")
                    }else{
                        y <- unit(mean(c(binX[i], binX[i + 1])), "native")
                        x <- unit(mean(c(0, binHeight[i])), "native")
                        
                        height <- unit(binwidth, "native")
                        width <- unit(binHeight[i], "native")
                    }
                    
                    isSelected  <- activeSelected[binId[[i]]]
                    
                    if(states$showStackedColors){
                        
                        if(any(isSelected)){
                            binSelected <- length(which(isSelected == TRUE))
                            oldColorBinHeight <- table(activeColor[binId[[i]]][which(isSelected == FALSE)])
                            colorBinHeight <- if(states$yshows == "frequency") {c(binSelected, oldColorBinHeight)
                            }else{c(binSelected, oldColorBinHeight) / (n * binwidth)}
                            names(colorBinHeight) <- c(sel_color, names(oldColorBinHeight))
                        }else{colorBinHeight <- if(states$yshows == "frequency") { table(activeColor[binId[[i]]])
                        }else{table(activeColor[binId[[i]]]) / (n * binwidth)} }
                        
                        cumsumColorBinHeight <- c(0, cumsum(colorBinHeight))
                        do.call(gList, 
                                lapply(1:length(colorBinHeight), function(i){
                                    if(!swapAxes){
                                        y <- unit(mean(c(cumsumColorBinHeight[i], 
                                                         cumsumColorBinHeight[i+1])), "native")
                                        height <- unit(colorBinHeight[i], "native")
                                    }else{
                                        x <- unit(mean(c(cumsumColorBinHeight[i], 
                                                         cumsumColorBinHeight[i+1])), "native")
                                        width <- unit(colorBinHeight[i], "native")
                                    }
                                    rectGrob(
                                        x = x, y = y, width = width, height = height, 
                                        gp = gpar(fill = names(colorBinHeight)[i], 
                                                  col = if(states$showOutlines) states$colorOutline else NA)) 
                                }))
                    }else{
                        rectGrobObject <- rectGrob(
                            x = x, y = y, width = width, height = height, 
                            gp = gpar(fill = states$colorFill, 
                                      col = if(states$showOutlines) states$colorOutline else NA))
                        if(any(isSelected)){
                            binSelected <- if(states$yshows == "frequency") {length(which(isSelected == TRUE))
                            }else{length(which(isSelected == TRUE)) / (n * binwidth)}
                            if(!swapAxes){
                                y <- unit(mean(c(0, binSelected)), "native")
                                height <- unit(binSelected, "native") 
                            }else{
                                x <- unit(mean(c(0, binSelected)), "native")
                                width<- unit(binSelected, "native")
                            }
                            
                            gList(
                                rectGrobObject,
                                rectGrob(
                                    x = x, y = y, width = width, height = height, 
                                    gp = gpar(fill = sel_color, 
                                              col = if(states$showOutlines) states$colorOutline else NA))
                            )
                        }else{rectGrobObject}
                    }
                }
            }))
        )
    } else NULL 
}