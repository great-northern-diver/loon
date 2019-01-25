
#' @export
loonGrob_layoutType.l_pairs <- function(target) {
    "arrangeGrobArgs"
}


#' @export
l_get_arrangeGrobArgs.l_pairs <- function(target) {
    widget <- target
    len_widget <- length(widget)
    numofScatterplots <- numofHistograms <- numofSerialAxes <- 0
    scatterplots <- histograms <- serialAxes <- list()
    namesWidget <- names(widget)
    for(i in 1:len_widget) {
        if("l_plot" %in% class(widget[[i]])) {
            numofScatterplots <- numofScatterplots + 1
            scatterplots[[numofScatterplots]] <- widget[[i]]
            names(scatterplots)[numofScatterplots] <- namesWidget[i]
        }
        if("l_hist" %in% class(widget[[i]])) {
            numofHistograms <- numofHistograms + 1
            histograms[[numofHistograms]] <- widget[[i]]
            names(histograms)[numofHistograms] <- namesWidget[i]
        }
        if("l_serialaxes" %in% class(widget[[i]])) {
            numofSerialAxes <- numofSerialAxes + 1
            serialAxes[[numofSerialAxes]] <- widget[[i]]
            names(serialAxes)[numofSerialAxes] <- namesWidget[i]
        }
    }

    nvar <- (-1 + sqrt(1 + 8 * numofScatterplots)) / 2 + 1
    showSerialAxes <- (numofSerialAxes > 0)
    showHistograms <- (numofHistograms > 0)
    if(showHistograms) {
        histLocation <- if(numofHistograms == (nvar - 1) * 2) "edge" else "diag"
        if(histLocation == "edge") {
            cells <- nvar + 1
            showTexts <- TRUE
        } else {
            cells <- nvar
            showTexts <- FALSE
        }
    } else {
        cells <- nvar
        showTexts <- TRUE
    }

    layout_matrix <- matrix(rep(NA, (cells)^2), nrow = cells)
    scatter_hist <- c(scatterplots, histograms)

    for(i in 1:length(scatter_hist)) {
        nameOfScatter_hist <- names(scatter_hist[i])
        pos <- xy_layout(nameOfScatter_hist)
        layout_matrix[pos$y, pos$x] <- i
    }

    scatter_histGrobs <- lapply(1:(numofScatterplots + numofHistograms),
                                function(i){
                                    pi <- scatter_hist[[i]]
                                    pi['foreground'] <- "white"
                                    pi['minimumMargins'] <- rep(2,4)
                                    if(i <= numofScatterplots) {
                                        loonGrob(pi, name = paste("scatterplot", i))
                                    } else {
                                        loonGrob(pi, name = paste("histogram", i - numofScatterplots))
                                    }
                                }
    )

    if(showTexts) {
        texts <- unique(
            c(sapply(scatterplots,
                     function(s){
                         s['ylabel']
                     }),
              sapply(scatterplots,
                     function(s){
                         s['xlabel']
                     })
            )
        )
        numofTexts <- length(texts)
        textGrobs <- lapply(texts,
                            function(t) {
                                textGrob(t, gp = gpar(fontsize = 9), name = paste("text", i))
                            }
        )

        if(cells == nvar) {
            for(i in 1:numofTexts) {
                layout_matrix[i, i] <- (numofScatterplots + numofHistograms) + i
            }
        } else {
            for(i in 1:numofTexts) {
                layout_matrix[(i + 1), i] <- (numofScatterplots + numofHistograms) + i
            }
        }
    } else  numofTexts <- 0

    serialAxesSpan <- floor(nvar/2)
    # square space
    for(i in 1:serialAxesSpan) {
        for(j in 1:serialAxesSpan) {
            layout_matrix[cells - serialAxesSpan + i, j] <- numofScatterplots + numofHistograms + numofTexts + 1
        }
    }
    serialAxesGrob <- unname(
        lapply(serialAxes,
               function(s) {
                   condGrob(test = showSerialAxes,
                            grobFun = loonGrob,
                            name = "serialaxes",
                            target = s)
               }
        )
    )

    lgrobs <- if(showTexts) {
        c(scatter_histGrobs, textGrobs, serialAxesGrob)
    } else {
        c(scatter_histGrobs, serialAxesGrob)
    }

    list(
        grobs = lgrobs,
        layout_matrix = layout_matrix,
        name = "pairs"
    )
}

#' @export
l_createCompoundGrob.l_pairs <- function(target, arrangeGrob.args){
    backgroundCol <- "grey94"
    grobTree(
        rectGrob(gp  = gpar(fill = backgroundCol, col = NA),
                 name = "bounding box"),
        do.call(gridExtra::arrangeGrob,  arrangeGrob.args),
        name = "l_pairs"
    )
}

#' @export
l_getPlots.l_pairs <- function(target){
    target
}

#' @export
l_getLocations.l_pairs <- function(target){
    get_arrangeGrobArgs <- l_get_arrangeGrobArgs(target)
    get_arrangeGrobArgs$layout_matrix
}
