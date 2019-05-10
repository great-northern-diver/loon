
#' @export
loonGrob_layoutType.l_pairs <- function(target) {
    "arrangeGrobArgs"
}


#' @export
l_get_arrangeGrobArgs.l_pairs <- function(pairsplot) {
    nPlots <- length(pairsplot)
    nScatterplots <- nHistograms <- nSerialAxes <- 0
    scatterplots <- histograms <- serialAxes <- list()
    plotNames <- names(pairsplot)
    for(i in 1:nPlots) {
        if("l_plot" %in% class(pairsplot[[i]])) {
            nScatterplots <- nScatterplots + 1
            scatterplots[[nScatterplots]] <- pairsplot[[i]]
            names(scatterplots)[nScatterplots] <- plotNames[i]
        }
        if("l_hist" %in% class(pairsplot[[i]])) {
            nHistograms <- nHistograms + 1
            histograms[[nHistograms]] <- pairsplot[[i]]
            names(histograms)[nHistograms] <- plotNames[i]
        }
        if("l_serialaxes" %in% class(pairsplot[[i]])) {
            nSerialAxes <- nSerialAxes + 1
            serialAxes[[nSerialAxes]] <- pairsplot[[i]]
            names(serialAxes)[nSerialAxes] <- plotNames[i]
        }
    }

    nvar <- (-1 + sqrt(1 + 8 * nScatterplots)) / 2 + 1
    showSerialAxes <- (nSerialAxes > 0)
    showHistograms <- (nHistograms > 0)
    if(showHistograms) {
        histLocation <- if(nHistograms == (nvar - 1) * 2) "edge" else "diag"
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

    scatter_histGrobs <- lapply(1:(nScatterplots + nHistograms),
                                function(i){
                                    pi <- scatter_hist[[i]]
                                    pi['foreground'] <- "white"
                                    pi['minimumMargins'] <- rep(2,4)
                                    if(i <= nScatterplots) {
                                        loonGrob(pi, name = paste("scatterplot", i))
                                    } else {
                                        loonGrob(pi, name = paste("histogram", i - nScatterplots))
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
        nTexts <- length(texts)
        textGrobs <- lapply(texts,
                            function(t) {
                                textGrob(t, gp = gpar(fontsize = 9), name = paste("text", i))
                            }
        )

        if(cells == nvar) {
            for(i in 1:nTexts) {
                layout_matrix[i, i] <- (nScatterplots + nHistograms) + i
            }
        } else {
            for(i in 1:nTexts) {
                layout_matrix[(i + 1), i] <- (nScatterplots + nHistograms) + i
            }
        }
    } else  nTexts <- 0

    serialAxesSpan <- floor(nvar/2)
    # square space
    for(i in 1:serialAxesSpan) {
        for(j in 1:serialAxesSpan) {
            layout_matrix[cells - serialAxesSpan + i, j] <- nScatterplots + nHistograms + nTexts + 1
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

