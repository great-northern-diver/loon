
#' @export
loonGrob_layoutType.l_pairs <- function(target) {
    "arrangeGrobArgs"
}


#' @export
l_get_arrangeGrobArgs.l_pairs <- function(target) {

    nPlots <- length(target)
    nScatterplots <- nHistograms <- nSerialAxes <- 0
    scatterplots <- histograms <- serialAxes <- list()
    plotNames <- names(target)

    for(i in seq(nPlots)) {
        if(inherits(target[[i]], "l_plot")) {
            nScatterplots <- nScatterplots + 1
            scatterplots[[nScatterplots]] <- target[[i]]
            names(scatterplots)[nScatterplots] <- plotNames[i]
        }
        if(inherits(target[[i]], "l_hist")) {
            nHistograms <- nHistograms + 1
            histograms[[nHistograms]] <- target[[i]]
            names(histograms)[nHistograms] <- plotNames[i]
        }
        if(inherits(target[[i]], "l_serialaxes")) {
            nSerialAxes <- nSerialAxes + 1
            serialAxes[[nSerialAxes]] <- target[[i]]
            names(serialAxes)[nSerialAxes] <- plotNames[i]
        }
    }

    locations <- l_getLocations(target)
    layout_matrix <- locations$layout_matrix

    nvar <- (-1 + sqrt(1 + 8 * nScatterplots)) / 2 + 1
    showSerialAxes <- (nSerialAxes > 0)
    showHistograms <- (nHistograms > 0)

    if(showHistograms) {
        histLocation <- if(nHistograms == (nvar - 1) * 2) "edge" else "diag"
        showTexts <- if(histLocation == "edge") TRUE else FALSE
    } else showTexts <- TRUE

    scatter_hist <- c(scatterplots, histograms)
    scatter_histGrobs <- lapply(seq(nScatterplots + nHistograms),
                                function(i){
                                    pi <- scatter_hist[[i]]
                                    pi['minimumMargins'] <- rep(2,4)
                                    if(i <= nScatterplots) {
                                        loonGrob(pi, name = paste("scatterplot", i))
                                    } else {
                                        loonGrob(pi, name = paste("histogram", i - nScatterplots))
                                    }
                                }
    )

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

        textGrobs <- lapply(seq(length(texts)),
                            function(i) {
                                textGrob(texts[i], gp = gpar(fontsize = 9), name = paste("text", i))
                            }
        )

        max_value <- max(layout_matrix, na.rm = TRUE)
        text_index <- (max_value + 1): (max_value + length(texts))
        if(!showHistograms) {

            diag(layout_matrix) <- text_index
        } else {
            if(histLocation == "diag") {

                diag(layout_matrix) <- text_index
            } else {

                lapply(1:length(text_index),
                       function(i){
                           layout_matrix[i + 1, i] <<- text_index[i]
                       }
                )
            }
        }

        locations$layout_matrix <- layout_matrix
    }

    if(showTexts) {
        grobs <- setNames(c(scatter_histGrobs, serialAxesGrob, textGrobs),
                          c(as.character(scatter_hist), as.character(serialAxes),
                            texts))

    } else {
        grobs <- setNames(c(scatter_histGrobs, serialAxesGrob),
                          c(as.character(scatter_hist), as.character(serialAxes)))
    }



    c(
        locations,
        list(
            grobs = grobs,
            name = "l_pairs"
        )
    )
}

#' @export
l_createCompoundGrob.l_pairs <- function(target, arrangeGrob.args){
    backgroundCol <- l_getOption("canvas_bg_guides")
    grobTree(
        rectGrob(gp  = gpar(fill = backgroundCol, col = NA),
                 name = "bounding box"),
        do.call(gridExtra::arrangeGrob,  arrangeGrob.args),
        name = "pairs"
    )
}

