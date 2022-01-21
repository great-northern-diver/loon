#' @export
loonGrob_layoutType.l_facet_grid <- function(target) {
    "arrangeGrobArgs"
}


#' @export
l_get_arrangeGrobArgs.l_facet_grid <- function(target) {

    # find the parent tk window name
    parent <- as.character(tkwinfo("parent",  target[[1L]]))
    # access all children
    children <- as.character(tkwinfo("child",  parent))

    # a trick here
    # `xlabel` and `ylabel` both include text `label`
    tkColumnlabelPathNames <- children[grepl("columnlabel", children)]
    tkRowlabelPathNames <- children[grepl("rowlabel", children)]

    span <- 8L
    fontsize <- 10L
    labelcm <- 0.6

    # xlabel, ylabel and title
    xLabelPathName <- children[grepl("xlabel", children)]
    yLabelPathName <- children[grepl("ylabel", children)]
    titlePathName <- children[grepl("title", children)]

    labelLocation <- c("top", "right")

    if(any(grepl("bottom", tkColumnlabelPathNames))) {
        labelLocation[1L] <- "bottom"
    }

    if(any(grepl("left", tkRowlabelPathNames))) {
        labelLocation[2L] <- "left"
    }

    # plot layout matrix
    locations <- l_getLocations(target)
    nrow <- locations$nrow
    ncol <- locations$ncol

    bg <- as.character(tkcget(tkColumnlabelPathNames[1L], "-bg"))
    fg <- as.character(tkcget(tkColumnlabelPathNames[1L], "-fg"))

    # label layouts
    # initial settings
    grobs <- list()

    outputGrob <- gridExtra::arrangeGrob(
        grobs = lapply(target,
                       function(w) {
                           lg <- loonGrob(w)
                           lg$name <- as.character(w)
                           lg
                       }),
        layout_matrix = locations$layout_matrix,
        name = "plots"
    )

    # find the number of a string that right after the letter "extend"
    pat <- "^.*extend.*?([0-9]+)"

    extendCol <- as.numeric(gsub(pat, "\\1", tkColumnlabelPathNames))
    lenCol <- length(unique(extendCol))

    extendRow <- as.numeric(gsub(pat, "\\1", tkRowlabelPathNames))
    lenRow <- length(unique(extendRow))

    lenColRow <- max(c(lenCol, lenRow))
    spanAdj <- round(span /sqrt(lenColRow))

    # k is the index of each arranged widget
    k <- 1L
    outputLayoutMatrix <- matrix(rep(k, spanAdj * nrow * ncol * spanAdj),
                                   nrow = spanAdj * nrow)
    columnAdj <- 0

    if(length(tkColumnlabelPathNames) > 0) {

        # reorder the path names
        tkColumnlabelPathNames <- tkColumnlabelPathNames[order(extendCol,
                                                               decreasing = labelLocation[1L] == "top")]

        extendCol <- sort(extendCol, decreasing = labelLocation[1L] == "top")
        uniextend <- unique(extendCol)

        columnLabelMatrix <- matrix(NA, nrow = lenCol, ncol = ncol * spanAdj)

        for(i in seq(lenCol)) {

            extend <- uniextend[i]
            columnPathNames <- tkColumnlabelPathNames[extendCol == extend]

            for(j in seq(length(columnPathNames))) {

                columnPathName <- columnPathNames[j]
                # its index
                k <- k + 1L

                columnLabelMatrix[i, (seq(extend * spanAdj) + (j - 1) * extend * spanAdj)] <- rep(k, extend * spanAdj)

                label <- paste0(as.character(tkcget(columnPathName, "-text")), collapse = " ")

                outputGrob <- grid::gList(
                    outputGrob,
                    # the k th object
                    ribbonGrob(height = unit(labelcm, "cm"),
                               rectCol = "white", rectFill = bg, label = label,
                               textCol = fg, fontsize = fontsize/sqrt(lenColRow),
                               name = paste0("facet columnlabel:", label))
                )
            }
        }

        if(labelLocation[1L] == "top"){
            outputLayoutMatrix <- rbind(
                columnLabelMatrix,
                outputLayoutMatrix
            )

            columnAdj <- lenCol

        } else {
            outputLayoutMatrix <- rbind(
                outputLayoutMatrix,
                columnLabelMatrix
            )
        }
    }

    if(length(tkRowlabelPathNames) > 0) {

        rot <- ifelse(labelLocation[2L] == "left", 90, -90)

        # reorder the path names
        tkRowlabelPathNames <- tkRowlabelPathNames[order(extendRow,
                                                         decreasing = labelLocation[2L] == "left")]

        extendRow <- sort(extendRow, decreasing = labelLocation[2L] == "left")
        uniextend <- unique(extendRow)

        rowLabelMatrix <- matrix(NA, ncol = lenRow, nrow = nrow(outputLayoutMatrix))

        for(i in seq(lenRow)) {

            extend <- uniextend[i]
            rowPathNames <- tkRowlabelPathNames[extendRow == extend]

            for(j in seq(length(rowPathNames))) {

                rowPathName <- rowPathNames[j]
                # its index
                k <- k + 1L

                rowLabelMatrix[(seq(extend * spanAdj) + (j - 1) * extend * spanAdj) + columnAdj, i] <- rep(k, extend * spanAdj)

                label <- paste0(as.character(tkcget(rowPathName, "-text")), collapse = "")

                outputGrob <- grid::gList(
                    outputGrob,
                    # the k th object
                    ribbonGrob(width = unit(labelcm, "cm"),
                               rectCol = "white", rectFill = bg, label = label, rot = rot,
                               textCol = fg, fontsize = fontsize/sqrt(lenColRow),
                               name = paste0("facet rowlabel:", label))
                )
            }
        }

        if(labelLocation[2L] == "left"){
            outputLayoutMatrix <- cbind(
                rowLabelMatrix,
                outputLayoutMatrix
            )
        } else {
            outputLayoutMatrix <- cbind(
                outputLayoutMatrix,
                rowLabelMatrix
            )
        }
    }

    args <- list(
        grobs = outputGrob,
        layout_matrix = outputLayoutMatrix
    )

    nrowM <- nrow(outputLayoutMatrix)
    ncolM <- ncol(outputLayoutMatrix)

    anchorCol <- if(labelLocation[2L] == "right") {
        outputLayoutMatrix[, 1L]
    } else {
        outputLayoutMatrix[, ncolM]
    }

    heights <- unit(rep(1, nrowM), "null")
    heights[anchorCol != 1] <- unit(labelcm, "cm")
    args$heights <- heights

    anchorRow <- if(labelLocation[1L] == "top") {
        outputLayoutMatrix[nrowM, ]
    } else {
        outputLayoutMatrix[1L, ]
    }

    widths <- unit(rep(1, ncolM), "null")
    widths[anchorRow != 1] <- unit(labelcm, "cm")
    args$widths <- widths

    # pack xlabel, title
    if(length(titlePathName) > 0) {
        args$top <- paste0(as.character(tkcget(titlePathName, "-text")), collapse = " ")
    }
    if(length(xLabelPathName) > 0) {
        args$bottom <- paste0(as.character(tkcget(xLabelPathName, "-text")), collapse = " ")
    }
    # pack ylabel
    if(length(yLabelPathName) > 0) {
        # the collapse is set as ""
        # it is not a typo, since the ylabel for tk widget is split and layout vertically.
        args$left <- paste0(as.character(tkcget(yLabelPathName, "-text")), collapse = "")
    }
    args
}


ribbonGrob <- function(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                       width = unit(1, "npc"), height = unit(1, "npc"),
                       rectCol = l_getOption("background"),
                       rectFill = l_getOption("facetLabelBackground"),
                       label = "", rot = 0,
                       textCol = l_getOption("foreground"),
                       fontsize = 10, name = label) {
    grid::gTree(
        children = grid::gList(rectGrob(x = x, y = y,
                                        width = width, height = height,
                                        gp = gpar(col = rectCol, fill = rectFill)),
                               textGrob(x = x, y = y,
                                        label = label,
                                        rot = rot,
                                        gp = gpar(col = textCol,
                                                  fontsize = fontsize))),
        name = name
    )
}
