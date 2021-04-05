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
        grobs = lapply(target, loonGrob),
        layout_matrix = locations$layout_matrix
    )

    # find the number of a string that right after the letter "ex" (extend)
    pat <- "^.*ex.*?([0-9]+)"

    expendCol <- as.numeric(gsub(pat, "\\1", tkColumnlabelPathNames))
    lenCol <- length(unique(expendCol))

    expendRow <- as.numeric(gsub(pat, "\\1", tkRowlabelPathNames))
    lenRow <- length(unique(expendRow))

    lenColRow <- max(c(lenCol, lenRow))
    spanAdj <- round(span /sqrt(lenColRow))

    # k is the index of each arranged widget
    k <- 1L
    outputLayoutMatrix <- matrix(rep(k, spanAdj * nrow * ncol * spanAdj),
                                   nrow = spanAdj * nrow)
    columnAdj <- 0

    if(length(tkColumnlabelPathNames) > 0) {

        # reorder the path names
        tkColumnlabelPathNames <- tkColumnlabelPathNames[order(expendCol,
                                                               decreasing = labelLocation[1L] == "top")]

        expendCol <- sort(expendCol, decreasing = labelLocation[1L] == "top")
        uniExpend <- unique(expendCol)

        columnLabelMatrix <- matrix(NA, nrow = lenCol, ncol = ncol * spanAdj)

        for(i in seq(lenCol)) {

            ex <- uniExpend[i]
            columnPathNames <- tkColumnlabelPathNames[expendCol == ex]

            for(j in seq(length(columnPathNames))) {

                columnPathName <- columnPathNames[j]
                # its index
                k <- k + 1L

                columnLabelMatrix[i, (seq(ex * spanAdj) + (j - 1) * ex * spanAdj)] <- rep(k, ex * spanAdj)

                label <- paste0(as.character(tkcget(columnPathName, "-text")), collapse = " ")

                outputGrob <- grid::gList(
                    outputGrob,
                    # the k th object
                    ribbonGrob(rectCol = "white", rectFill = bg, label = label,
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

    # pack xlabel, title
    if(length(titlePathName) > 0) {

        title <- paste0(as.character(tkcget(titlePathName, "-text")), collapse = " ")

        k <- k + 1L
        columnAdj <- columnAdj + 1L

        outputLayoutMatrix <- rbind(
            rep(k, ncol(outputLayoutMatrix)),
            outputLayoutMatrix
        )

        outputGrob <- grid::gList(
            outputGrob,
            # the k th object
            ribbonGrob(rectCol = "white", rectFill = as.character(tkcget(titlePathName, "-bg")),
                       label = title, textCol = as.character(tkcget(titlePathName, "-fg")),
                       fontsize = fontsize/sqrt(lenColRow),
                       name = paste0("facet title:", title))
        )

    }

    if(length(xLabelPathName) > 0) {

        xlabel <- paste0(as.character(tkcget(xLabelPathName, "-text")), collapse = " ")

        k <- k + 1L

        outputLayoutMatrix <- rbind(
            outputLayoutMatrix,
            rep(k, ncol(outputLayoutMatrix))
        )

        outputGrob <- grid::gList(
            outputGrob,
            # the kth object
            ribbonGrob(rectCol = "white", rectFill = as.character(tkcget(xLabelPathName, "-bg")),
                       label = xlabel, textCol = as.character(tkcget(xLabelPathName, "-fg")),
                       fontsize = fontsize/sqrt(lenColRow),
                       name = paste0("facet xlabel:", xlabel))
        )

    }

    if(length(tkRowlabelPathNames) > 0) {

        rot <- ifelse(labelLocation[2L] == "left", 90, -90)

        # reorder the path names
        tkRowlabelPathNames <- tkRowlabelPathNames[order(expendRow,
                                                         decreasing = labelLocation[2L] == "left")]

        expendRow <- sort(expendRow, decreasing = labelLocation[2L] == "left")
        uniExpend <- unique(expendRow)

        rowLabelMatrix <- matrix(NA, ncol = lenRow, nrow = nrow(outputLayoutMatrix))

        for(i in seq(lenRow)) {

            ex <- uniExpend[i]
            rowPathNames <- tkRowlabelPathNames[expendRow == ex]

            for(j in seq(length(rowPathNames))) {

                rowPathName <- rowPathNames[j]
                # its index
                k <- k + 1L

                rowLabelMatrix[(seq(ex * spanAdj) + (j - 1) * ex * spanAdj) + columnAdj, i] <- rep(k, ex * spanAdj)

                label <- paste0(as.character(tkcget(rowPathName, "-text")), collapse = "")

                outputGrob <- grid::gList(
                    outputGrob,
                    # the k th object
                    ribbonGrob(rectCol = "white", rectFill = bg, label = label, rot = rot,
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

    # pack ylabel
    if(length(yLabelPathName) > 0) {

        # the collapse is set as ""
        # it is not a typo, since the ylabel for tk widget is split and layout vertically.
        ylabel <- paste0(as.character(tkcget(yLabelPathName, "-text")), collapse = "")

        k <- k + 1L

        yLabelRow <- rep(NA, nrow(outputLayoutMatrix))
        yLabelRow[columnAdj + seq(nrow * spanAdj)] <- k

        outputLayoutMatrix <- cbind(
            yLabelRow,
            outputLayoutMatrix
        )

        outputGrob <- grid::gList(
            outputGrob,
            ribbonGrob(rectCol = "white", rectFill = as.character(tkcget(yLabelPathName, "-bg")),
                       label = ylabel, rot = 90,
                       textCol = as.character(tkcget(yLabelPathName, "-fg")),
                       fontsize = fontsize/sqrt(lenColRow),
                       name = paste0("facet ylabel:", ylabel))
        )

    }

    list(
        grobs = outputGrob,
        layout_matrix = outputLayoutMatrix
    )
}


ribbonGrob <- function(rectCol = "white", rectFill = "grey92", label = "",
                       rot = 0, textCol = "black", fontsize = 10, name = label) {
    grid::gTree(
        children = grid::gList(rectGrob(gp = gpar(col = rectCol, fill = rectFill)),
                               textGrob(label = label,
                                        rot = rot,
                                        gp = gpar(col = textCol,
                                                  fontsize = fontsize))),
        name = name
    )
}
