#' @export
loonGrob_layoutType.l_facet_wrap <- function(target) {
    "arrangeGrobArgs"
}


#' @export
l_get_arrangeGrobArgs.l_facet_wrap <- function(target) {

    # find the parent tk window name
    parent <- as.character(tkwinfo("parent",  target[[1L]]))
    # access all children
    children <- as.character(tkwinfo("child",  parent))

    # a trick here
    # `xlabel` and `ylabel` both include text `label`
    tkLabelPathNames <- children[grepl("label", children)]

    span <- 10L
    fontsize <- 15

    # xlabel, ylabel and title
    xl <- which(grepl("xlabel", tkLabelPathNames))
    xLabelPathName <- tkLabelPathNames[xl]
    yl <- which(grepl("ylabel", tkLabelPathNames))
    yLabelPathName <- tkLabelPathNames[yl]
    ti <- which(grepl("title", children))
    titlePathName <- children[ti]

    # labels
    if(length(c(xl, yl)) == 0) {
        labelPathNames <- tkLabelPathNames
    } else {
        labelPathNames <- tkLabelPathNames[-c(xl, yl)]
    }


    if(any(grepl("bottom", labelPathNames))) {
        labelLocation <- "bottom"
    } else {
        labelLocation <- "top"
    }

    # plot layout matrix
    locations <- l_getLocations(target)
    nrow <- locations$nrow
    ncol <- locations$ncol

    bg <- as.character(tkcget(labelPathNames[1L], "-bg"))
    fg <- as.character(tkcget(labelPathNames[1L], "-fg"))

    lgrob <- lapply(target, loonGrob)

    # label layouts
    # initial settings
    grobs <- list()
    lenTexts <- 1L
    for(i in seq(nrow)) {
        for(j in seq(ncol)) {

            xy <- paste0("x", i, "y", j)
            labelij <- labelPathNames[grepl(xy, labelPathNames)]
            lgrobij <- lgrob[[xy]]

            if(length(labelij) != 0 &&  length(lgrobij) != 0) {

                texts <- vapply(labelij,
                                function(l)
                                    paste0(as.character(tkcget(l, "-text")), collapse = " "),
                                character(1L))

                lenTexts <- length(texts)

                label.gList <- do.call(grid::gList,
                                       lapply(texts,
                                              function(text) {

                                                  ribbonGrob(rectCol = "white", rectFill = bg, label = text,
                                                             textCol = fg, fontsize = fontsize/sqrt(lenTexts),
                                                             name = paste0("facet label:", text))
                                              })
                )

                spanAdj <- round(span /sqrt(lenTexts))

                grobs[[(i - 1) * ncol + j]] <- if(labelLocation == "top") {
                    gridExtra::arrangeGrob(
                        grobs = gList(
                            label.gList,
                            lgrobij
                        ),
                        layout_matrix = matrix(c(rep(seq(lenTexts), each = spanAdj),
                                                 rep(lenTexts + 1, spanAdj * spanAdj)),
                                               ncol = spanAdj,
                                               byrow = TRUE)
                    )
                } else {
                    # bottom
                    gridExtra::arrangeGrob(
                        grobs = gList(
                            label.gList,
                            lgrobij
                        ),
                        layout_matrix = matrix(c(rep(lenTexts + 1, spanAdj * spanAdj),
                                                 rep(seq(lenTexts), each = spanAdj)),
                                               ncol = spanAdj,
                                               byrow = TRUE)
                    )
                }
            }
        }
    }

    outputGrob <- gridExtra::arrangeGrob(
        grobs = grobs,
        layout_matrix = locations$layout_matrix
    )
    k <- 1L
    outputLayoutMatrix <- matrix(rep(k, span * nrow * ncol * span), nrow = span * nrow)

    if(length(c(titlePathName, xLabelPathName, yLabelPathName)) == 0) {
        return(list(grobs = list(outputGrob)))
    }

    # pack xlabel, ylabel, title
    if(length(titlePathName) > 0) {

        title <- paste0(as.character(tkcget(titlePathName, "-text")), collapse = " ")

        k <- k + 1L

        outputLayoutMatrix <- rbind(
            rep(k, ncol(outputLayoutMatrix)),
            outputLayoutMatrix
        )

        outputGrob <- grid::gList(
            outputGrob,
            # the k th object
            ribbonGrob(rectCol = "white", rectFill = as.character(tkcget(titlePathName, "-bg")),
                       label = title, textCol = as.character(tkcget(titlePathName, "-fg")),
                       fontsize = fontsize/sqrt(lenTexts),
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
                       fontsize = fontsize/sqrt(lenTexts),
                       name = paste0("facet xlabel:", xlabel))
        )

    }

    if(length(yLabelPathName) > 0) {

        # the collapse is set as ""
        # it is not a typo, since the ylabel for tk widget is split and layout vertically.
        ylabel <- paste0(as.character(tkcget(yLabelPathName, "-text")), collapse = "")

        k <- k + 1L

        outputLayoutMatrix <- cbind(
            rep(k, nrow(outputLayoutMatrix)),
            outputLayoutMatrix
        )



        outputGrob <- grid::gList(
            outputGrob,
            ribbonGrob(rectCol = "white", rectFill = as.character(tkcget(yLabelPathName, "-bg")),
                       label = ylabel, rot = 90,
                       textCol = as.character(tkcget(yLabelPathName, "-fg")),
                       fontsize = fontsize/sqrt(lenTexts),
                       name = paste0("facet ylabel:", ylabel))
        )

    }

    list(
        grobs = outputGrob,
        layout_matrix = outputLayoutMatrix
    )
}

