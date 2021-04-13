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
    fontsize <- 12
    labelcm <- 0.6

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

    lgrob <- lapply(target,
                    function(w) {
                        lg <- loonGrob(w)
                        lg$name <- as.character(w)
                        lg
                    })

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
                labelcmAdj <- labelcm/sqrt(lenTexts)

                label.gList <- do.call(grid::gList,
                                       lapply(texts,
                                              function(text) {

                                                  ribbonGrob(height = unit(labelcmAdj, "cm"),
                                                             rectFill = bg, label = text,
                                                             textCol = fg, fontsize = fontsize/sqrt(lenTexts),
                                                             name = paste0("facet label:", text))
                                              })
                )

                spanAdj <- round(span /sqrt(lenTexts))

                if(labelLocation == "top") {

                    args <- list(
                        grobs = gList(
                            label.gList,
                            do.call(gList, setNames(list(lgrobij), lgrobij$name))
                        ),
                        layout_matrix = matrix(c(rep(seq(lenTexts), each = spanAdj),
                                                 rep(lenTexts + 1, spanAdj * spanAdj)),
                                               ncol = spanAdj,
                                               byrow = TRUE),
                        heights = unit(c(rep(labelcmAdj, lenTexts), rep(1, spanAdj)),
                                       c(rep("cm", lenTexts), rep("null", spanAdj)))
                    )

                } else {

                    args <- list(
                        grobs = gList(
                            label.gList,
                            do.call(gList, setNames(list(lgrobij), lgrobij$name))
                        ),
                        layout_matrix = matrix(c(rep(lenTexts + 1, spanAdj * spanAdj),
                                                 rep(seq(lenTexts), each = spanAdj)),
                                               ncol = spanAdj,
                                               byrow = TRUE),
                        heights = unit(c(rep(1, spanAdj), rep(labelcmAdj, lenTexts)),
                                       c(rep("null", spanAdj), rep("cm", lenTexts)))
                    )
                }

                grobs[[(i - 1) * ncol + j]] <- do.call(gridExtra::arrangeGrob, args)
            }
        }
    }

    args <- list(
        grobs = grobs,
        layout_matrix = locations$layout_matrix
    )

    # pack xlabel, title
    if(length(titlePathName) > 0) {
        args$title <- paste0(as.character(tkcget(titlePathName, "-text")), collapse = " ")
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
