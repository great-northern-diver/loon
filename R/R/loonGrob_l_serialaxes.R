# Little helper function to convert the data frame
# returned by widget['data'] from characters to numeric.
char2num.data.frame <- function(chardataframe){

    dat <- as.data.frame(suppressWarnings(sapply(chardataframe, as.numeric)))
    NAcolumn <- which(
        apply(dat, 2,
              function(column){
                  any(is.na(column))
              }
        ) == TRUE
    )
    if(length(NAcolumn) > 0){
        for(i in 1:length(NAcolumn)){
            dat[, NAcolumn[i]] <- as.numeric(as.factor(chardataframe[, NAcolumn[i]])) - 1
        }
        if(!is.data.frame(dat)) {
            as.data.frame(dat)
        } else dat
    } else dat
}

#' @rdname loonGrob
#'
#' @examples
#'
#' ## Serial axes (radial and parallel coordinate) examples
#'
#' s <- l_serialaxes(data=oliveAcids, color=olive$Area, title="olive data")
#' sGrob_radial <- loonGrob(s)
#' library(grid)
#' grid.newpage(); grid.draw(sGrob_radial)
#' s['axesLayout'] <- 'parallel'
#' sGrob_parallel <- loonGrob(s)
#' grid.newpage(); grid.draw(sGrob_parallel)
#'
#' @export

loonGrob.l_serialaxes <- function(target, name = NULL, gp = NULL, vp = NULL){

    widget <- target
    # show or not
    showGuides <- widget['showGuides']
    showAxes <- widget['showAxes']
    showAxesLabels <- widget['showAxesLabels']
    showLabels <- widget['showLabels']
    showArea <- widget['showArea']
    # which scaled method
    scaling <- widget['scaling']
    # title
    title <- widget['title']

    # sequences
    seqName <- widget['sequence']
    len.xaxis <- length(seqName)

    # active or not
    active <- widget['active']
    display_order <- get_model_display_order(widget)
    active_displayOrder <- display_order[active]
    dat <- char2num.data.frame(widget['data'])   # convert to numeric
    activeData <- dat[active_displayOrder , seqName]

    if(is.null(activeData)) {
        n <- NULL
        scaledActiveData <- NULL
    } else {
        n <- dim(activeData)[1]

        scaledActiveData <- switch(scaling,
                                   "variable" = {
                                       apply2min <- apply(dat, 2, "min")
                                       apply2max <- apply(dat, 2, "max")
                                       t(
                                           (t(activeData) - apply2min)/
                                               (apply2max  - apply2min)
                                       )
                                   },
                                   "observation" = {
                                       apply1min <- apply(activeData, 1, "min")
                                       apply1max <- apply(activeData, 1, "max")
                                       (activeData - apply1min ) / (apply1max - apply1min )
                                   },
                                   "data" = {
                                       minD <- min(dat)
                                       maxD <- max(dat)
                                       (activeData - minD)/ (maxD - minD)
                                   },
                                   "none" = activeData)
    }


    activeSelected <- widget['selected'][active_displayOrder]
    activeColor <- get_display_color( as_hex6color(widget['color'][active_displayOrder]), activeSelected)
    activeLinewidth <- widget['linewidth'][active_displayOrder]

    gT <- if (widget['axesLayout'] == "parallel") {

        xlim <- ylim <- c(-0.1, 1.12)
        xaxis <- seq(0, 1, length.out =  len.xaxis)

        gTree(
            children = gList(
                if (showGuides){
                    yaxis <- grid.pretty(ylim)
                    len.yaxis <- length(yaxis)
                    gTree(
                        children = do.call(
                            gList,
                            lapply(1:(len.xaxis + len.yaxis + 1),
                                   function(i) {
                                       if(i == 1){
                                           rectGrob(gp = gpar(col = NA, fill = "#EBEBEB" ), name = "bounding box")
                                       } else if( i > 1 && i<= (1 + len.xaxis)){
                                           condGrob(
                                               test = showAxes,
                                               grobFun = linesGrob,
                                               name = paste("x axis", i - 1),
                                               x = unit(rep(xaxis[i - 1],2 ), "native"),
                                               y =  unit(c(0, 1), "native"),
                                               gp = gpar(col =  "white", lwd = 2)
                                           )

                                       } else {
                                           linesGrob(
                                               x = unit(c(0, 1), "native"),
                                               y =  unit(rep(yaxis[i - (1 + len.xaxis)],2 ), "native"),
                                               gp = gpar(col ="white", lwd = 2),
                                               name = paste("y axis", i - (1 + len.xaxis))
                                           )
                                       }
                                   })),
                        name = "guides"
                    )
                } else {
                    gTree(
                        children =  do.call(
                            gList,
                            lapply(1:(len.xaxis),
                                   function(i) {
                                       condGrob(
                                           test = showAxes,
                                           grobFun = linesGrob,
                                           name = paste("x axis", i),
                                           x = unit(rep(xaxis[i],2 ), "native"),
                                           y =  unit(c(0, 1), "native"),
                                           gp = gpar(col =  "black", lwd = 2)
                                       )
                                   }
                            )
                        ),
                        name = "guides"
                    )
                },
                condGrob(
                    test = showLabels & title != "",
                    grobFun = textGrob,
                    label = title,
                    name = "title",
                    y = unit(1, "npc") - unit(.8, "lines"),
                    gp = gpar(fontsize = 18, fontface="bold"),
                    vjust = .5

                ),
                gTree(
                    children = do.call(
                        gList,
                        lapply(1:(len.xaxis),
                               function(i) {
                                   condGrob(
                                       test = showAxesLabels,
                                       grobFun = textGrob,
                                       label = seqName[i],
                                       name = paste("label", i),
                                       x = unit(xaxis[i], "native"),
                                       y = unit(0, "npc") + unit(1.2, "lines"),
                                       gp = gpar(fontsize = 9), vjust = 1
                                   )
                               }
                        )
                    ),
                    name = "labels"
                ),
                clipGrob(name = "clip"),
                if(is.null(scaledActiveData)) {
                    grob(name = "parallelAxes")
                } else {
                    gTree(
                        children = gList(
                            do.call(
                                gList,
                                lapply(seq_len(n),
                                       function(i){
                                           if (showArea) {
                                               polygonGrob(
                                                   x = unit(c(xaxis, rev(xaxis) ), "native"),
                                                   y = unit(c(scaledActiveData[i, ], rep(0, len.xaxis)), "native"),
                                                   name = paste("polyline: showArea", i),
                                                   gp = gpar(fill = activeColor[i],
                                                             col = NA)
                                               )
                                           } else {
                                               linesGrob(
                                                   x = unit(xaxis, "native"),
                                                   y = unit(scaledActiveData[i, ], "native"),
                                                   name = paste("polyline", i),
                                                   gp = gpar(col = activeColor[i],
                                                             lwd = activeLinewidth[i])
                                               )
                                           }
                                       }
                                )
                            )
                        ),
                        name = "parallelAxes"
                    )
                }
            ),
            vp = vpStack(
                plotViewport(margins = rep(0, 4), name = "plotViewport"),
                dataViewport(xscale = xlim, yscale = ylim, name = "dataViewport")
            ),
            name = "l_serialaxes"
        )
    } else {
        # radial axes
        xlim <- ylim <- c(-0.2, 1.2)
        angle <- seq(0, 2*pi, length.out = len.xaxis + 1)[1:len.xaxis]

        xpos <- unit(0.5, "native")
        ypos <- unit(0.5, "native")
        radius <- 0.2

        gTree(
            children = gList(
                if (showGuides) {
                    gTree(
                        children = gList(
                            rectGrob(gp = gpar(col = NA, fill = "#EBEBEB" ), name = "bounding box"),  # TODO find background colour need info in TCL
                            polygonGrob(xpos + unit(radius * cos(seq(0, 2*pi, length=101)), "npc"),
                                        ypos + unit(radius * sin(seq(0, 2*pi, length=101)), "npc"),
                                        gp = gpar(fill = NA, col = l_getOption("guidelines"),
                                                  lwd = 2),
                                        name = "bounding line" # TODO find line width
                            ),
                            condGrob(
                                test = showAxes,
                                grobFun = polylineGrob,
                                name = "axes",
                                x = xpos + unit(c(rep(0, len.xaxis) ,radius * cos(angle)), "npc"),
                                y =  ypos + unit(c(rep(0, len.xaxis) ,radius * sin(angle)), "npc"),
                                id = rep(1:len.xaxis, 2),
                                gp = gpar(col = "white", lwd = 2)   # TODO Again with width loon should use guide colours
                            )
                        ),
                        name = "guides"
                    )
                } else {
                    gTree(
                        children = gList(
                            condGrob(
                                test = showAxes,
                                grobFun = polylineGrob,
                                name = "axes",
                                x = unit(c(rep(0, len.xaxis) ,radius * cos(angle)), "npc") + xpos,
                                y = unit(c(rep(0, len.xaxis) ,radius * sin(angle)), "npc") + ypos,
                                id = rep(1:len.xaxis, 2),
                                gp = gpar(col =  "black", lwd = 2) # TODO Again with width loon should use guide colours
                            )
                        ), name = "guides"
                    )
                },

                condGrob(
                    test = showLabels & title != "",
                    grobFun = textGrob,
                    name = "title",
                    label = title,
                    y = unit(1, "npc") - unit(.8, "lines"),
                    gp = gpar(fontsize = 18, fontface="bold"),
                    vjust = .5
                ),
                gTree(
                    children = do.call(
                        gList,
                        lapply(1:(len.xaxis),
                               function(i) {
                                   condGrob(
                                       test = showAxesLabels,
                                       grobFun = textGrob,
                                       name = paste("label", i),
                                       label = seqName[i],
                                       x = unit((radius + 0.1) * cos(angle[i]), "npc") + xpos,
                                       y = unit((radius + 0.1) * sin(angle[i]), "npc") + ypos,
                                       gp = gpar(fontsize = 9), vjust = 0.5
                                   )
                               }
                        )
                    ),
                    name = "labels"
                ),
                clipGrob(name = "clip"),
                if(is.null(scaledActiveData)) {
                    grob(name = "radialAxes")
                }
                else {
                    gTree(
                        children = do.call(
                            gList,
                            lapply(seq_len(n),
                                   function(i){
                                       radialxais <- radius * scaledActiveData[i,] * cos(angle)
                                       radialyais <- radius * scaledActiveData[i,] * sin(angle)
                                       if(showArea){
                                           polygonGrob(
                                               x = xpos + unit(c(radialxais, radialxais[1]), "npc"),
                                               y = ypos + unit(c(radialyais, radialyais[1]), "npc"),
                                               name = paste("polyline: showArea", i),
                                               gp = gpar(fill = activeColor[i], col = NA)
                                           )
                                       } else {
                                           linesGrob(
                                               x = xpos + unit(c(radialxais, radialxais[1]), "npc"),
                                               y = ypos + unit(c(radialyais, radialyais[1]), "npc"),
                                               name = paste("polyline", i),
                                               gp = gpar(col = activeColor[i], lwd = activeLinewidth[i])
                                           )
                                       }
                                   }
                            )
                        ),
                        name = "radialAxes"
                    )
                }
            ),
            vp = vpStack(
                plotViewport(margins = rep(0, 4), name = "plotViewport"),
                dataViewport(xscale = xlim, yscale = ylim, name = "dataViewport")
            ),
            name = "l_serialaxes"
        )
    }
    gTree(
        children = gList(gT),
        name = name,
        gp = gp,
        vp = vp
    )
}
