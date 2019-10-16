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

loonGrob.l_serialaxes <- function(target, name = NULL, gp = NULL, vp = NULL) {

    # keep consistent
    widget <- target

    # sequences
    seqName <- widget['sequence']

    displayOrder <- get_model_display_order(widget)
    active <- widget['active'][displayOrder]
    active_displayOrder <- displayOrder[active]

    scaledActiveData <- get_scaledData(
        data = char2num.data.frame(widget['data']), # convert to numeric
        sequence = seqName,
        scaling = widget['scaling'],
        displayOrder = active_displayOrder)

    layout <- widget['axesLayout']

    # different limits for different axes layout
    if(layout == "parallel") {
        xlim <- c(-0.1, 1.12)
        ylim <- c(-0.1, 1.12)
    } else {
        xlim <- c(-0.2, 1.2)
        ylim <- c(-0.2, 1.2)
    }

    gT <- gTree(
        children = gList(
            guidesGrobTree(showGuides = widget['showGuides'],
                           showAxes = widget['showAxes'],
                           len.xaxis = length(seqName),
                           layout = layout),
            labelsGrobTree(showLabels = widget['showLabels'],
                           title =  widget['title']),
            axesLabelsGrobTree(showAxesLabels = widget['showAxesLabels'],
                               seqName = seqName,
                               layout = layout),
            clipGrob(name = "clip"),
            axesGrobTree(data = scaledActiveData,
                         showArea = widget['showArea'],
                         len.xaxis = length(seqName),
                         colour = get_display_color(as_hex6color(widget['color'][active_displayOrder]),
                                                    widget['selected'][active_displayOrder]),
                         lineWidth = widget['linewidth'][active_displayOrder],
                         layout = layout)
        ),
        vp = vpStack(
            plotViewport(margins = rep(0, 4), name = "plotViewport"),
            dataViewport(xscale = xlim, yscale = ylim, name = "dataViewport")
        ),
        name = "l_serialaxes"
    )
    gTree(
        children = gList(gT),
        name = name,
        gp = gp,
        vp = vp
    )
}

######################################### aesthetics tree #########################################
guidesGrobTree <- function(showGuides = TRUE,
                           showAxes = TRUE,
                           len.xaxis = NULL,
                           layout = c("parallel", "radial")) {

    if(is.null(len.xaxis)) return(grob(name = "guides"))
    layout <- match.arg(layout)

    switch(
        layout,
        "parallel" = {
            xlim <- ylim <- c(-0.1, 1.12)
            xaxis <- seq(0, 1, length.out = len.xaxis)

            gT <- if (showGuides) {
                yaxis <- grid.pretty(ylim)
                len.yaxis <- length(yaxis)
                gTree(
                    children = do.call(
                        gList,
                        lapply(1:(len.xaxis + len.yaxis + 1),
                               function(i) {
                                   if(i == 1){
                                       rectGrob(gp = gpar(col = NA,
                                                          fill = l_getOption("canvas_bg_guides")),
                                                name = "bounding box")
                                   } else if(i > 1 && i<= (1 + len.xaxis)) {
                                       condGrob(
                                           test = showAxes,
                                           grobFun = linesGrob,
                                           name = paste("x axis", i - 1),
                                           x = unit(rep(xaxis[i - 1],2 ), "native"),
                                           y =  unit(c(0, 1), "native"),
                                           gp = gpar(col =  l_getOption("background"), lwd = 2)
                                       )
                                   } else {
                                       linesGrob(
                                           x = unit(c(0, 1), "native"),
                                           y =  unit(rep(yaxis[i - (1 + len.xaxis)],2 ), "native"),
                                           gp = gpar(col =l_getOption("background"), lwd = 2),
                                           name = paste("y axis", i - (1 + len.xaxis))
                                       )
                                   }
                               })),
                    name = "guides"
                )
            } else {
                gTree(
                    children = do.call(
                        gList,
                        lapply(1:(len.xaxis),
                               function(i) {
                                   condGrob(
                                       test = showAxes,
                                       grobFun = linesGrob,
                                       name = paste("x axis", i),
                                       x = unit(rep(xaxis[i],2 ), "native"),
                                       y =  unit(c(0, 1), "native"),
                                       gp = gpar(col =  l_getOption("foreground"), lwd = 2)
                                   )
                               }
                        )
                    ),
                    name = "guides"
                )
            }
        },
        "radial" = {

            # radial axes
            xlim <- ylim <- c(-0.2, 1.2)
            angle <- seq(0, 2*pi, length.out = len.xaxis + 1)[1:len.xaxis]

            xpos <- unit(0.5, "native")
            ypos <- unit(0.5, "native")
            radius <- default_radius()

            gT <- if(showGuides) {
                gTree(
                    children = gList(
                        rectGrob(gp = gpar(col = NA,
                                           fill = l_getOption("canvas_bg_guides")),
                                 name = "bounding box"),  # TODO find background colour need info in TCL
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
                            gp = gpar(col = l_getOption("background"),
                                      lwd = 2)   # TODO Again with width loon should use guide colours
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
                            gp = gpar(col =  l_getOption("foreground"),
                                      lwd = 2) # TODO Again with width loon should use guide colours
                        )
                    ), name = "guides"
                )
            }
        }
    )

    return(gT)
}

labelsGrobTree <- function(showLabels = TRUE,
                           title = "") {

    condGrob(
        test = showLabels & title != "",
        grobFun = textGrob,
        label = title,
        name = "title",
        y = unit(1, "npc") - unit(.8, "lines"),
        gp = gpar(fontsize = 18, fontface="bold"),
        vjust = .5
    )
}

axesLabelsGrobTree <- function(showAxesLabels = TRUE,
                               seqName = NULL,
                               layout = c("parallel", "radial")) {

    if(is.null(seqName)) return(grob(name = "labels"))
    layout <- match.arg(layout)
    len.xaxis <- length(seqName)

    switch(
        layout,
        "parallel" = {

            xaxis <- seq(0, 1, length.out = len.xaxis)

            gT <- gTree(
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
            )
        },
        "radial" = {

            # radial axes
            angle <- seq(0, 2*pi, length.out = len.xaxis + 1)[1:len.xaxis]
            radius <- default_radius()
            xpos <- unit(0.5, "native")
            ypos <- unit(0.5, "native")

            gT <- gTree(
                children = do.call(
                    gList,
                    lapply(1:(len.xaxis),
                           function(i) {
                               condGrob(
                                   test = showAxesLabels,
                                   grobFun = textGrob,
                                   name = paste("label", i),
                                   label = seqName[i],
                                   x = xpos + unit((radius + 0.1) * cos(angle[i]), "npc"),
                                   y = ypos + unit((radius + 0.1) * sin(angle[i]), "npc"),
                                   gp = gpar(fontsize = 9), vjust = 0.5
                               )
                           }
                    )
                ),
                name = "labels"
            )
        })

    return(gT)
}

axesGrobTree <- function(data = NULL,
                         showArea = FALSE,
                         len.xaxis = NULL,
                         colour = "black",
                         lineWidth = 1,
                         layout = c("parallel", "radial")) {



    if(is.null(data) || is.null(len.xaxis)) return(grob(name = "parallelAxes"))

    layout <- match.arg(layout)

    n <- dim(data)[1]

    stopifnot(
        exprs = {
            length(colour) == 1 || length(colour) == n
            length(lineWidth) == 1 || length(lineWidth) == n
        }
    )

    if(length(colour) == 1) colour <- rep(colour, n)
    if(length(lineWidth) == 1) lineWidth <- rep(lineWidth, n)

    switch(
        layout,
        "parallel" = {
            xaxis <- seq(0, 1, length.out = len.xaxis)
            gT <- gTree(
                children = gList(
                    do.call(
                        gList,
                        lapply(seq_len(n),
                               function(i){
                                   if (showArea) {
                                       polygonGrob(
                                           x = unit(c(xaxis, rev(xaxis) ), "native"),
                                           y = unit(c(data[i, ], rep(0, len.xaxis)), "native"),
                                           name = paste("polyline: showArea", i),
                                           gp = gpar(fill = colour[i],
                                                     col = NA)
                                       )
                                   } else {
                                       linesGrob(
                                           x = unit(xaxis, "native"),
                                           y = unit(data[i, ], "native"),
                                           name = paste("polyline", i),
                                           gp = gpar(col = colour[i],
                                                     lwd = lineWidth[i])
                                       )
                                   }
                               }
                        )
                    )
                ),
                name = "parallelAxes"
            )
        },
        "radial" = {
            # radial axes
            angle <- seq(0, 2*pi, length.out = len.xaxis + 1)[1:len.xaxis]

            xpos <- unit(0.5, "native")
            ypos <- unit(0.5, "native")
            radius <- default_radius()

            gT <- gTree(
                children = do.call(
                    gList,
                    lapply(seq_len(n),
                           function(i){
                               radialxais <- radius * data[i, ] * cos(angle)
                               radialyais <- radius * data[i, ] * sin(angle)
                               if(showArea){
                                   polygonGrob(
                                       x = xpos + unit(c(radialxais, radialxais[1]), "npc"),
                                       y = ypos + unit(c(radialyais, radialyais[1]), "npc"),
                                       name = paste("polyline: showArea", i),
                                       gp = gpar(fill = colour[i], col = NA)
                                   )
                               } else {
                                   linesGrob(
                                       x = xpos + unit(c(radialxais, radialxais[1]), "npc"),
                                       y = ypos + unit(c(radialyais, radialyais[1]), "npc"),
                                       name = paste("polyline", i),
                                       gp = gpar(col = colour[i], lwd = lineWidth[i])
                                   )
                               }
                           }
                    )
                ),
                name = "radialAxes"
            )
        }
    )

    return(gT)
}

######################################### data transformation #########################################

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

get_scaledData <- function(data,
                           sequence = NULL,
                           scaling = c("variable", "observation", "data", "none"),
                           displayOrder = NULL) {

    # data is the original data set
    # since "variable" scaling is based on the original data
    if(missing(data) || is.null(data) || is.null(displayOrder)) return(NULL)

    if(!is.null(sequence)) {
        col_name <- colnames(data)
        if(!all(sequence %in% col_name)) {
            warning("unknown variable names in sequence")
            sequence <- intersect(sequence, col_name)
        }
        data <-  data[, sequence]
    }

    scaling <- match.arg(scaling)

    is_char <- FALSE
    is_factor <- FALSE
    is_logical <- FALSE

    dat <- sapply(data,
                  function(x) {
                      if(is.numeric(x)) x
                      else if(is.character(x)) {
                          is_char <<- TRUE
                          as.numeric(as.factor(x))
                      } else if (is.factor(x)) {
                          is_factor <<- TRUE
                          as.numeric(x)
                      } else if(is.logical(x)) {
                          is_logical <<- TRUE
                          as.numeric(x)
                      } else stop("unknown data structure")
                  })
    # give warning once
    if(is_char || is_factor || is_logical)
        warning("No numerical columns exist", call. = FALSE)

    if(length(displayOrder) == 1) {
        dat <- setNames(as.data.frame(matrix(dat, nrow = 1)), names(dat))
        if(scaling == "variable") {
            warning("Only one observation in serialAxesData, 'scaling' will be set as 'data' by default")
            scaling <- 'data'
        }
    }

    switch(scaling,
           "variable" = {
               minV <- apply(dat, 2, "min")
               maxV <- apply(dat, 2, "max")
               dat <- dat[displayOrder, ]
               t(
                   (t(dat) - minV) / (maxV  - minV)
               )
           },
           "observation" = {
               minO <- apply(dat, 1, "min")
               maxO <- apply(dat, 1, "max")
               dat <- (dat - minO) / (maxO - minO)
               dat[displayOrder, ]
           },
           "data" = {
               minD <- min(dat)
               maxD <- max(dat)
               dat <- dat[displayOrder, ]
               (dat - minD)/ (maxD - minD)
           },
           "none" = {
               dat[displayOrder, ]
           })

}

default_radius <- function() 0.2
