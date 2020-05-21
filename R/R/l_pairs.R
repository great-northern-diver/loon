
#' @title An interactive scatterplot matrix
#'
#' @description Function creates a scatterplot matrix using loon's scatterplot
#'   widgets
#'
#' @param data a data.frame with numerical data to create the scatterplot matrix
#' @param linkingGroup string giving the linkingGroup for all plots.  If missing,
#' a default \code{linkingGroup} will be determined from deparsing the \code{data}.
#' @param linkingKey a vector of strings to provide a linking identity for each row of the
#' \code{data} data.frame.  If missing, a default \code{linkingKey} will be \code{0:(nrows(data)-1)}.
#' @param showItemLabels TRUE, logical indicating whether its itemLabel pops up over a point when
#' the mouse hovers over it.
#' @param itemLabel a vector of strings to be used as pop up information when the mouse hovers
#' over a point.  If missing, the default \code{itemLabel} will be the \code{row.names(data)}.
#' @param showHistograms logical (default FALSE) to show histograms of each variable
#' or not
#' @param histLocation one "edge" or "diag", when showHistograms = TRUE
#' @param histHeightProp a positive number giving the height of the histograms as a
#' proportion of the height of the scatterplots
#' @param histArgs additional arguments to modify the `l_hist` states
#' @param showSerialAxes logical (default FALSE) indication of whether to show a serial axes plot
#' in the bottom left of the pairs plot (or not)
#' @param serialAxesArgs additional arguments to modify the `l_serialaxes` states
#' @template param_parent
#' @param ... named arguments to modify the `l_plot` states of the scatterplots
#'
#' @return an `l_pairs` object (an `l_compound` object), being a list with named elements,
#' each representing a separate interactive plot.
#' The names of the plots should be self explanatory and a list
#' of all plots can be accessed from the `l_pairs` object via `l_getPlots()`.
#' All plots are linked by default (name taken from data set if not provided).
#' Panning and zooming are constrained to work together within the scatterplot
#' matrix (and histograms).
#'
#' @seealso  \code{\link{l_plot}} and \code{\link{l_getPlots}}
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' p <- l_pairs(iris[,-5], color=iris$Species, linkingGroup = "iris")
#'
#' p <- l_pairs(iris[,-5], color=iris$Species, linkingGroup = "iris",
#'              showHistograms = TRUE, showSerialAxes = TRUE)
#' # plot names
#' names(p)
#'
#' # Each plot must be accessed to make changes not managed through
#' # linking.
#' # E.g. to change the glyph on all scatterplots to open circles
#' for (plot in l_getPlots(p)) {
#'       if (is(plot, "l_plot")) {
#'           plot["glyph"] <- "ocircle"}
#' }
#'
#' }

l_pairs <- function(data, linkingGroup, linkingKey, showItemLabels = TRUE, itemLabel,
                    showHistograms = FALSE, histLocation = c("edge", "diag"),
                    histHeightProp = 1, histArgs = list(),
                    showSerialAxes = FALSE, serialAxesArgs = list(), parent=NULL, ...) {

  args <- list(...)

  if (missing(linkingGroup)) {
    linkingGroup <- paste0("l_pairs_", deparse(substitute(data)))
  }
  # Use default as in tcl/tk
  if (missing(linkingKey)) {
    linkingKey <- NULL
  }
  if (missing(itemLabel)) {
    itemLabel <- row.names(data)
  }
  if (length(itemLabel) != nrow(data)) {
    warning("itemLabel length not equal to number of observations, using row.names(data) instead")
    itemLabel <- row.names(data)
  }


  args[['x']] <- NULL
  args[['y']] <- NULL
  args[['linkingGroup']] <- linkingGroup
  args[['linkingKey']] <- linkingKey
  args[['itemLabel']] <- itemLabel
  args[['showItemLabels']] <- showItemLabels
  if(!is.null(args[['by']])) {
    warning("'l_pairs' does not support facetting layouts")
    args[['by']] <- NULL
  }

  if (dim(data)[2] < 2) {
    args[['x']] <- data
    args[['parent']] <- parent
    return(do.call(l_plot, args))
  }

  # if (dim(data)[2] == 2) {
  #     args[['x']] <- data
  #     args[['parent']] <- parent
  #     return(do.call(l_plot, args))
  # }

  args[['showLabels']] <- FALSE
  args[['showScales']] <- FALSE
  args[['swapAxes']] <- FALSE

  new.toplevel <- FALSE
  if(is.null(parent)) {
    new.toplevel <- TRUE
    parent <- l_toplevel()
  }

  subwin <- l_subwin(parent, 'pairs')
  child <- as.character(tcl('frame', subwin))

  title <- paste("loon scatterplot matrix for",
                 deparse(substitute(data)), "data", "--path:", subwin)
  tktitle(parent) <- title
  ## parent for individual scatterplots
  args[['parent']] <- child

  nvar <- dim(data)[2]
  pair <- utils::combn(nvar, 2)
  varnames <- names(data)

  ## combn returns the variable combinations for the scatterplot
  ## matrix. The scatterplot arrangements is as follows
  ##
  ##      1      2      3      4
  ##  1  [1]   (2,1)  (3,1)  (4,1)
  ##  2         [2]   (3,2)  (4,2)
  ##  3                [3]   (4,3)
  ##  4                       [4]
  ##
  ##
  ## pair is
  ##  1  1  1  2  2  3
  ##  2  3  4  3  4  4
  cells <- nvar - 1
  text_adjustValue <- 1
  scatter_adjustValue <- 0
  span <- 1
  histLocation <- match.arg(histLocation)

  if (showHistograms) {
    if(is.null(histArgs[['showStackedColors']])) histArgs[['showStackedColors']] <- TRUE
    if(is.null(histArgs[['showOutlines']])) histArgs[['showOutlines']] <- FALSE
    if(is.null(histArgs[['yshows']])) histArgs[['yshows']] <- "density"
    if(is.null(histArgs[['showBinHandle']])) histArgs[['showBinHandle']] <- FALSE
    if(!is.null(histArgs[['by']])) {
      warning("'l_pairs' does not support facetting layouts")
      histArgs[['by']] <- NULL
    }
    # histArgs is consistent with args
    histArgs[['showLabels']] <- args[['showLabels']]
    histArgs[['showScales']] <- args[['showScales']]
    histArgs[['parent']] <- args[['parent']]
    histArgs[['linkingGroup']] <- args[['linkingGroup']]
    histArgs[['linkingKey']] <- args[['linkingKey']]
    histograms <- list()

    if(histLocation == "edge") {
      span <- ifelse(round(1/histHeightProp) >= 1, 1, round(1/histHeightProp))
      # The first half are top hists, the second half are right hists
      index <- 2:(2*nvar - 1)
      for(i in index) {
        if (i <= nvar) {
          histArgs[['x']] <- data[[varnames[i]]] # as.numeric(data[[varnames[i]]])
          histArgs[['xlabel']] <- varnames[i]
          # top level histograms
          histArgs[['swapAxes']] <- FALSE
          ix <- i
          iy <- 1
        } else {
          histArgs[['x']] <- data[[varnames[i - nvar]]] # as.numeric(data[[varnames[i - nvar]]])
          histArgs[['xlabel']] <- varnames[i - nvar]
          # right level histograms
          histArgs[['swapAxes']] <- TRUE
          ix <- nvar + 1
          iy <- i - nvar + 1
        }
        histograms[[i]] <- do.call(l_hist, histArgs)
        names(histograms)[i] <- paste('x',ix,'y',iy, sep="")
      }
      # throw errors
      if (any(sapply(histograms, function(p) {is(p, 'try-error')}))) {
        if(new.toplevel) tkdestroy(parent)
        stop("histogram could not be created.")
      }
      sapply(index,
             function(i) {
               h <- histograms[[i]]
               if(i <= nvar){
                 tkconfigure(paste(h,'.canvas',sep=''),
                             width=50,
                             height=50 * histHeightProp)
               } else {
                 tkconfigure(paste(h,'.canvas',sep=''),
                             width=50 * histHeightProp,
                             height=50)
               }
             }
      )
      # grid layout
      lapply(index,
             function(i){
               if(i <= nvar) {
                 tkgrid(histograms[[i]], row = 0,
                        column = (i-1) * span,
                        rowspan = 1, columnspan = span,
                        sticky="nesw")
               } else {
                 tkgrid(histograms[[i]], row = 1 + (i - nvar - 1)* span,
                        column = nvar * span,
                        rowspan = span, columnspan = 1,
                        sticky="nesw")
               }
             }
      )

      cells <- nvar
      text_adjustValue <- 0
      scatter_adjustValue <- 1
    } else {
      if(histHeightProp != 1) warning("histHeightProp must be 1 when histograms are placed on diagonal")
      for(i in 1:nvar){
        histArgs[['x']] <- data[[varnames[i]]] # as.numeric(data[[varnames[i]]])
        histArgs[['xlabel']] <- varnames[i]
        histArgs[['swapAxes']] <- FALSE
        histograms[[i]] <- do.call(l_hist, histArgs)
        xText <- histograms[[i]]['panX'] + histograms[[i]]['deltaX']/(2*histograms[[i]]['zoomX'])
        yText <- histograms[[i]]['panY'] + histograms[[i]]['deltaY']/(2*histograms[[i]]['zoomY'])
        layerText <- l_layer_text(histograms[[i]], xText, yText, text = names(data)[i],
                                  color = "black", size = 8)
        names(histograms)[i] <- paste('x',i,'y',i, sep="")
      }
      # throw errors
      if (any(sapply(histograms, function(p) {is(p, 'try-error')}))) {
        if(new.toplevel) tkdestroy(parent)
        stop("histogram could not be created.")
      }
      sapply(seq_len(nvar),
             function(i) {
               h <- histograms[[i]]
               tkconfigure(paste(h,'.canvas',sep=''), width=50, height=50)
             }
      )
      # grid layout
      lapply(seq_len(nvar),
             function(i){
               tkgrid(histograms[[i]], row = (i-1), column = (i-1),
                      rowspan = span, columnspan = span,
                      sticky="nesw")
             }
      )
    }
  }

  if (showSerialAxes) {
    serialAxesArgs[['data']] <- data
    serialAxesArgs[['showScales']] <- NULL
    serialAxesArgs[['swapAxes']] <- NULL
    serialAxesArgs[['axesLayout']] <- "parallel"
    serialAxesArgs[['showLabels']] <- args[['showLabels']]
    serialAxesArgs[['parent']] <- args[['parent']]
    serialAxesArgs[['linkingGroup']] <- args[['linkingGroup']]
    serialAxesArgs[['linkingKey']] <- args[['linkingKey']]
    serialAxesArgs[['itemLabel']] <- args[['itemLabel']]
    serialAxesArgs[['showItemLabels']] <- args[['showItemLabels']]
    if(!is.null(serialAxesArgs[['by']])) {
      warning("'l_pairs' does not support facetting layouts")
      histArgs[['by']] <- NULL
    }
    serialAxesSpan <- floor(nvar/2)
    serialAxes <- do.call(l_serialaxes, serialAxesArgs)
    tkconfigure(paste(serialAxes,'.canvas',sep=''),
                width= serialAxesSpan * 50,
                height = serialAxesSpan * 50)
    tkgrid(serialAxes,
           row = (cells - serialAxesSpan) * span + 1, column = 0,
           rowspan = serialAxesSpan * span, columnspan = serialAxesSpan * span,
           sticky="nesw")
  }
  scatterplots <- vector(mode="list", dim(pair)[2])

  ## create first plot
  for (i in 1:dim(pair)[2]) {
    ix <- pair[2,i]
    iy <- pair[1,i]

    args[['xlabel']] <- varnames[ix]
    args[['ylabel']] <- varnames[iy]

    args[['x']] <- data[[varnames[ix]]]
    args[['y']] <- data[[varnames[iy]]]

    scatterplots[[i]] <- do.call(l_plot, args)
    # reset names (if showHistograms)
    if (showHistograms & histLocation == "edge") {
      names(scatterplots)[i] <- paste('x',ix,'y',iy + 1, sep="")
    } else {
      names(scatterplots)[i] <- paste('x',ix,'y',iy, sep="")
    }
  }

  if (any(sapply(scatterplots, function(p) {is(p, 'try-error')}))) {
    if(new.toplevel) tkdestroy(parent)
    stop("scatterplot matrix could not be created.")
  }

  ## resize the min canvas size
  sapply(scatterplots,
         function(p) {
           tkconfigure(paste(p,'.canvas',sep=''), width=50, height=50)
         }
  )

  ## grid layout
  apply(rbind(unlist(scatterplots), pair - 1), 2,
        function(obj) {
          tkgrid(obj[1],
                 row= as.numeric(obj[2]) * span + scatter_adjustValue,
                 column = as.numeric(obj[3]) * span,
                 rowspan = span,
                 columnspan = span,
                 sticky="nesw")
        }
  )

  ## Column and Row weight such that the cells expand
  for (i in seq(0, cells)) {
    tkgrid.columnconfigure(child, i, weight = 1)
    tkgrid.rowconfigure(child, i, weight = 1)
  }

  ## Add Variable Label
  if (!showHistograms | all(c(showHistograms, histLocation == "edge"))){
    maxchar <- max(sapply(names(data), nchar))
    strf <- paste("%-", maxchar,'s', sep='')
    for (i in 1:nvar) {
      lab <- as.character(tcl('label',
                              as.character(l_subwin(child,'label')),
                              text= sprintf(strf, names(data)[i])))
      tkgrid(lab,
             row = (i - text_adjustValue - 1) * span + 1,
             column = (i - 1) * span,
             rowspan = span,
             columnspan = span)
    }
  }

  if(new.toplevel) {
    tkpack(child, fill="both", expand=TRUE)
  }
  plotsHash <- vector(mode="list", dim(pair)[2])
  for (i in 1:dim(pair)[2]) {
    ix <- pair[2,i]
    iy <- pair[1,i]

    tmpX <- which(pair[2,] == ix)
    shareX <- tmpX[tmpX != i]

    tmpY <- which(pair[1,] == iy)
    shareY <- tmpY[tmpY != i]
    plotsHash[[paste("scatter_y_",
                     scatterplots[i],
                     sep="")]] <- scatterplots[shareY]
    if(showHistograms) {
      plotsHash[[paste("scatter_x_",
                       scatterplots[i],
                       sep="")]] <- c(scatterplots[shareX], histograms[pair[2,i]])
      if(histLocation == "edge") {
        plotsHash[[paste("swap_hist_",
                         scatterplots[i],
                         sep="")]] <- histograms[pair[1,i] + nvar]
      } else {
        plotsHash[[paste("swap_hist_",
                         scatterplots[i],
                         sep="")]] <- histograms[pair[1,i]]
      }
    } else {
      plotsHash[[paste("scatter_x_",
                       scatterplots[i],
                       sep="")]] <- scatterplots[shareX]
    }
  }

  ## Make bindings for scatter synchronizing zoom and pan
  busy <- FALSE

  synchronizeScatterBindings <- function(W) {
    #print(paste(W, ', busy', busy))
    if (!busy) {
      busy <<- TRUE
      class(W) <- "loon"
      zoomX <- W['zoomX']; zoomY <- W['zoomY']
      panX <- W['panX']; panY <- W['panY']
      deltaX <- W['deltaX']; deltaY <- W['deltaY']

      lapply(plotsHash[[paste("scatter_x_",W,sep="")]], function(p) {
        l_configure(p, zoomX=zoomX, panX=panX, deltaX=deltaX)
      })
      lapply(plotsHash[[paste("scatter_y_",W,sep="")]], function(p) {
        l_configure(p, zoomY=zoomY, panY=panY, deltaY=deltaY)
      })
      if (showHistograms) {
        lapply(plotsHash[[paste("swap_hist_",W,sep="")]], function(p) {
          l_configure(p, zoomX=zoomY, panX=panY, deltaX=deltaY)
        })
      }
      busy <<- FALSE
      tcl('update', 'idletasks')
      ##assign("busy", FALSE, envir=parent.env(environment()))
    }
  }

  lapply(scatterplots,
         function(p) {
           tcl(p, 'systembind', 'state', 'add',
               c('zoomX', 'panX', 'zoomY', 'panY', 'deltaX', 'deltaY'),
               synchronizeScatterBindings)
         }
  )

  # forbidden scatter plots
  lapply(scatterplots,
         function(p) {
           tcl(p, 'systembind', 'state', 'add',
               c('showLabels', 'showScales', 'swapAxes'),
               undoScatterStateChanges)
         }
  )

  plots <- scatterplots
  if (showHistograms) {
    # synchronize hist bindings
    histsHash <- list()
    namesHist <- names(histograms)
    namesScatter <- names(scatterplots)

    scatterLayout <- xy_layout(namesScatter)
    scatterX <- scatterLayout$x
    scatterY <- scatterLayout$y

    if(histLocation == "edge") {
      for(i in 1:length(histograms)) {
        nameHist <- namesHist[i]
        if(i != 1 & i != length(histograms)) {
          if(i <= nvar) {
            histX <- xy_layout(nameHist)$x
            shareX <- which(scatterX %in% histX == TRUE)
            histsHash[[paste("hist_x_",
                             histograms[i],sep="")]] <- c(scatterplots[shareX])
          } else {
            histY <- xy_layout(nameHist)$y
            shareY <- which(scatterY %in% histY == TRUE)
            histsHash[[paste("hist_y_",
                             histograms[i],sep="")]] <- c(scatterplots[shareY])
          }
        }
      }

    } else {
      for(i in 1:length(histograms)){
        nameHist <- namesHist[i]
        histLayout <- xy_layout(nameHist)
        histX <- histLayout$x
        histY <- histLayout$y
        shareX <- which(scatterX %in% histX == TRUE)
        shareY <- which(scatterY %in% histY == TRUE)
        if(length(shareX) > 0) {
          histsHash[[paste0("hist_x_",
                            histograms[i])]] <- c(scatterplots[shareX])
        }
        if(length(shareY) > 0) {
          histsHash[[paste0("hist_y_",
                            histograms[i])]] <- c(scatterplots[shareY])
        }
      }
    }

    synchronizeHistBindings <- function(W) {
      #print(paste(W, ', busy', busy))
      if (!busy) {
        busy <<- TRUE
        class(W) <- "loon"
        zoomX <- W['zoomX']; zoomY <- W['zoomY']
        panX <- W['panX']; panY <- W['panY']
        deltaX <- W['deltaX']; deltaY <- W['deltaY']

        lapply(histsHash[[paste("hist_x_",W,sep="")]], function(h) {
          l_configure(h, zoomX=zoomX, panX=panX, deltaX=deltaX)
        })

        lapply(histsHash[[paste("hist_y_",W,sep="")]], function(h) {
          l_configure(h, zoomY=zoomX, panY=panX, deltaY=deltaX)
        })
        busy <<- FALSE
        tcl('update', 'idletasks')
        ##assign("busy", FALSE, envir=parent.env(environment()))
      }
    }
    # synchronize
    lapply(histograms,
           function(h) {
             if(!is.null(h))
               tcl(h, 'systembind', 'state', 'add',
                   c('zoomX', 'panX', 'zoomY', 'panY', 'deltaX', 'deltaY'),
                   synchronizeHistBindings)
           })
    # forbidden
    lapply(histograms,
           function(h) {
             if(!is.null(h))
               tcl(h, 'systembind', 'state', 'add',
                   c('showLabels', 'showScales'),
                   undoHistStateChanges)
           })

    if(histLocation == "edge") {
      plots<- c(plots, histograms[2:(2*nvar-1)])
    } else {
      plots<- c(plots, histograms)
    }

    callbackFunctions$state[[paste(child,"synchronizeHist", sep="_")]] <- synchronizeHistBindings
    callbackFunctions$state[[paste(child,"undoHistStateChanges", sep="_")]] <- undoHistStateChanges
  }
  if(showSerialAxes) {
    plots <- c(plots, list(serialAxes = serialAxes))
  }

  ## beware undoScatterStateChanges and synchronizeScatterBindings from garbage collector
  callbackFunctions$state[[paste(child,"synchronizeScatter", sep="_")]] <- synchronizeScatterBindings
  callbackFunctions$state[[paste(child,"undoScatterStateChanges", sep="_")]] <- undoScatterStateChanges

  structure(
    plots,
    class = c("l_pairs", "l_compound", "loon")
  )
}



## forbidden states
undoScatterStateChanges <- function(W) {
  warning("showLabels, showScales, and swapAxes can not be changed for scatterplot matrix.")
  l_configure(W, showLabels = FALSE, showScales = FALSE, swapAxes = FALSE)
}

undoHistStateChanges <- function(W) {
  warning("showLabels, showScales can not be changed for scatterplot matrix.")
  l_configure(W, showLabels = FALSE, showScales = FALSE)
}

# names must follow the pattern xayb, (a,b) is the coords of the corresponding layout
xy_layout <- function(names){
  namesSplit <- strsplit(names, split = "")
  lay_out <- as.data.frame(
    t(
      sapply(namesSplit,
             function(char){
               xpos <- which(char %in% "x" == TRUE)
               ypos <- which(char %in% "y" == TRUE)
               len_char <- length(char)
               c(as.numeric(paste0(char[(xpos + 1) : (ypos - 1)], collapse = "")),
                 as.numeric(paste0(char[(ypos + 1) : (len_char)], collapse = "")))
             }
      )
    )
  )
  colnames(lay_out) <- c("x", "y")
  lay_out
}



#' @rdname l_getPlots
#'
#' @export
l_getPlots.l_pairs <- function(target){
  # throw errors if elements of compound are a not loon widget
  lapply(target,
         function(tar){l_throwErrorIfNotLoonWidget(tar) }
  )
  target
}




#' @rdname l_getLocations
#'
#' @export
l_getLocations.l_pairs <- function(target) {

  nPlots <- length(target)
  nScatterplots <- nHistograms <- nSerialAxes <- 0
  scatterplots <- histograms <- serialAxes <- list()
  plotNames <- names(target)
  for(i in 1:nPlots) {
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

  nvar <- (-1 + sqrt(1 + 8 * nScatterplots)) / 2 + 1
  showSerialAxes <- (nSerialAxes > 0)
  showHistograms <- (nHistograms > 0)

  if(showHistograms) {
    histLocation <- if(nHistograms == (nvar - 1) * 2) "edge" else "diag"
    if(histLocation == "edge") {
      cells <- nvar + 1
    } else {
      cells <- nvar
    }
  } else {
    cells <- nvar
  }

  layout_matrix <- matrix(rep(NA, (cells)^2), nrow = cells)
  scatter_hist <- c(scatterplots, histograms)

  for(i in 1:length(scatter_hist)) {
    nameOfScatter_hist <- names(scatter_hist[i])
    pos <- xy_layout(nameOfScatter_hist)
    layout_matrix[pos$y, pos$x] <- i
  }

  if(showSerialAxes) {
    serialAxesSpan <- floor(nvar/2)
    # square space
    for(i in 1:serialAxesSpan) {
      for(j in 1:serialAxesSpan) {
        layout_matrix[cells - serialAxesSpan + i, j] <- nScatterplots + nHistograms + 1
      }
    }
  }

  list(
    nrow = cells,
    ncol = cells,
    layout_matrix = layout_matrix,
    heights = rep(1, cells),
    widths = rep(1, cells)
  )
}

