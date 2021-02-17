
#' @title An interactive scatterplot matrix
#'
#' @description Function creates a scatterplot matrix using loon's scatterplot
#'   widgets
#'
#' @param data a data.frame with numerical data to create the scatterplot matrix
#' @param connectedScales Determines how the scales of the panels are to be connected.
#' \itemize{
#' \item{\code{"cross"}: only the scales in the same row and the same column are connected;}
#' \item{\code{"none"}: neither "x" nor "y" scales are connected in any panels.}
#' }
#' @param linkingGroup string giving the linkingGroup for all plots. If missing,
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
#' @param span How many column/row occupies for each widget
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
#' @importFrom utils setTxtProgressBar txtProgressBar
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
l_pairs <- function(data,
                    connectedScales = c("cross", "none"),
                    linkingGroup, linkingKey, showItemLabels = TRUE, itemLabel,
                    showHistograms = FALSE, histLocation = c("edge", "diag"),
                    histHeightProp = 1, histArgs = list(),
                    showSerialAxes = FALSE, serialAxesArgs = list(), parent=NULL,
                    span = 10L, ...) {

  ### as the number of plots rises, the running time increases dramatically
  ### so we provide a progress bar to give the progress information
  # the minimum length of plots to give the progress bar
  minLenToGivePb <- 10L

  substitueData <- deparse(substitute(data))
  # matrix input
  data <- as.data.frame(data)

  dotArgs <- list(...)

  new.linkingGroup <- FALSE
  if (missing(linkingGroup)) {
    new.linkingGroup <- TRUE
    linkingGroup <- paste0("l_pairs_", substitueData)
  }

  call <- match.call()

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
  connectedScales <- match.arg(connectedScales)

  sync <- dotArgs[['sync']]
  # if null, it is always **pull**
  if(is.null(sync)) sync <- "pull"
  dotArgs[['sync']] <- NULL
  dotArgs[['x']] <- NULL
  dotArgs[['y']] <- NULL
  dotArgs[['linkingKey']] <- linkingKey
  dotArgs[['itemLabel']] <- itemLabel
  dotArgs[['showItemLabels']] <- showItemLabels
  if(!is.null(dotArgs[['by']])) {
    warning("'l_pairs' does not support facetting layouts")
    dotArgs[['by']] <- NULL
  }

  if (dim(data)[2] < 2) {
    dotArgs[['x']] <- data
    dotArgs[['parent']] <- parent
    return(do.call(l_plot, dotArgs))
  }

  # if (dim(data)[2] == 2) {
  #     dotArgs[['x']] <- data
  #     dotArgs[['parent']] <- parent
  #     return(do.call(l_plot, dotArgs))
  # }

  dotArgs[['showLabels']] <- FALSE
  dotArgs[['showScales']] <- FALSE
  dotArgs[['swapAxes']] <- FALSE

  new.toplevel <- FALSE
  if(is.null(parent)) {
    new.toplevel <- TRUE
    parent <- l_toplevel()
  }

  subwin <- l_subwin(parent, 'pairs')
  child <- as.character(tcl('frame', subwin))

  title <- paste("loon scatterplot matrix for",
                 substitueData, "data", "--path:", subwin)
  tktitle(parent) <- title
  ## parent for individual scatterplots
  dotArgs[['parent']] <- child

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
  scatter_adjustValue <- 0
  histLocation <- match.arg(histLocation)
  histspan <- 0L

  totalNumPlots <- 0L
  numScatterPlots <- dim(pair)[2]
  numHistPlots <- 0L
  numSerialaxesPlot <- 0L

  totalNumPlots <- totalNumPlots + numScatterPlots
  if(showHistograms) {
    numHistPlots <- if(histLocation == "edge") {
      2*nvar - 2
    } else {
      # diag
      nvar
    }
    totalNumPlots <- totalNumPlots + numHistPlots
  }
  if(showSerialAxes) {
    numSerialaxesPlot <- 1
    totalNumPlots <- totalNumPlots + numSerialaxesPlot
  }

  pbPlots <- l_txtProgressBar(min = 0, max = totalNumPlots,
                              minLenToGivePb = minLenToGivePb,
                              message = "Prepare Plots:")

  histograms <- list()
  if (showHistograms) {
    if(is.null(histArgs[['showStackedColors']])) histArgs[['showStackedColors']] <- TRUE
    if(is.null(histArgs[['showOutlines']])) histArgs[['showOutlines']] <- FALSE
    if(is.null(histArgs[['yshows']])) histArgs[['yshows']] <- "density"
    if(is.null(histArgs[['showBinHandle']])) histArgs[['showBinHandle']] <- FALSE
    if(!is.null(histArgs[['by']])) {
      warning("'l_pairs' does not support facetting layouts", call. = FALSE)
      histArgs[['by']] <- NULL
    }
    # histArgs is consistent with dotArgs
    histArgs[['showLabels']] <- dotArgs[['showLabels']]
    histArgs[['showScales']] <- dotArgs[['showScales']]
    histArgs[['parent']] <- dotArgs[['parent']]
    histArgs[['linkingGroup']] <- NULL
    histArgs[['linkingKey']] <- dotArgs[['linkingKey']]

    switch(histLocation,
           "edge" = {
             histspan <- round(histHeightProp * span)
             # The first half are top hists, the second half are right hists
             index <- seq(numHistPlots) + 1
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

               l_setTxtProgressBar(pbPlots, i - 1)
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
                               rowspan = histspan, columnspan = span,
                               sticky="nesw")
                      } else {
                        tkgrid(histograms[[i]], row = histspan + (i - nvar - 1)* span,
                               column = nvar * span,
                               rowspan = span, columnspan = histspan,
                               sticky="nesw")
                      }
                    }
             )

             scatter_adjustValue <- 1
           },
           "diag" = {
             if(histHeightProp != 1) {
               warning("histHeightProp must be 1 when histograms are placed on diagonal")
               histHeightProp <- 1
             }
             for(i in seq(numHistPlots)) {
               histArgs[['x']] <- data[[varnames[i]]] # as.numeric(data[[varnames[i]]])
               histArgs[['xlabel']] <- varnames[i]
               histArgs[['swapAxes']] <- FALSE
               histograms[[i]] <- do.call(l_hist, histArgs)
               xText <- histograms[[i]]['panX'] + histograms[[i]]['deltaX']/(2*histograms[[i]]['zoomX'])
               yText <- histograms[[i]]['panY'] + histograms[[i]]['deltaY']/(2*histograms[[i]]['zoomY'])
               layerText <- l_layer_text(histograms[[i]], xText, yText, text = names(data)[i],
                                         color = "black", size = 8)
               names(histograms)[i] <- paste('x',i,'y',i, sep="")

               l_setTxtProgressBar(pbPlots, i)
             }
             # throw errors
             if (any(sapply(histograms, function(p) {is(p, 'try-error')}))) {
               if(new.toplevel) tkdestroy(parent)
               stop("histogram could not be created.")
             }
             sapply(seq_len(numHistPlots),
                    function(i) {
                      h <- histograms[[i]]
                      tkconfigure(paste(h,'.canvas',sep=''), width=50, height=50)
                    }
             )
             # grid layout
             lapply(seq_len(numHistPlots),
                    function(i){
                      tkgrid(histograms[[i]], row = (i-1) * span, column = (i-1) * span,
                             rowspan = span, columnspan = span,
                             sticky="nesw")
                    }
             )
           })

    histograms <- Filter(Negate(is.null), histograms)
    namesHist <- names(histograms)
    histLayout <- xy_layout(namesHist)
    histX <- histLayout$x
    histY <- histLayout$y
  }

  if (showSerialAxes) {
    serialAxesArgs[['data']] <- data
    serialAxesArgs[['showScales']] <- NULL
    serialAxesArgs[['swapAxes']] <- NULL
    serialAxesArgs[['axesLayout']] <- "parallel"
    serialAxesArgs[['showLabels']] <- dotArgs[['showLabels']]
    serialAxesArgs[['parent']] <- dotArgs[['parent']]
    serialAxesArgs[['linkingGroup']] <- NULL
    serialAxesArgs[['linkingKey']] <- dotArgs[['linkingKey']]
    serialAxesArgs[['itemLabel']] <- dotArgs[['itemLabel']]
    serialAxesArgs[['showItemLabels']] <- dotArgs[['showItemLabels']]
    if(!is.null(serialAxesArgs[['by']])) {
      warning("'l_pairs' does not support facetting layouts")
      serialAxesArgs[['by']] <- NULL
    }
    serialAxesSpan <- floor(nvar/2)
    serialAxes <- do.call(l_serialaxes, serialAxesArgs)

    # give progress bar
    l_setTxtProgressBar(pbPlots, numHistPlots + 1)

    tkconfigure(paste(serialAxes,'.canvas',sep=''),
                width= serialAxesSpan * 50,
                height = serialAxesSpan * 50)
    tkgrid(serialAxes,
           row = (nvar - serialAxesSpan) * span + histspan, column = 0,
           rowspan = serialAxesSpan * span, columnspan = serialAxesSpan * span,
           sticky="nesw")
  }

  scatterplots <- vector(mode="list", dim(pair)[2])

  ## create first plot
  for (i in 1:dim(pair)[2]) {
    ix <- pair[2,i]
    iy <- pair[1,i]

    dotArgs[['xlabel']] <- varnames[ix]
    dotArgs[['ylabel']] <- varnames[iy]

    dotArgs[['x']] <- data[[varnames[ix]]]
    dotArgs[['y']] <- data[[varnames[iy]]]

    scatterplots[[i]] <- do.call(l_plot, dotArgs)

    # give progress bar
    l_setTxtProgressBar(pbPlots, i + numHistPlots + numSerialaxesPlot)

    # reset names (if showHistograms)
    if (showHistograms & histLocation == "edge") {
      names(scatterplots)[i] <- paste('x',ix,'y',iy + 1, sep="")
    } else {
      names(scatterplots)[i] <- paste('x',ix,'y',iy, sep="")
    }
  }

  # close the pb
  l_close(pbPlots)

  namesScatter <- names(scatterplots)
  scatterLayout <- xy_layout(namesScatter)
  scatterX <- scatterLayout$x
  scatterY <- scatterLayout$y

  if (any(sapply(scatterplots, function(p) {is(p, 'try-error')}))) {
    if(new.toplevel) tkdestroy(parent)
    stop("scatterplot matrix could not be created.")
  }

  ## resize the min canvas size
  sapply(scatterplots,
         function(p) {
           tkconfigure(paste(p,'.canvas',sep=''),
                       width=50,
                       height=50)
         }
  )

  ## grid layout
  apply(rbind(unlist(scatterplots), pair - 1), 2,
        function(obj) {
          tkgrid(obj[1],
                 row= as.numeric(obj[2]) * span + scatter_adjustValue * histspan,
                 column = as.numeric(obj[3]) * span,
                 rowspan = span,
                 columnspan = span,
                 sticky="nesw")
        }
  )

  ## Column and Row weight such that the cells expand
  for (i in seq(0, nvar * span + histspan - 1)) {
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
             row = (i - 1) * span + histspan,
             column = (i - 1) * span,
             rowspan = span,
             columnspan = span)
    }
  }

  if(new.toplevel) {
    tkpack(child, fill="both", expand=TRUE)
  }

  pbScales <- l_txtProgressBar(min = 0, max = numScatterPlots,
                               minLenToGivePb = minLenToGivePb,
                               message = "Bind Scales:")
  plotsHash <- list()
  for (i in seq(numScatterPlots)) {
    ix <- pair[2,i]
    iy <- pair[1,i]

    tmpX <- which(pair[2,] == ix)
    shareX <- tmpX[tmpX != i]

    tmpY <- which(pair[1,] == iy)
    shareY <- tmpY[tmpY != i]
    plotsHash[[paste0("scatter_y_", scatterplots[i])]] <- scatterplots[shareY]

    if(showHistograms) {

      histShareX <- which(histX %in% scatterX[i])
      histShareY <- which(histY %in% scatterY[i])

      plotsHash[[paste0("scatter_x_", scatterplots[i])]] <- c(scatterplots[shareX], histograms[histShareX])

      plotsHash[[paste0("swap_hist_", scatterplots[i])]] <- histograms[histShareY]

      if(histLocation == "diag") {
        plotsHash[[paste0("hist_sync_y", scatterplots[i])]] <- c(scatterplots[scatterY %in% histShareX])
        plotsHash[[paste0("hist_sync_x", scatterplots[i])]] <- c(scatterplots[scatterX %in% histShareY])
      }
    } else {
      plotsHash[[paste("scatter_x_",
                       scatterplots[i],
                       sep="")]] <- scatterplots[shareX]
    }

    l_setTxtProgressBar(pbScales, i)
  }
  l_close(pbScales)

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

      lapply(plotsHash[[paste0("scatter_x_",W)]], function(p) {
        if(is.null(p)) return(NULL)
        l_configure(p, zoomX=zoomX, panX=panX, deltaX=deltaX)
      })
      lapply(plotsHash[[paste0("scatter_y_",W)]], function(p) {
        if(is.null(p)) return(NULL)
        l_configure(p, zoomY=zoomY, panY=panY, deltaY=deltaY)
      })
      if (showHistograms) {
        lapply(plotsHash[[paste0("swap_hist_",W)]], function(p) {
          if(is.null(p)) return(NULL)
          l_configure(p, zoomX=zoomY, panX=panY, deltaX=deltaY)
        })

        if(histLocation == "diag") {
          lapply(plotsHash[[paste0("hist_sync_x",W)]],
                 function(p) {
                   if(is.null(p)) return(NULL)
                   l_configure(p, zoomX=zoomY, panX=panY, deltaX=deltaY)
                 })

          lapply(plotsHash[[paste0("hist_sync_y",W)]],
                 function(p) {
                   if(is.null(p)) return(NULL)
                   l_configure(p, zoomY=zoomX, panY=panX, deltaY=deltaX)
                 })
        }
      }
      busy <<- FALSE
      tcl('update', 'idletasks')
      ##assign("busy", FALSE, envir=parent.env(environment()))
    }
  }

  lapply(scatterplots,
         function(p) {

           if(connectedScales == "cross") {
             tcl(p, 'systembind', 'state', 'add',
                 c('zoomX', 'panX', 'zoomY', 'panY', 'deltaX', 'deltaY'),
                 synchronizeScatterBindings)
           }
           tcl(p, 'systembind', 'state', 'add',
               c('showLabels', 'showScales', 'swapAxes'),
               undoScatterStateChanges)
         }
  )


  plots <- scatterplots
  if (showHistograms) {
    # synchronize hist bindings
    histsHash <- list()

    setHistScalesStartTime <- Sys.time()
    if(histLocation == "edge") {

      for(i in seq(numHistPlots)) {

        if(i <= (nvar - 1)) {

          shareX <- which(scatterX %in% histX[i])
          histsHash[[paste0("hist_x_", histograms[i])]] <- c(scatterplots[shareX])
        } else {

          shareY <- which(scatterY %in% histY[i])
          histsHash[[paste0("hist_y_", histograms[i])]] <- c(scatterplots[shareY])
        }
      }

    } else {

      for(i in seq(numHistPlots)) {

        shareX <- which(scatterX %in% histX[i])
        shareY <- which(scatterY %in% histY[i])

        if(length(shareX) > 0) {
          histsHash[[paste0("hist_x_", histograms[i])]] <- c(scatterplots[shareX])
        }

        if(length(shareY) > 0) {
          histsHash[[paste0("hist_y_", histograms[i])]] <- c(scatterplots[shareY])
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
          if(is.null(h)) return(NULL)
          l_configure(h, zoomX=zoomX, panX=panX, deltaX=deltaX)
        })

        lapply(histsHash[[paste("hist_y_",W,sep="")]], function(h) {
          if(is.null(h)) return(NULL)
          l_configure(h, zoomY=zoomX, panY=panX, deltaY=deltaX)
        })
        busy <<- FALSE
        tcl('update', 'idletasks')
        ##assign("busy", FALSE, envir=parent.env(environment()))
      }
    }

    # forbidden
    lapply(histograms,
           function(h) {

             if(is.null(h)) return(NULL)

             if(connectedScales == "cross") {

               tcl(h, 'systembind', 'state', 'add',
                   c('zoomX', 'panX', 'zoomY', 'panY', 'deltaX', 'deltaY'),
                   synchronizeHistBindings)
             }


             tcl(h, 'systembind', 'state', 'add',
                 c('showLabels', 'showScales'),
                 undoHistStateChanges)
           })


    plots<- c(plots, histograms)

    callbackFunctions$state[[paste(child,"synchronizeHist", sep="_")]] <- synchronizeHistBindings
    callbackFunctions$state[[paste(child,"undoHistStateChanges", sep="_")]] <- undoHistStateChanges
  }
  if(showSerialAxes) {
    plots <- c(plots, list(serialAxes = serialAxes))
  }

  # configure sync
  pbLinking <- l_txtProgressBar(min = 0, max = totalNumPlots,
                                minLenToGivePb = minLenToGivePb,
                                message = "Configure plot linking:")

  lapply(seq(totalNumPlots),
         function(i) {

           plot <- plots[[i]]
           type <- class(plot)[1L]

           modifiedLinkedStates <- l_modifiedLinkedStates(type, names(call))

           if(!new.linkingGroup) {

             syncTemp <- ifelse(length(modifiedLinkedStates) == 0,  sync, "pull")
             # give message once
             if(i == 1L && syncTemp == "push") {
               message("The modification of linked states is not detected",
                       " so that the default settings will be pushed to all plots")
             }
             l_configure(plot,
                         linkingGroup = linkingGroup,
                         sync = syncTemp)

             if(sync == "push" && length(modifiedLinkedStates) > 0) {

               do.call(l_configure,
                       c(
                         list(
                           target = plot,
                           linkingGroup = linkingGroup,
                           sync = sync
                         ),
                         dotArgs[modifiedLinkedStates]
                       )
               )
             } else {
               if(i == 1L) {
                 l_linkingWarning(plot, sync, args = dotArgs,
                                  modifiedLinkedStates = modifiedLinkedStates)
               }
             }

           } else {

             l_configure(plot,
                         linkingGroup = linkingGroup,
                         sync = sync)
           }
           l_setTxtProgressBar(pbLinking, i)
         })
  l_close(pbLinking)

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

l_txtProgressBar <- function(min = 0, max, minLenToGivePb, style = 3, message = "") {
  if(max > minLenToGivePb) {
    message(message)
    txtProgressBar(min = min, max = max, style = style)
  } else return(NULL)
}

l_setTxtProgressBar <- function(pb, value) {
  if(is.null(pb)) return(NULL)
  setTxtProgressBar(pb, value)
}

l_close <- function(con, ...) {
  if(is.null(con)) return(NULL)
  close(con, ...)
}
