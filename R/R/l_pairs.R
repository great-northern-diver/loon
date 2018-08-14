
#' @title Scatterplot Matrix in Loon
#'   
#' @description Function creates a scatterplot matrix using loon's scatterplot 
#'   widgets
#'   
#' @param data a data.frame with numerical data to create the scatterplot matrix
#' @param showHistoram show histograms of each varible or not. Default is FALSE
#' @param showSerialAxes show serialAxes or not. Default is FALSE
#' @param hist_height_prop If show histograms, the proportion between the height of histograms and the height of scatterplots
#' @param hist_diag If show histograms, display histograms on the diagonal
#' @template param_parent
#' @param ... named arguments to modify the scatterplot states
#' 
#' @return a list with scatterplot handles
#' 
#' @seealso \code{\link{l_plot}}
#' 
#' @export
#' 
#' @examples
#' p <- l_pairs(iris[,-5], color=iris$Species)

l_pairs <- function(data, showHistoram = FALSE, showSerialAxes = FALSE, hist_height_prop = 1, 
                    hist_diag = FALSE, parent=NULL, ...) {

    args <- list(...)
    if(!identical(class(data), "data.frame")) { # use of identical to deal with tibbles
        data <- as.data.frame(data)
    }

    if (is.null(args[['linkingGroup']])) {
        args[['linkingGroup']] <- deparse(substitute(data))
    }
    
    args[['x']] <- NULL
    args[['y']] <- NULL

    if (dim(data)[2] <= 2) {
        args[['x']] <- data
        args[['parent']] <- parent
        return(do.call(l_plot, args))
    }

    args[['showLabels']] <- FALSE
    args[['showScales']] <- FALSE
    args[['swapAxes']] <- FALSE
    
    new.toplevel <- FALSE
    if(is.null(parent)) {
        new.toplevel <- TRUE
        parent <- l_toplevel()
        title <- paste("loon scatterplot matrix for",
                       deparse(substitute(data)), "data")
        tktitle(parent) <- title
    }
    
    child <- as.character(tcl('frame', l_subwin(parent, 'pairs')))

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
    cellExpand <- nvar - 1
    text_adjustValue <- 1
    scatter_adjustValue <- 0
    span <- 1
    if (showHistoram) {
        if(hist_diag & hist_height_prop != 1) {
            warning("hist_height_prop must be 1 when histograms are placed on diagonal")
        }
        hist_args <- args
        hist_args[['showStackedColors']] <- TRUE
        hist_args[['showOutlines']] <- FALSE
        histograms <- list()

        if(hist_diag) {
            for(i in 1:nvar){
                hist_args[['x']] <- as.numeric(data[, i])
                hist_args[['xlabel']] <- varnames[i]
                hist_args[['swapAxes']] <- FALSE
                histograms[[i]] <- do.call(l_hist, hist_args)
                names(histograms)[i] <- paste('x',i,'y',i, sep='')
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
                       tkgrid(histograms[[i]], row = (i -1), column = (i-1), 
                              rowspan = span, columnspan = span,
                              sticky="nesw") 
                   }
            )
        } else {
            span <- ifelse(round(1/hist_height_prop) >= 1, 1, round(1/hist_height_prop))
            # The first half are top hists, the second half are right hists
            for(i in 1:(2*nvar)){
                if (i <= nvar) {
                    hist_args[['x']] <- as.numeric(data[, i])
                    hist_args[['xlabel']] <- varnames[i]
                    # top level histograms  
                    hist_args[['swapAxes']] <- FALSE
                    ix <- i
                    iy <- 1
                } else {
                    hist_args[['x']] <- as.numeric(data[, i - nvar])
                    hist_args[['xlabel']] <- varnames[i - nvar]
                    # right level histograms  
                    hist_args[['swapAxes']] <- TRUE
                    ix <- nvar + 1 
                    iy <- i - nvar + 1
                }
                histograms[[i]] <- do.call(l_hist, hist_args)
                names(histograms)[i] <- paste('x',ix,'y',iy, sep='')
            }
            # throw errors
            if (any(sapply(histograms, function(p) {is(p, 'try-error')}))) {
                if(new.toplevel) tkdestroy(parent)
                stop("histogram could not be created.")
            }
            sapply(seq_len(2*nvar), 
                   function(i) {
                       h <- histograms[[i]]
                       if(i <= nvar){
                           tkconfigure(paste(h,'.canvas',sep=''), width=50, height=50 * hist_height_prop)
                       } else {
                           tkconfigure(paste(h,'.canvas',sep=''), width=50 * hist_height_prop, height=50)
                       }
                   }
            )
            # grid layout
            lapply(2:(2*nvar-1), 
                   function(i){
                       if(i <= nvar) {
                           tkgrid(histograms[[i]], row = 0, column = (i-1) * span, 
                                  rowspan = 1, columnspan = span,
                                  sticky="nesw") 
                       } else {
                           tkgrid(histograms[[i]], row = 1 + (i - nvar - 1)* span, column = nvar * span, 
                                  rowspan = span, columnspan = 1,
                                  sticky="nesw") 
                       }
                   }
            )
            
            cellExpand <- nvar
            text_adjustValue <- 0
            scatter_adjustValue <- 1
        }
    }

    if (showSerialAxes) {
        seriel_args <- args
        seriel_args[['data']] <- data
        seriel_args[['showScales']] <- NULL
        seriel_args[['swapAxes']] <- NULL
        seriel_args[['axesLayout']] <- "parallel"
        serielAxesSpan <- floor(nvar/2)
        serialAxes <- do.call(l_serialaxes, seriel_args)
        tkconfigure(paste(serialAxes,'.canvas',sep=''), 
                    width= serielAxesSpan * 50, 
                    height = serielAxesSpan * 50)
        tkgrid(serialAxes, 
               row = (cellExpand - serielAxesSpan) * span + 1, column = 0, 
               rowspan = serielAxesSpan * span, columnspan = serielAxesSpan * span,
               sticky="nesw") 
    }
    
    scatterplots <- vector(mode="list", dim(pair)[2])
    
    ## create first plot
    for (i in 1:dim(pair)[2]) {
        ix <- pair[2,i]; iy <- pair[1,i]
        args[['x']] <- data[,ix]
        args[['y']] <- data[,iy]
        args[['xlabel']] <- varnames[ix]
        args[['ylabel']] <- varnames[iy]
        scatterplots[[i]] <- do.call(l_plot, args)
        # reset names (if showHistogram)
        if (showHistoram & !hist_diag) {
            names(scatterplots)[i] <- paste('x',ix,'y',iy + 1, sep='')
        } else {
            names(scatterplots)[i] <- paste('x',ix,'y',iy, sep='')  
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
    
    ## Column and Row wheight such that the cells expand
    for (i in seq(0, cellExpand)) {
        tkgrid.columnconfigure(child, i, weight = 1)
        tkgrid.rowconfigure(child, i, weight = 1)
    }
    ## Add Variable Label
    maxchar <- max(sapply(names(data), nchar))
    strf <- paste("%-", maxchar,'s', sep='')
    for (i in 1:nvar) {
        lab <- as.character(tcl('label', as.character(l_subwin(child,'label')),
                                text= sprintf(strf, names(data)[i])))
        tkgrid(lab, row = (i - text_adjustValue - 1) * span + 1, column = (i - 1) * span,
               rowspan = span, columnspan = span)
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
        plotsHash[[paste("y_",scatterplots[i],sep="")]] <- scatterplots[shareY]
        if(showHistoram) {
            plotsHash[[paste("x_",scatterplots[i],sep="")]] <- c(scatterplots[shareX], histograms[pair[2,i]])
            if(hist_diag) {
                plotsHash[[paste("swap_hist_",scatterplots[i],sep="")]] <- histograms[pair[1,i]]
            } else {
                plotsHash[[paste("swap_hist_",scatterplots[i],sep="")]] <- histograms[pair[1,i] + nvar]  
            }
        } else {
            plotsHash[[paste("x_",scatterplots[i],sep="")]] <- scatterplots[shareX]
        }
    }
    
    ## Make bindings for scatter synchronizing zoom and pan
    busy <- FALSE

    synchronizeBindings <- function(W) {
        #print(paste(W, ', busy', busy))
        if (!busy) {
            busy <<- TRUE
            class(W) <- "loon"
            zoomX <- W['zoomX']; zoomY <- W['zoomY']
            panX <- W['panX']; panY <- W['panY']
            deltaX <- W['deltaX']; deltaY <- W['deltaY']
            
            lapply(plotsHash[[paste("x_",W,sep="")]], function(p) {
                l_configure(p, zoomX=zoomX, panX=panX, deltaX=deltaX)
            })
            lapply(plotsHash[[paste("y_",W,sep="")]], function(p) {
                l_configure(p, zoomY=zoomY, panY=panY, deltaY=deltaY)
            })
            if (showHistoram) {
                lapply(plotsHash[[paste("swap_hist_",W,sep="")]], function(p) {
                    l_configure(p, zoomX=zoomY, panX=panY, deltaX=deltaY)
                }) 
            }
            busy <<- FALSE
            tcl('update', 'idletasks')
            ##assign("busy", FALSE, envir=parent.env(environment()))
        }
    }
    
    lapply(scatterplots, function(p) {
        tcl(p, 'systembind', 'state', 'add',
            c('zoomX', 'panX', 'zoomY', 'panY', 'deltaX', 'deltaY'),
            synchronizeBindings)
    })
        
    # forbidden scatter plots
    lapply(scatterplots, function(p) {
               tcl(p, 'systembind', 'state', 'add',
                   c('showLabels', 'showScales', 'swapAxes'),
                   undoStateChanges)
           })

    plots <- scatterplots
    if (showHistoram) {
        # forbidden histogram states (both top and right)
        lapply(histograms, function(h) {
            tcl(h, 'systembind', 'state', 'add',
                c('showLabels', 'showScales', 'swapAxes'),
                undoStateChanges)
        })
        if(hist_diag) {
            plots<- c(plots, histograms)
        } else {
            plots<- c(plots, histograms[2:(2*nvar-1)])
        }
    }
    if(showSerialAxes) {
        plots <- c(plots, list(serialAxes = serialAxes))
    }
    
    ## beware undoStateChanges and synchronizeBindings from garbage collector
    callbackFunctions$state[[paste(child,"synchronize", sep="_")]] <- synchronizeBindings
    callbackFunctions$state[[paste(child,"undo", sep="_")]] <- undoStateChanges
    
    structure(
        plots,
        class = c("l_pairs", "l_compound", "loon")
    )
}


#'@export
names.l_pairs <- function(x) {attr(x, "names")}

#' @export
l_cget.l_pairs <- function(target, state) {
    
    plotNames <- names(target)
    plots <- lapply(plotNames, 
                    function(plotName) {
                        target[[plotName]]
                        
                    })
    values <- lapply(plots, l_cget, state)
    
    values
    
}


#' @export
l_configure.l_pairs <- function(target, ...) {
    
    args <- list(...)
    states <- names(args)
    if (is.null(states) || any("" %in% states))
        stop("configuration needs key=value pairs")
    
    plotNames <- names(target)
    plots <- lapply(plotNames, 
                    function(plotName) {
                        target[[plotName]]
        
    })
    for (state in states) {
        
        switch(
            state,
            linkingGroup = lapply(plots, l_configure, 
                                  linkingGroup = args$linkingGroup, sync = "pull"),
            selected = stop("not implemented yet"),
            stop("state ", state, " not implemented")
        )
    }
    
    target
}

## forbidden states
undoStateChanges <- function(W) {
    warning("showLabels, showScales, and swapAxes can not be changed for scatterplot matrix.")
    l_configure(W, showLabels=FALSE, showScales=FALSE, swapAxes=FALSE)
}
