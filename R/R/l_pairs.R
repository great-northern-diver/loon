
#' @title Scatterplot Matrix in Loon
#'   
#' @description Function creates a scatterplot matrix using loon's scatterplot 
#'   widgets
#'   
#' @param data a data.frame with numerical data to create the scatterplot matrix
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
l_pairs <- function(data, parent=NULL, ...) {

    args <- list(...)
    
    if(!is.data.frame(data)) {
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
        tktitle(parent) <- paste("loon scatterplot matrix for",
                                 deparse(substitute(data)), "data")
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
    
    plots <- vector(mode="list", dim(pair)[2])
    ## create first plot
    for (i in 1:dim(pair)[2]) {
        ix <- pair[2,i]; iy <- pair[1,i]
        args[['x']] <- data[,ix]
        args[['y']] <- data[,iy]
        args[['xlabel']] <- varnames[ix]
        args[['ylabel']] <- varnames[iy]
        plots[[i]] <- do.call(l_plot, args)
        names(plots)[i] <- paste('x',ix,'y',iy, sep='')
    }
    
    if (any(sapply(plots, function(p) {is(p, 'try-error')}))) {
        if(new.toplevel) tkdestroy(parent)
        stop("scatterplot matrix could not be created.")
    }
    
    ## resize the min canvas size
    sapply(plots, function(p) {
               tkconfigure(paste(p,'.canvas',sep=''), width=50, height=50)
           })

    ## grid layout
    apply(rbind(unlist(plots), pair-1), 2, function(col) {
        tkgrid(col[1], row=col[2], column=col[3], sticky="nesw")
    })
    
    ## Column and Row wheight such that the cells expand
    for (i in seq(0, nvar-1)) {
        tkgrid.columnconfigure(child, i, weight = 1)
        tkgrid.rowconfigure(child, i, weight = 1)
    }
    
    ## Add Variable Label
    maxchar <- max(sapply(names(data), nchar))
    strf <- paste("%-", maxchar,'s', sep='')
    for (i in 1:nvar) {
        lab <- as.character(tcl('label', as.character(l_subwin(child,'label')),
                                text= sprintf(strf, names(data)[i])))
        tkgrid(lab, row = i - 1, column = i - 1)
    }
    
    if(new.toplevel) {
        tkpack(child, fill="both", expand=TRUE)
    }

    
    ## Make bindings for synchronizing zoom and pan
    plotsHash <- vector(mode="list", dim(pair)[2])
    for (i in 1:dim(pair)[2]) {
        ix <- pair[2,i]
        iy <- pair[1,i]

        tmpX <- which(pair[2,] == ix)
        shareX <- tmpX[tmpX != i]
        
        tmpY <- which(pair[1,] == iy)
        shareY <- tmpY[tmpY != i]
        
        plotsHash[[paste("x_",plots[i],sep="")]] <- plots[shareX]
        plotsHash[[paste("y_",plots[i],sep="")]] <- plots[shareY]
    } 

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
            busy <<- FALSE
            tcl('update', 'idletasks')
            ##assign("busy", FALSE, envir=parent.env(environment()))
        }
        
     
    }
    
    lapply(plots, function(p) {
               tcl(p, 'systembind', 'state', 'add',
                   c('zoomX', 'panX', 'zoomY', 'panY', 'deltaX', 'deltaY'),
                   synchronizeBindings)
           })


    ## forbidden states
    undoStateChanges <- function(W) {
        warning("showLabels, showScales, and swapAxes can not be changed for scatterplot matrix.")
        l_configure(W, showLabels=FALSE, showScales=FALSE, swapAxes=FALSE)
    }
    lapply(plots, function(p) {
               tcl(p, 'systembind', 'state', 'add',
                   c('showLabels', 'showScales', 'swapAxes'),
                   undoStateChanges)
           })

    ## beware undoStateChanges and synchronizeBindings from garbage collector
    callbackFunctions$state[[paste(child,"synchronize", sep="_")]] <- synchronizeBindings
    callbackFunctions$state[[paste(child,"undo", sep="_")]] <- undoStateChanges
    
    
    return(plots)
}
