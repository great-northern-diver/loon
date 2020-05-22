layout_synchronizeSetting <- function(plots, child,
                                      connectedScales = "cross",
                                      xrange, yrange, zoomX = 5/6, zoomY = 5/6, force = TRUE) {

    if(force) {
        # force scales
        forceScales(plots = plots,
                    xrange = xrange,
                    yrange = yrange,
                    zoomX = zoomX,
                    zoomY = zoomY)
    }

    if(connectedScales == "none") return(NULL)

    layout_position <- layout_position(plots)
    plotsHash <- list()
    for (i in 1:length(plots)) {

        tmpX <- which(layout_position[, "y"] == layout_position[i, "y"])
        shareX <- tmpX[tmpX != i]

        tmpY <- which(layout_position[, "x"] == layout_position[i, "x"])
        shareY <- tmpY[tmpY != i]
        plotsHash[[paste("scatter_x_",
                         plots[[i]],
                         sep="")]] <- plots[shareX]
        plotsHash[[paste("scatter_y_",
                         plots[[i]],
                         sep="")]] <- plots[shareY]
    }

    busy <- FALSE
    switch(
        connectedScales,
        "cross" = {
            synchronizeBindings <- function(W) {
                if (!busy) {
                    busy <<- TRUE
                    class(W) <- "loon"
                    zoomX <- W['zoomX']
                    panX <- W['panX']
                    deltaX <- W['deltaX']

                    lapply(plotsHash[[paste("scatter_x_", W, sep="")]], function(p) {
                        l_configure(p, zoomX=zoomX, panX=panX, deltaX=deltaX)
                    })

                    zoomY <- W['zoomY']
                    panY <- W['panY']
                    deltaY <- W['deltaY']

                    lapply(plotsHash[[paste("scatter_y_",W,sep="")]], function(p) {
                        l_configure(p, zoomY=zoomY, panY=panY, deltaY=deltaY)
                    })
                    busy <<- FALSE
                    tcl('update', 'idletasks')
                }
            }

            lapply(plots,
                   function(p) {
                       tcl(p, 'systembind', 'state', 'add',
                           c('zoomX', 'panX', 'zoomY', 'panY', 'deltaX', 'deltaY'),
                           synchronizeBindings)
                   }
            )
        },
        "row" = {
            synchronizeBindings <- function(W) {
                if (!busy) {
                    busy <<- TRUE
                    class(W) <- "loon"
                    zoomY <- W['zoomY']
                    panY <- W['panY']
                    deltaY <- W['deltaY']
                    zoomX <- W['zoomX']
                    panX <- W['panX']
                    deltaX <- W['deltaX']

                    lapply(plotsHash[[paste("scatter_y_",W,sep="")]], function(p) {
                        l_configure(p, zoomY=zoomY, panY=panY, deltaY=deltaY,
                                    zoomX=zoomX, panX=panX, deltaX=deltaX)
                    })
                    busy <<- FALSE
                    tcl('update', 'idletasks')
                }
            }
            lapply(plots,
                   function(p) {
                       tcl(p, 'systembind', 'state', 'add',
                           c('zoomY', 'panY', 'deltaY', 'zoomX', 'panX', 'deltaX'),
                           synchronizeBindings)
                   }
            )
        },
        "column" = {
            synchronizeBindings <- function(W) {
                if (!busy) {
                    busy <<- TRUE
                    class(W) <- "loon"
                    zoomY <- W['zoomY']
                    panY <- W['panY']
                    deltaY <- W['deltaY']
                    zoomX <- W['zoomX']
                    panX <- W['panX']
                    deltaX <- W['deltaX']

                    lapply(plotsHash[[paste("scatter_x_",W,sep="")]], function(p) {
                        l_configure(p, zoomY=zoomY, panY=panY, deltaY=deltaY,
                                    zoomX=zoomX, panX=panX, deltaX=deltaX)
                    })
                    busy <<- FALSE
                    tcl('update', 'idletasks')
                }
            }
            lapply(plots,
                   function(p) {
                       tcl(p, 'systembind', 'state', 'add',
                           c('zoomY', 'panY', 'deltaY', 'zoomX', 'panX', 'deltaX'),
                           synchronizeBindings)
                   }
            )
        },
        "both" = {

            synchronizeBindings <- function(W) {
                if (!busy) {
                    busy <<- TRUE
                    class(W) <- "loon"
                    zoomX <- W['zoomX']
                    panX <- W['panX']
                    deltaX <- W['deltaX']

                    lapply(plots,
                           function(p) {
                               l_configure(p, zoomX=zoomX, panX=panX, deltaX=deltaX)
                           })

                    zoomY <- W['zoomY']
                    panY <- W['panY']
                    deltaY <- W['deltaY']
                    lapply(plots,
                           function(p) {
                               l_configure(p, zoomY=zoomY, panY=panY, deltaY=deltaY)
                           })
                    busy <<- FALSE
                    tcl('update', 'idletasks')
                }
            }

            lapply(plots,
                   function(p) {
                       tcl(p, 'systembind', 'state', 'add',
                           c('zoomX', 'panX', 'zoomY', 'panY', 'deltaX', 'deltaY'),
                           synchronizeBindings)
                   }
            )
        },
        "y" = {


            # fixed Y
            synchronizeBindings <- function(W) {
                if (!busy) {
                    busy <<- TRUE
                    class(W) <- "loon"
                    zoomY <- W['zoomY']
                    panY <- W['panY']
                    deltaY <- W['deltaY']
                    lapply(plots,
                           function(p) {
                               l_configure(p, zoomY=zoomY, panY=panY, deltaY=deltaY)
                           })
                    busy <<- FALSE
                    tcl('update', 'idletasks')
                }
            }

            lapply(plots,
                   function(p) {
                       tcl(p, 'systembind', 'state', 'add',
                           c('zoomY', 'panY', 'deltaY'),
                           synchronizeBindings)
                   }
            )

        },
        "x" = {
            # fixed X
            synchronizeBindings <- function(W) {
                if (!busy) {
                    busy <<- TRUE
                    class(W) <- "loon"
                    zoomX <- W['zoomX']
                    panX <- W['panX']
                    deltaX <- W['deltaX']

                    lapply(plots,
                           function(p) {
                               l_configure(p, zoomX=zoomX, panX=panX, deltaX=deltaX)
                           })
                    busy <<- FALSE
                    tcl('update', 'idletasks')
                }
            }

            lapply(plots,
                   function(p) {
                       tcl(p, 'systembind', 'state', 'add',
                           c('zoomX', 'panX', 'deltaX'),
                           synchronizeBindings)
                   }
            )

        }
    )
    callbackFunctions$state[[paste(child,"synchronizeBindings", sep="_")]] <- synchronizeBindings
}

