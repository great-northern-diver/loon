layout_wrap_synchronizeSetting <- function(plots, child,
                                           connectedScales = "both",
                                           xrange, yrange,
                                           zoomX = 5/6, zoomY = 5/6, force = TRUE) {


    connectedScales <- switch(connectedScales,
                              "cross" = "both",
                              "row" = "y",
                              "column" = "x",
                              {
                                  connectedScales
                              })

    if(force) {
        # force scales
        forceScales(plots = plots,
                    xrange = xrange,
                    yrange = yrange,
                    connectedScales = connectedScales,
                    zoomX = zoomX,
                    zoomY = zoomY)
    }

    busy <- FALSE
    switch(connectedScales,
           "both" = {

               synchronizeXYBindings <- function(W) {
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
                              synchronizeXYBindings)
                      }
               )
               callbackFunctions$state[[paste(child,"synchronizeXY", sep="_")]] <- synchronizeXYBindings
           },
           "y" = {


               # fixed Y
               synchronizeYBindings <- function(W) {
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
                              synchronizeYBindings)
                      }
               )
               callbackFunctions$state[[paste(child,"synchronizeY", sep="_")]] <- synchronizeYBindings

           },
           "x" = {
               # fixed X
               synchronizeXBindings <- function(W) {
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
                              synchronizeXBindings)
                      }
               )
               callbackFunctions$state[[paste(child,"synchronizeX", sep="_")]] <- synchronizeXBindings
           },
           "none" = NULL)
}

layout_grid_synchronizeSetting <- function(plots, child,
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
    switch(connectedScales,
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
               callbackFunctions$state[[paste(child,"synchronizeBindings", sep="_")]] <- synchronizeBindings
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
               callbackFunctions$state[[paste(child,"synchronizeBindings", sep="_")]] <- synchronizeBindings
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
               callbackFunctions$state[[paste(child,"synchronizeBindings", sep="_")]] <- synchronizeBindings
           },
           {
               layout_wrap_synchronizeSetting(plots = plots,
                                              child = child,
                                              connectedScales = connectedScales,
                                              xrange, yrange,
                                              zoomX = zoomX,
                                              zoomY = zoomY,
                                              force = FALSE)
           }
    )
}

