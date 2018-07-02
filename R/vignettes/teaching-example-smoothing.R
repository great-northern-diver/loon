## ----knitr_setup, include=FALSE------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----setup, warning=FALSE, message=FALSE, error=FALSE--------------------
library(loon)

## ----loadElemStatLearn, eval=FALSE---------------------------------------
#  library("ElemStatLearn")
#  summary(bone)

## ----scatterplot, eval=FALSE---------------------------------------------
#  # The plot
#  x <- bone$age
#  y <- bone$spnbmd
#  #  A scatterplot
#  p <- l_plot(x, y,
#              color="darkgrey",
#              xlabel="age", ylabel="spnbmd",
#              showGuides = TRUE, showScales = TRUE,
#              itemLabel = paste0("IDnum = ", bone$idnum, "\n",
#                                 bone$gender, "\n",
#                                 "Age: ", bone$age),
#              showItemLabels = TRUE,
#              linkingGroup="Bone density",
#              title = "Spinal bone mineral density (spnbmd)")

## ----changeVals, eval=FALSE----------------------------------------------
#  p["showItemLabels"]
#  p["showGuides"]
#  p["swapAxes"] <- TRUE
#  p["swapAxes"] <- FALSE

## ----addAxis, eval=FALSE-------------------------------------------------
#  axis <- l_layer_line(p,
#                       x=extendrange(x, f=0.5), y=c(0,0),
#                       label="axis", linewidth=2,
#                       color = "black",
#                       dash=c(10,10),
#                       index="end")   # last argument places axis behind other layers
#  

## ----histogram, eval=FALSE-----------------------------------------------
#  h <- l_hist(as.numeric(bone$gender), # note as.numeric(...)
#              binwidth = 1, showBinHandle = FALSE,
#              showStackedColors = TRUE,
#              xlabel = "gender",
#              linkingGroup="Bone density",
#              title = "Gender (female=1; male=2)"
#              )

## ----second plot, eval=FALSE---------------------------------------------
#  p2 <- l_plot(bone$idnum, bone$age,
#               xlabel="idnum", ylabel="age",
#               linkingGroup="Bone density",
#               title = "ID numbers and age")

## ----smooth, eval=FALSE--------------------------------------------------
#  library(splines)
#  # Fit a smoothing spline
#  fitsmooth <- smooth.spline(x, y, df=5)
#  xOrder <- order(x)
#  smooth <- l_layer_line(p,
#                         x=x[xOrder],
#                         y=predict(fitsmooth,x=x[xOrder])$y,
#                         label="smooth fit", linewidth=4,
#                         color = "blue")

## ----update smooth, eval=FALSE-------------------------------------------
#  
#  ## Define the update function
#  updateSmooth <- function(myPlot, minpts, df, color="blue") {
#    ## Get the values for x and y from the plot
#    ##
#    ## For x
#    xnew <- myPlot['xTemp']
#    if (length(xnew) == 0) {xnew <- myPlot['x']}
#  
#    ## For y
#    ynew <- myPlot['yTemp']
#    if (length(ynew) == 0) {ynew <- myPlot['y']}
#  
#    ## Now **only** use the active selected points to construct the smooth
#    sel <- myPlot['selected'] & myPlot['active']
#    xnew <- xnew[sel]
#    ynew <- ynew[sel]
#    Nsel <- sum(sel)
#  
#    if (Nsel > 3 & diff(range(xnew)) > 0) {
#      ## Find the range of the selected x values
#      xrng <- extendrange(xnew)
#      xvals.temp <- seq(from=min(xrng),
#                        to=max(xrng),
#                        length.out=100)
#  
#      ## Redo our smooth **only** if we have enough points
#      if ((Nsel > minpts) & (minpts > (df + 1))){
#        fit.temp <- smooth.spline(xnew, ynew, df=df)
#        ypred.temp <- predict(fit.temp,x=xvals.temp)$y
#        ## update the smooth
#        if (smooth %in% l_layer_ids(myPlot)) {
#          ## reconfigure the smooth with new data
#          l_configure(smooth, x=xvals.temp, y=ypred.temp)
#        } else {
#          ## If the smooth has been deleted, then we recreate it
#          ## (N.B. in the global environment)
#          smooth <<-  l_layer_line(myPlot,
#                                   x=xvals.temp,
#                                   y=ypred.temp,
#                                   label="smooth fit",
#                                   linewidth=4,
#                                   color = color)
#        }
#      }
#    }
#    ## Update the tcl language's event handler
#    tcl('update', 'idletasks')
#  }

## ----bindSmooth, eval=FALSE----------------------------------------------
#  # Here we "bind" the anonymous to the named state changes of p
#  l_bind_state(p, c("selected"),
#               function() {updateSmooth(p, 10, 5, "blue")}
#  )

## ----Gaussian weights, eval=FALSE----------------------------------------
#  GaussWt <- function(x) {
#    # Get an estimated standard deviation
#    h <- diff(range(x))/4
#    # Centre at median
#    xloc <- median(x)
#    # Gaussian density
#    dnorm(x, mean=xloc, sd=h)
#  }

## ----locally fitted line, eval = FALSE-----------------------------------
#  # Fit a local line using some Gaussian weights.
#  # Prediction will be at the median of x, fit by
#  ### weights that decrease with x's
#  # distance from the median.
#  fitwls <- lm(y ~ x, weights=GaussWt(x))
#  linewls <- l_layer_line(p,
#                          x=x,
#                          y=predict(fitwls,
#                                    newdata=data.frame(x=x)),
#                          label="Fitted line",
#                          linewidth=4,
#                          color = "blue")

## ----hide layer, eval=FALSE----------------------------------------------
#  l_layer_hide(p, smooth)

## ----update line, eval=FALSE---------------------------------------------
#  updateLocalLine <- function(myPlot, minpts, df, volor="blue") {
#    ## Get the values for x and y from the plot
#    ## For x
#    xnew <- myPlot['xTemp']
#    if (length(xnew) == 0) {xnew <- myPlot['x']}
#  
#    ## For y
#    ynew <- myPlot['yTemp']
#    if (length(ynew) == 0) {ynew <- myPlot['y']}
#  
#    ## Now **only** use the active selected points to construct the smooth
#    sel <- myPlot['selected'] & myPlot['active']
#    xnew <- xnew[sel]
#    ynew <- ynew[sel]
#    Nsel <- sum(sel)
#    if (Nsel > 3 & diff(range(xnew)) > 0) {
#      xrng <- extendrange(xnew)
#      xvals.temp <- seq(from=min(xrng),
#                        to=max(xrng),
#                        length.out=100)
#      ## Redo line if more than two points.
#      if (Nsel> 2) {
#        fit.wls <-  lm(ynew ~ xnew, weights=GaussWt(xnew))
#        ywls.temp <- predict(fit.wls,
#                             newdata=data.frame(xnew=xvals.temp))
#        ## update the fit
#        if (linewls %in% l_layer_ids(myPlot)) {
#          l_configure(linewls, x=xvals.temp, y=ywls.temp)
#        } else {
#          ## If it's been deleted, we recreate it (in the global environment).
#          linewls <<- l_layer_line(myPlot,
#                                   x=xvals.temp,
#                                   y=predict(fitwls,
#                                             newdata=data.frame(x=xvals.temp)
#                                   ),
#                                   label="GaussWt at median line",
#                                   linewidth=4,
#                                   color="blue"
#          )
#        }
#      }
#  
#    }
#    ## Update the tcl language's event handler
#    tcl('update', 'idletasks')
#  }

## ----bindLine, eval=FALSE------------------------------------------------
#  # Here we "bind" the anonymous to the named state changes of p
#  l_bind_state(p, c("active","selected"),
#               function() {updateLocalLine(p, 10, 5, "blue")}
#  )

