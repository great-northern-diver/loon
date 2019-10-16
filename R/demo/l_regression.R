
local({
    p1 <- with(faithful, 
               l_plot(x=eruptions, y=waiting,
                      size=10,
                      xlabel="Duration of eruption (minutes)",
                      ylabel="Waiting time to next eruption (minutes)",
                      title="Old faithful geyser",
                      showGuides=TRUE
               )
    )
    
    ## Add the least squares fitted line
    
    fit <- lm(waiting~eruptions, data=faithful)
    
    fitx <- range(faithful$eruptions)
    xdelta <- diff(fitx)/5
    xprange <- fitx + c(-xdelta,xdelta)
    fity <- predict(fit,data.frame(eruptions=xprange))
    
    l <- l_layer_line(p1, xprange, fity, color="black", 
                      linewidth=2, label="least squares fitted line")
    
    
    ## confidence intervals for the mean
    
    xpvals <- seq(from=min(xprange),to=max(xprange),
                  length.out=60)
    
    
    conf90 <- predict(fit, data.frame(eruptions=xpvals),
                      interval="confidence", level=0.90)
    conf95 <- predict(fit, data.frame(eruptions=xpvals),
                      interval="confidence", level=0.95)
    conf99 <- predict(fit, data.frame(eruptions=xpvals),
                      interval="confidence", level=0.99)
    
    confg <- l_layer_group(p1, label="Confidence intervals", index="end")
    
    l_layer_polygon(p1, 
                    x=c(xpvals,rev(xpvals)), 
                    y=c(conf90[,2],rev(conf90[,3])),
                    color="lightblue3", 
                    parent=confg, 
                    label="90% confidence",
                    index="end") 
    
    l_layer_polygon(p1, 
                    x=c(xpvals,rev(xpvals)), 
                    y=c(conf95[,2],rev(conf95[,3])),
                    color="lightblue2", 
                    parent=confg, 
                    label="95% confidence",
                    index="end") 
    
    l_layer_polygon(p1, 
                    x=c(xpvals,rev(xpvals)), 
                    y=c(conf99[,2],rev(conf99[,3])),
                    color="lightblue1", 
                    parent=confg, 
                    label="99% confidence",
                    index="end") 
    
    
    
    ## prediction intervals for observations
    
    pred90 <- predict(fit, data.frame(eruptions=xpvals),
                      interval="prediction", level=0.90)
    pred95 <- predict(fit, data.frame(eruptions=xpvals),
                      interval="prediction", level=0.95)
    pred99 <- predict(fit, data.frame(eruptions=xpvals),
                      interval="prediction", level=0.99)
    
    predg <- l_layer_group(p1, label="prediction intervals", index="end")
    
    l_layer_polygon(p1, 
                    x=c(xpvals,rev(xpvals)), 
                    y=c(pred90[,2],rev(pred90[,3])),
                    color="lightpink3", 
                    parent=predg, 
                    label="90% prediction",
                    index="end") 
    
    l_layer_polygon(p1, 
                    x=c(xpvals,rev(xpvals)), 
                    y=c(pred95[,2],rev(pred95[,3])),
                    color="lightpink2", 
                    parent=predg, 
                    label="95% prediction",
                    index="end") 
    
    l_layer_polygon(p1, 
                    x=c(xpvals,rev(xpvals)), 
                    y=c(pred99[,2],rev(pred99[,3])),
                    color="lightpink1", 
                    parent=predg, 
                    label="99% prediction",
                    index="end") 
    
    
    #
    # Plot residuals versus fit
    
    p2 <-  l_plot(x=fit$fitted, y=fit$resid,
                  size=10,
                  xlabel="Fitted values",
                  ylabel="Residuals",
                  title="Old faithful geyser: least squares fit",
                  showGuides=TRUE,
                  showScales=TRUE,
                  linkingGroup="Old faithful"
    )
    # Add a horizontal line at zero residual
    fxrange <- range(fit$fitted)
    fxdelta <- diff(fxrange)/5
    fxrange <- fxrange + c(-fxdelta,fxdelta)
    l_layer_line(p2, fxrange, rep(0,length(fxrange)), color="black", 
                 linewidth=2, label="zero line",index="end")
    
    #
    # Add p1 to this linking group, pull the scatterplot display values from the group
    #
    
    l_configure(p1, linkingGroup="Old faithful", sync="pull")
    
    
    #
    # normal qq plot of residuals with 
    # pointwise intervals simulated from normal
    #
    
    n <- length(fit$resid)
    
    frac <- ((1:n)-0.5)/n
    quantiles <- qnorm(frac)
    
    p3 <-  l_plot(x=quantiles, y=sort(fit$resid),
                  size=10,
                  xlabel="Gaussian quantiles",
                  ylabel="Ordered residuals",
                  title="Old faithful geyser: qqplot, as in qqtest pkg",
                  showGuides=TRUE,
                  linkingGroup="Old faithful",
                  linkingKey=order(fit$resid)-1   #default linking map is zero based
    )
    
    #
    # Rest is just to add simulated pointwise intervals (stolen from qqtest package in R)
    #
    nreps <- 100
    reps <- rnorm(n * nreps)
    # Get a reasonable location and scale from the qqplot
    qql <- MASS::rlm(sort(fit$resid) ~ 1 + quantiles, psi = MASS::psi.bisquare, 
                     method = "MM")
    loc <- qql$coefficients[1]
    scale <- qql$coefficients[2]
    reps <- loc + scale * reps
    reps <- array(reps, dim = c(nreps, n))
    reps <- apply(reps, 1, sort)
    
    #
    # What central percents do we want to offer on the layers?
    #
    centralPercents <- c(0.90, 0.95, 0.99)
    
    nLevels <- length(centralPercents)
    centralPercents <- sort(centralPercents)
    SymmetricAdjust <- (1 - centralPercents)/2
    bottomPcts <- (1 - centralPercents) - SymmetricAdjust
    topPcts <- centralPercents + SymmetricAdjust
    Pcts <- c(rev(bottomPcts), topPcts)
    NumsPct <- apply(reps, 1, function(x) { quantile(x, Pcts)})
    
    #
    # line characteristics and adding lines
    #
    
    lineCols <- c("grey80", "grey50", "grey10")
    lwidth <- rep(2, nLevels)
    
    #i <- 0
    
    for (i in 1:nLevels) {
        # i <- i+1
        actualPercent <- centralPercents[nLevels-i+1]
        layer_group_name <- paste("Simulated ",
                                  round(100*actualPercent),
                                  "% pointwise central intervals",
                                  sep="")
        
        layer_group <- l_layer_group(p3, label=layer_group_name, index="end")
        
        dashPattern <- c(15*i,5)
        
        l_layer_line(p3, quantiles, NumsPct[i, ], 
                     color=lineCols[i], 
                     linewidth=lwidth[i], dash=dashPattern,
                     label=paste("lower ", round(100*actualPercent),"% limit", sep=""),
                     parent=layer_group,
                     index="end"
        )
        
        l_layer_line(p3, quantiles, NumsPct[2 * nLevels - i + 1, ], 
                     color=lineCols[i], 
                     linewidth=lwidth[i], dash=dashPattern,
                     label=paste("upper ", round(100*actualPercent),"% limit", sep=""),
                     parent=layer_group,
                     index="end"
        )
        
    }
    
    
    l_layer_texts(p3, x=c(-2,2), y=quantile(fit$resid, c(0.1,0.9)),
                  text=c("Residuals appear to\n have a short left tail", 
                         "Residuals appear to\n have a Gaussian right tail"),
                  color="steelblue", angle=45, size=6)
    
    
    
    #
    # Get waiting time and duration matching on the same eruption
    
    faithful2 <- data.frame(eruptions=faithful$eruptions[-n], 
                            waiting=faithful$waiting[-1],
                            groups=rep(1,(n-1))
    )
    
    #
    # Groups defined by Azzalini and Bowman paper in Applied Statistics, 1990
    #
    faithful2$groups[faithful2$eruptions >= 3 & faithful2$waiting >= 68] <- 2
    faithful2$groups[faithful2$eruptions < 3 & faithful2$waiting >= 68] <- 3
    faithful2$groups[faithful2$eruptions < 3 & faithful2$waiting < 68] <- 4
    
    p4 <- with(faithful2, 
               l_plot(x=eruptions, 
                      y=waiting,
                      size=10,
                      xlabel="Duration of eruption (minutes)",
                      ylabel="Waiting time to this eruption (minutes)",
                      title="Plot used by Azzalini and Bowman, 1990, to define groups",
                      color=groups,
                      showGuides=TRUE,
                      linkingGroup="Old faithful",
                      sync="push"
               )
    )
})


