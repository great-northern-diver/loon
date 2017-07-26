
local({
    pco2 <- l_plot(co2, title="Atmospheric carbon dioxide over Mauna Loa", 
                   ylabel="CO2 (ppm)", linkingGroup="co2", showScales=TRUE, showGuides=TRUE)
    
    xy.raw <- xy.coords(co2)
    l_layer_line(pco2, x= xy.raw$x, y= xy.raw$y, index="end", label="connect the dots")
    
    co2.stl <- stl(co2,"per")$time.series
    
    
    readline("Add a fitted trend to the data. Press the <enter> key to continue: ")
    
    xy.trend <- xy.coords(co2.stl[,"trend"])
    l_layer_line(pco2, x= xy.trend$x, y= xy.trend$y, color="brown", linewidth=3,index="end", label="trend")
    
    readline("Add a fitted seasonal component to the trend. Press the <enter> key to continue: ")
    
    xy.seasonal <- xy.coords(co2.stl[,"seasonal"])
    l_layer_line(pco2, x= xy.seasonal$x, y= xy.seasonal$y + xy.trend$y, color="green", linewidth=3,index="end", label="trend + seasonal")
    
    
    readline("Display a seasonal trend analysis (stl) in a single window. Press the <enter> key to continue: ")
    
    ## Tk geometry managers like grid and pack may be used
    ## to place the widgets
    
    ## With pack stack three scatterplots that share the same x on top
    ## and bind the zoomX and panX
    tt <- tktoplevel()
    tktitle(tt) <- "Seasonal Trend Analysis"
    
    p1 <- l_plot(parent=tt, x= co2, color="steelblue", size=1,
                 ylabel="CO2 data (ppm)", xlabel="Time",
                 title="Atmospheric carbon dioxide over Mauna Loa",
                 linkingGroup="co2", showScales=TRUE, showGuides=TRUE, showLabels=TRUE)
    l_layer_line(p1, x= xy.raw$x, y= xy.raw$y,  color="steelblue",
                 linewidth=1, index="end", label="connect the dots")
    
    
    p2 <- l_plot(parent=tt, x= xy.trend$x, y= xy.trend$y, color="steelblue", size=1,
                 ylabel="CO2 trend", xlabel="Time",
                 linkingGroup="co2", showScales=TRUE, showGuides=TRUE, showLabels=TRUE)
    l_layer_line(p2, x= xy.trend$x, y= xy.trend$y, color="brown",
                 linewidth=1,index="end", label="trend")
    
    
    p3 <- l_plot(parent=tt, x= xy.seasonal$x, y= xy.seasonal$y,color="steelblue", size=1,
                 ylabel="CO2 seasonality", xlabel="Time",
                 linkingGroup="co2", showScales=TRUE, showGuides=TRUE, showLabels=TRUE)
    l_layer_line(p3, x= xy.seasonal$x, y= xy.seasonal$y, color="green", 
                 linewidth=1,index="end", label="seasonal")
    
    xy.remainder <- xy.coords(co2.stl[,"remainder"])
    
    p4 <- l_plot(parent=tt, x= xy.remainder$x, y= xy.remainder$y,color="steelblue", size=1,
                 ylabel="CO2 remainder", xlabel="Time",
                 linkingGroup="co2", showScales=TRUE, showGuides=TRUE, showLabels=TRUE)
    l_layer_line(p4, x= xy.remainder$x, y= xy.remainder$y, color="grey",
                 linewidth=1,index="end", label="remainder")
    
    
    
    ## make the canvas resize to fairly small
    
    for (p in c(p1,p2,p3,p4)) {
        tkconfigure(paste(p,".canvas", sep=''), width=500, height=150)
    }
    
    tkpack(p1, p2, p3, p4, fill="both", expand=TRUE)
    
    
    ## Bind so that they show the same x range
    l_bind_state(p1, c("panX", "zoomX"), function(W)updateZoomPan(W))
    l_bind_state(p2, c("panX", "zoomX"), function(W)updateZoomPan(W))
    l_bind_state(p3, c("panX", "zoomX"), function(W)updateZoomPan(W))
    l_bind_state(p4, c("panX", "zoomX"), function(W)updateZoomPan(W))
    
    busy <- FALSE
    updateZoomPan <- function(widget) {
        if (!busy) {
            busy <- TRUE
            zoomX <- l_cget(widget, "zoomX")
            panX <- l_cget(widget, "panX")
            
            for (w in c(p1, p2, p3, p4)) {
                l_configure(w, panX=panX, zoomX=zoomX)
            }
            
            busy <- FALSE
        }
    }
    
    
    
    
    readline("Add the remainder's five number summary to all plots. Press the <enter> key to continue: ")
    
    five <- fivenum(xy.remainder$y)
    ymin <- five[1]
    q1   <- five[2]
    med  <- five[3]
    q3   <- five[4]
    ymax <- five[5]
    
    readline("Add lag (1-12) plots. Press the <enter> key to continue: ")
    
    tt2 <- tktoplevel()
    tktitle(tt2) <- "Lag (1-12) plots"
    
    nlags <- 12
    n <- length(xy.raw$y)
    
    ## create plots
    plots <- vector("list",nlags)
    for (i in 1:nlags){
        plots[[i]] <- l_plot(parent=tt2, 
                             x=xy.raw$y[1:(n-i)],
                             y=xy.raw$y[-(1:i)],
                             showLabels=TRUE,
                             showGuides=TRUE,
                             linkingGroup="co2",
                             ylabel="CO2",
                             xlabel=paste("CO2(-",i,")",sep=""),
                             title="")
        
    }
    
    ## resize the min canvas size
    sapply(plots, function(p) {
        tkconfigure(paste(p,'.canvas',sep=''), width=200, height=200)
    })
    
    ## grid layout
    nrow <- floor(sqrt(nlags))
    ncol <- ceiling(sqrt(nlags))
    for (i in 1:nrow){
        for (j in 1:ncol){
            index <- (i-1)*ncol +  j
            tkgrid(plots[[index]], 
                   row=i, column=j, 
                   sticky="nesw"
            )
        }
    }    
    
    ## Column and Row weight such that the cells expand
    for (i in 1:ncol) {
        tkgrid.columnconfigure(tt2, i, weight = 1)
    }
    for (i in 1:nrow) {
        tkgrid.rowconfigure(tt2, i, weight = 1)
    }
    
})


           
    
    

