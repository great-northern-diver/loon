
local({
    ## Tk geometry managers like grid and pack may be used
    ## to place the widgets

    ## With pack stack three scatterplots that share the same x on top
    ## and bind the zoomX and panX
    tt <- tktoplevel()
    tktitle(tt) <- "Using the pack geometry manager"
    
    attach(iris)
    p1 <- l_plot(parent=tt, x=Sepal.Width, y=Petal.Width, color=Species,
                 linkingGroup="iris", showLabels=FALSE)
    p2 <- l_plot(parent=tt, x=Sepal.Width, y=Petal.Length,
                 linkingGroup="iris", showLabels=FALSE)
    p3 <- l_plot(parent=tt, x=Sepal.Width, y=Sepal.Length,
                 linkingGroup="iris", showLabels=FALSE)
    s <- l_serialaxes(parent=tt, iris[,-5], linkingGroup="iris",
                             axesLayout="parallel")

    detach(iris)

    ## make the canvas resize to fairly small

    for (p in c(p1,p2,p3,s)) {
        tkconfigure(paste(p,".canvas", sep=''), width=200, height=100)
    }
    
    tkpack(p1, p2, p3, s, fill="both", expand=TRUE)
    
    
    ## Bind so that they show the same x range
    l_bind_state(p1, c("panX", "zoomX"), function(W)updateZoomPan(W))
    l_bind_state(p2, c("panX", "zoomX"), function(W)updateZoomPan(W))
    l_bind_state(p3, c("panX", "zoomX"), function(W)updateZoomPan(W))

    busy <- FALSE
    updateZoomPan <- function(widget) {
        if (!busy) {
            busy <- TRUE
            zoomX <- l_cget(widget, "zoomX")
            panX <- l_cget(widget, "panX")

            for (w in c(p1, p2, p3)) {
                l_configure(w, panX=panX, zoomX=zoomX, zoomY=zoomX)
            }
            
            busy <- FALSE
        }
    }
    

    readline("press the return key to continue: next scatterplot matrix with grid")
    
    tt2 <- tktoplevel()
    tktitle(tt2) <- "Scatterplot Maxtrix Using Grid"

    nvar <- 7
    pair <- combn(3:10, 2)

    ## create plots
    plots <- apply(pair, 2, function(col) {
        l_plot(parent=tt2, olive[,col[2]], olive[,col[1]], showLabels=FALSE)
    })

    ## resize the min canvas size
    sapply(plots, function(p) {
        tkconfigure(paste(p,'.canvas',sep=''), width=50, height=50)
    })

    ## grid layout
    apply(rbind(plots, pair-2), 2, function(col) {
        tkgrid(col[1], row=col[2], column=col[3], sticky="nesw")
    })
    
    ## Column and Row wheight such that the cells expand
    for (i in 1:(nvar+1)) {
        tkgrid.columnconfigure(tt2, i, weight = 1)
        tkgrid.rowconfigure(tt2, i, weight = 1)
    }
    
    ## Add Variable Label
    for (i in 3:10) {
        lab <- tklabel(tt2, text=names(olive)[i])
        tkgrid(lab, row = i - 2, column = i - 2)
    }

    ## Link Plots and specify color
    ## sync=push is cheaper
    sapply(plots, function(p)l_configure(p, linkingGroup="olive", sync="push"))

    l_configure(plots[1], color=olive$Area)
           
    
    
})
