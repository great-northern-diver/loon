
local({
    
    tt <- tktoplevel()
    p1 <- with(mtcars,
               l_plot(parent=tt, x=mpg, y=hp, color=gear, title="mtcars",
                             useLoonInspector=FALSE)
               )
    
    tkpack(p1, side="left", fill="both", expand=TRUE)
    
    
    ## Add a custom inspector (no layers)
    f <- tkframe(tt)
    lf <- tkwidget(f,"labelframe", text="Worldview")
    wv <- l_worldview(parent=lf, activewidget=p1)
    tkconfigure(paste(wv,".canvas", sep=""), width=50, height=160) 
    ai <- l_plot_inspector_analysis(parent=f, activewidget=p1)

    tkpack(f, side="right", anchor="ne")
    tkpack(lf, side="top", fill="x")
    tkpack(wv, side="top", fill="x")
    tkpack(ai, side="top", fill="x")
    tktitle(tt) <- "Custom Loon Plot"
    
})
