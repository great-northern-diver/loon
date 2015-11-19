
local({
    p <- with(olive,
              l_plot(x=linoleic, y=oleic,
                     color=Region, title="Olive Data"))
    
    
    ## Layer a Group
    l.g <- l_layer_group(p, label="Drawings", parent="root", index="end")
    
    
    ## Layer Points
    readline("press the return key to continue: next add points layer")
    
    l.pts <- l_layer_points(p,
                            x=c(200, 450, 1800),
                            y=c(6000, 8000, 7000),
                            color=c("green", "orange", "lightblue"),
                            parent=l.g)
    
    l_scaleto_layer(p, l.pts)
    
    readline("press the return key to continue: next scale to world")
    
    l_scaleto_world(p)
    
    
    readline("press the return key to continue: next configure size and color")
    
    l_configure(l.pts, color="thistle", size=30)
    
    readline("press the return key to continue: next re-initialize points")
    
    l_configure(l.pts,
                x=seq(from=200,to=1600, length.out=20),
                y=seq(from=6000,to=8000, length.out=20),
                color="steelblue", size=20:39)
    
    
    readline("press the return key to continue: next re-label and move layer")
    
    l_layer_relabel(p, l.pts, "Different Sizes")
    l_layer_move(p, l.pts, parent="root")
    
    
    ## Polygon
    
    readline("press the return key to continue: next layer a polygon")
    
    i <- with(olive, chull(linoleic, oleic))
    
    x.hull <- olive$linoleic[i]
    y.hull <- olive$oleic[i]
    
    l_layer_polygon(p, x.hull, y.hull, color="thistle",
                    linecolor="black", linewidth=4, parent=l.g)
    

    ## Rectangle

    readline("press the return key to continue: next layer a rectangle")

    l_layer_rectangle(p, x=c(1100, 1300), y=c(7600, 8300), linewidth=2)
    
    ## Oval
    readline("press the return key to continue: next layer an oval")

    l_layer_oval(p, x=c(1500, 1750), y=c(7900, 8100),
                 color="", linecolor="orange", linewidth=4)
    
    ## Line
    readline("press the return key to continue: next layer a (regression) line and polygon")
    
    x <- with(olive, linoleic[Region=="North"])
    y <- with(olive, oleic[Region=="North"])
    
    fit <- lm(y~x)
    ##summary(fit)
    
    xr <- seq(from=min(x), to=max(x), length.out=20)
    yp <- predict(fit, data.frame(x=xr), interval="prediction")
    
    
    l.pi <- l_layer_polygon(p, x=c(xr, rev(xr)),
                            y=c(yp[,2],rev(yp[,3])),
                            color="lightgreen",
                            linecolor= "darkgreen", linewidth=2,
                            label="predition interval west liguria")
    
    l.fit <- l_layer_line(p, x=xr, y=yp[,1],
                          color="darkgreen", linewidth=8,
                          label="fit west liguria")
    
    l_layer_move(p, l.pi, "root", "end")
    l_layer_raise(p, l.pi)
    
    
    ## Text (size does not work and color is gray)
    
    readline("press the return key to continue: next layer text")
    
    bbox <- l_layer_bbox(p, "root")
    
    l_layer_texts(p, x=seq(from=bbox[1], to=bbox[3], length.out=length(LETTERS)),
                 y=rev(seq(from=bbox[2], to=bbox[4], length.out=length(LETTERS))),
                 text=LETTERS, size=seq_along(LETTERS),
                 angle=seq_along(LETTERS)/length(LETTERS)*360)

})
