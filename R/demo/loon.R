local({
    
    p1 <- with(olive, l_plot(x=palmitoleic, y=stearic,
                             color=Region))
    
    p2 <- with(olive, l_plot(linoleic, oleic,
                             color=Area,
                             size=3,
                             linkingGroup="olive"))
    
    ## Automatic sync="pull" when initializing a plot
    ## with a linkingGroup
    p3 <- with(olive, l_plot(eicosenoic~arachidic,
                             linkingGroup="olive"))
    
    l_configure(p1, title="Olive Data", size=5)

    ## Layer Drawings
    l_layer_points(p1, seq(0,500,by=50), seq(100,280,length=11))

    l_layer_texts(p1, x=c(300, 400), y=c(480, 260),
                 text=c("Hello\nWorld", "This is\n text"))

    g <- l_layer_group(p1, label="Regression", index="end")

    fit <- lm(stearic~palmitoleic, data=olive)
    x <- range(olive$palmitoleic)
    y <- predict(fit,data.frame(palmitoleic=x))
    rl <- l_layer_line(p1, x, y, linewidth=3, label="fit")

    l_configure(rl, color="orange", linewidth=5)
    

    # prediction and confidence interval
    x <- with(olive, seq(from=min(palmitoleic),
                         to=max(palmitoleic),
                         length.out=60))

    
    prd <- predict(fit, data.frame(palmitoleic=x),
                   interval="confidence", level=0.95)
    l_layer_polygon(p1, x=c(x,rev(x)), y=c(prd[,2],rev(prd[,3])),
                    color="gray20", parent=g, index="end") 

    prd <- predict(fit, data.frame(palmitoleic=x),
                   interval="prediction", level=0.95)
    l_layer_polygon(p1, x=c(x,rev(x)), y=c(prd[,2],rev(prd[,3])),
                    parent=g,index="end")

        
})
