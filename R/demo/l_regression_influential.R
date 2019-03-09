
local({
    ## Fit simple linear regression
    fit <- lm(Fertility~Infant.Mortality, data=swiss)

    ## Data
    p <- with(swiss, l_plot(Fertility~Infant.Mortality,
                            title='swiss data (least-squares)',
                            linkingGroup='swiss',
                            itemLabel=rownames(swiss)))

    ## layer fit
    xrng <- range(swiss$Infant.Mortality)
    yhat <- predict(fit, data.frame(Infant.Mortality=xrng))
    l_layer_line(p, x=xrng, y=yhat, linewidth=3, index="end")

    ## Fitted vs. Residuals
    pr <- l_plot(x=fit$fitted, y=fit$resid,
                 xlabel="Fitted values",
                 ylabel="Residuals",
                 title="Residuals vs. fitted values",
                 linkingGroup='swiss',
                 itemLabel=rownames(swiss))

    l_layer_line(pr, x=c(-25,100), y=c(0,0),
                 linewidth=3, color="gray80",
                 index="end")

    ## Influential Points
    plev <- l_plot(x=hatvalues(fit), y=rstudent(fit),
                   title="Leverage and outlier plot",
                   ylabel="Externally studentized residuals",
                   xlabel="hat values",
                   linkingGroup="swiss",
                   itemLabel=rownames(swiss))

    llev <- l_layer_line(plev, index='end',
                         x=rep(4/plev['n'],2), y=c(-5,5),
                         linewidth=3, color='gray80')


    ## Layer Confidence Intervals
    xpvals <- with(swiss, seq(from=min(Infant.Mortality),to=max(Infant.Mortality),
                              length.out=60))

    conf95 <- predict(fit, data.frame(Infant.Mortality=xpvals),
                      interval="confidence", level=0.95)

    conf99 <- predict(fit, data.frame(Infant.Mortality=xpvals),
                      interval="confidence", level=0.99)

    pred95 <- predict(fit, data.frame(Infant.Mortality=xpvals),
                      interval="prediction", level=0.95)

    pred99 <- predict(fit, data.frame(Infant.Mortality=xpvals),
                      interval="prediction", level=0.99)


    ## Interactively remove points from OLS
    use_color <- p['color'][1]

    l.sel <- l_layer_line(p, x=xrng, y=yhat, color='red', linewidth=3,
                          index="end")

    confg <- l_layer_group(p, label="Confidence intervals", index="end")
    predg <- l_layer_group(p, label="Prediction intervals", index="end")


    polyc95 <- l_layer_polygon(p,
                               x=c(xpvals,rev(xpvals)),
                               y=c(conf95[,2],rev(conf95[,3])),
                               color="lightblue2",
                               linecolor="",
                               parent=confg,
                               label="95% confidence",
                               index="end")

    polyc99 <- l_layer_polygon(p,
                               x=c(xpvals,rev(xpvals)),
                               y=c(conf99[,2],rev(conf99[,3])),
                               color="lightblue1",
                               linecolor="",
                               parent=confg,
                               label="99% confidence",
                               index="end")

    polyp95 <- l_layer_polygon(p,
                               x=c(xpvals,rev(xpvals)),
                               y=c(pred95[,2],rev(pred95[,3])),
                               color="lightpink2",
                               linecolor="",
                               parent=predg,
                               label="95% prediction",
                               index="end")

    polyp99 <- l_layer_polygon(p,
                               x=c(xpvals,rev(xpvals)),
                               y=c(pred99[,2],rev(pred99[,3])),
                               color="lightpink1",
                               linecolor="",
                               parent=predg,
                               label="99% prediction",
                               index="end")

    l_scaleto_world(p)


    updateRegression <- function() {
        ## which points to use for regression
        sel <- p['color'] == use_color
        sel <- sel & p['active']
        ## which coordinates to use for regression
        xnew <- p['xTemp']
        if (length(xnew) == 0) {
            xnew <- p['x']
        }

        ynew <- p['yTemp']
        if (length(ynew) == 0) {
            ynew <- p['y']
        }

        fit.temp <- lm(y~x, subset(data.frame(x=xnew, y=ynew), sel))

        xrng <- range(xnew)

        ## the fitted line
        yhat <- predict(fit.temp, data.frame(x=xrng))
        l_configure(l.sel, y=yhat, x=xrng)

        ## the intervals
        ##
        xpvals.temp <- seq(from=min(xrng),to=max(xrng),
                           length.out=60)

        conf95.temp <- predict(fit.temp, data.frame(x=xpvals.temp),
                               interval="confidence", level=0.95)

        conf99.temp <- predict(fit.temp, data.frame(x=xpvals.temp),
                               interval="confidence", level=0.99)

        pred95.temp <- predict(fit.temp, data.frame(x=xpvals.temp),
                               interval="prediction", level=0.95)

        pred99.temp <- predict(fit.temp, data.frame(x=xpvals.temp),
                               interval="prediction", level=0.99)

        ## update the prediction intervals
        ##
        l_configure(polyp99,
                    x=c(xpvals.temp,rev(xpvals.temp)),
                    y=c(pred99.temp[,2],rev(pred99.temp[,3])))

        l_configure(polyp95,
                    x=c(xpvals.temp,rev(xpvals.temp)),
                    y=c(pred95.temp[,2],rev(pred95.temp[,3])))

        ## update the confidence intervals
        ##
        l_configure(polyc99,
                    x=c(xpvals.temp,rev(xpvals.temp)),
                    y=c(conf99.temp[,2],rev(conf99.temp[,3])))

        l_configure(polyc95,
                    x=c(xpvals.temp,rev(xpvals.temp)),
                    y=c(conf95.temp[,2],rev(conf95.temp[,3])))


        #l_scaleto_world(p)

        ## resids versus fitted plot
        fitted <- predict(fit.temp, data.frame(x=xnew))
        l_configure(pr, x=fitted, y=ynew - fitted)

        ## leverage plot
        l_configure(plev, x=hatvalues(fit.temp), y=rstudent(fit.temp),
                    linkingKey=which(sel)-1, sync="pull")
        l_scaleto_world(plev)
        l_configure(llev, x=rep(4/sum(sel),2))
        tcl('update', 'idletasks')
    }

    bnd <- l_bind_state(p, c("color","active","xTemp","yTemp"),
                        function()updateRegression())

    ## Map

    if (requireNamespace("maps", quietly = TRUE)) {
        m <- maps::map("world", 'Switzerland', fill=TRUE, plot=FALSE)
        ## Coordinates
        swissCoords <- structure(list(
            latitude = c(47.1783274, 47.365837, 47.254872,
                         47.2782749, 47.0632023, 47.416647, 46.5892626, 46.761285, 46.6757485,
                         46.7866673, 46.5280339, 46.3190253, 46.4953291, 46.8806009, 46.6140944,
                         46.6410996, 46.8092091, 46.5196535, 46.4312213, 46.5, 46.5088127,
                         46.6698891, 46.3832683, 46.7293301, 46.566667, 46.8220266, 46.4666667,
                         46.4612971, 46.4628333, 46.7784736, 46.2244777, 46.0163423, 46.0801475,
                         46.1049798, 46.2521873, 46.214941, 46.2941311, 46.2331221, 46.9542921,
                         47.1034892, 47.0577195, 46.9899874, 47.083333, 46.9, 46.2043907,
                         46.2083126, 46.2455233),
            longitude = c(7.0729547, 7.3451555,
                          7.0028421, 7.3716656, 7.0912628, 7.0765657, 6.9555376, 7.0901001,
                          7.095521, 7.1621113, 6.9175828, 6.970566, 6.3918325, 7.0427075,
                          6.507171, 6.6344508, 6.6457678, 6.6322734, 6.9106799, 6.75, 6.4961301,
                          6.7975224, 6.2347852, 6.5323588, 6.833333, 6.9405663, 7.0833333,
                          6.3397549, 6.8419192, 6.641183, 7.303512, 7.2706464, 7.4698932,
                          7.0755334, 6.9469598, 7.0047948, 7.5335362, 7.360626, 6.8478409,
                          6.8327838, 6.7487354, 6.9292732, 6.966667, 6.6, 6.1431577, 6.1458643,
                          6.2090779)),
            .Names = c("latitude", "longitude"),
            row.names = c("Courtelary Switzerland",
                          "Delemont Switzerland", "Franches-Mnt Switzerland", "Moutier Switzerland",
                          "Neuveville Switzerland", "Porrentruy Switzerland", "Broye Switzerland",
                          "Glane Switzerland", "Gruyere Switzerland", "Sarine Switzerland",
                          "Veveyse Switzerland", "Aigle Switzerland", "Aubonne Switzerland",
                          "Avenches Switzerland", "Cossonay Switzerland", "Echallens Switzerland",
                          "Grandson Switzerland", "Lausanne Switzerland", "La Vallee Switzerland",
                          "Lavaux Switzerland", "Morges Switzerland", "Moudon Switzerland",
                          "Nyone Switzerland", "Orbe Switzerland", "Oron Switzerland",
                          "Payerne Switzerland", "Paysd'enhaut Switzerland", "Rolle Switzerland",
                          "Vevey Switzerland", "Yverdon Switzerland", "Conthey Switzerland",
                          "Entremont Switzerland", "Herens Switzerland", "Martigwy Switzerland",
                          "Monthey Switzerland", "St Maurice Switzerland", "Sierre Switzerland",
                          "Sion Switzerland", "Boudry Switzerland", "La Chauxdfnd Switzerland",
                          "Le Locle Switzerland", "Neuchatel Switzerland", "Val de Ruz Switzerland",
                          "ValdeTravers Switzerland", "V. De Geneve Switzerland", "Rive Droite Switzerland",
                          "Rive Gauche Switzerland"),
            class = "data.frame")


        p_map <- with(swissCoords, l_plot(longitude,latitude,
                                          itemLabel=rownames(swissCoords),
                                          showItemLabels=TRUE,
                                          linkingGroup='swiss'))

        l <- l_layer(p_map, m, col = "cornsilk", index=1)
        l_layer_lower(p_map, l)
        l_scaleto_world(p_map)
    }

})
