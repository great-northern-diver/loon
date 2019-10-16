

if (requireNamespace("maps", quietly = TRUE)) {
    local({
        ## Start with plot
        p_map <- l_plot()

        ## Maps from the map library
        canada <- maps::map("world",  "Canada", fill=TRUE, plot=FALSE)

        landcol <- "cornsilk"
        ## Add the map
        id <- l_layer(p_map, canada,
                      color = landcol,
                      asSingleLayer=FALSE,
                      index = "end")
        l_scaleto_layer(p_map, id)

        # Get the lakes:
        cdn.lakes <-  maps::map("lakes",
                            plot=FALSE,
                            fill=TRUE)$names[c(7,8,27, #great
                                               22, 25,
                                               68:73,
                                               82, 85
                                               )]


        # Add them
        lakecol ="lightblue"
        great.lakes.layer <-  l_layer(p_map,
                                      maps::map("lakes", cdn.lakes,
                                           plot=FALSE, fill=TRUE),
                                      label = "Great Lakes Water",
                                      color = lakecol,
                                      index="end"    # Add the layer at the end so we know
                                      # where it is
        )

        # Move the lake layer up to be above the land map
        l_layer_raise(p_map, great.lakes.layer)

        readline("press the return key to continue: next add points to scatterplot")

        canada.cities <- subset(maps::world.cities,
                                grepl("canada", country.etc , ignore.case=TRUE))

        with(canada.cities,
             l_configure(p_map, x=long, y=lat,  color = "red"))
        l_scaleto_world(p_map)

        readline("press the return key to continue: next add city name glyphs")

        g.t <- l_glyph_add_text(p_map, text=canada.cities$name)
        p_map['glyph'] <- g.t

    })
}
