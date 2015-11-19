
require(maps) || stop("package maps needed.")

local({

    ## Maps from the map library
    canada <- map("world",  "Canada", fill=TRUE, plot=FALSE)
    pmap <- l_plot()

    ## Color the lakes
    id <- l_layer(pmap, canada,
                  color = ifelse(grepl("lake", canada$names,
                      ignore.case=TRUE), "lightblue", ""), asSingleLayer=FALSE)
    l_scaleto_layer(pmap, id)

    ## Or manually
    ##l_layer(pmap, target='layer69', color="lightblue")  # Lake Winnipeg
    ##l_layer(pmap, target='layer33', color="lightblue")  # Great Slave Lake
    ##l_layer(pmap, target='layer15', color="lightblue")  # Great Bear Lake
      
    readline("press the return key to continue: next add points to scatterplot")

    data(world.cities)
    canada.cities <- subset(world.cities,
                            grepl("canada", country.etc , ignore.case=TRUE))
    
    with(canada.cities,
         l_configure(pmap, x=long, y=lat))
    l_scaleto_world(pmap)

    readline("press the return key to continue: next add city name glyphs")
    
    g.t <- l_glyph_add_text(pmap, text=canada.cities$name)
    pmap['glyph'] <- g.t
    
})
