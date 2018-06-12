

if (FALSE && requireNamespace("sp", quietly = TRUE)) {
    local({
        con <- url("http://biogeo.ucdavis.edu/data/gadm2/R/CHE_adm0.RData")
        load(con)
        close(con)
        
        p <- l_plot()
        g <- l_layer_group(p, label="Switzerland")
        m <- l_layer(p, gadm, label="Switzerland", parent=g,
                     color="", linecolor="black")
        l_scaleto_world(p)
        
        readline("press the return key to continue: map with multiple layers")    
        
        l_layer_hide(p, g)
        
        g1 <- l_layer_group(p, label="Swiss Cantons")
        
        con <- url("http://biogeo.ucdavis.edu/data/gadm2/R/CHE_adm1.RData")
        load(con)
        close(con)
        
        m1 <- l_layer(p, gadm, label="Swiss Cantons", parent=g1,
                      color="", linecolor="red")
        
        #    ## name the layers
        cantons <- gadm@data$NAME_1[gadm@plotOrder]
        
        for (i in 1:length(m1)) {
            sapply(m1[[i]], function(l)l_layer_relabel(p, l, cantons[i]))
        }
        ## l_aspect(p) <- 5/3
    })
} else {
    cat(paste0('loon demo l_map_sp should be evaluated by hand\nFind the demo file at: ',
               system.file('demo','l_map_sp.R', package='loon'),'\n'))
}