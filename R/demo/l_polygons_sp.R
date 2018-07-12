
if (requireNamespace("sp", quietly = TRUE)) {
    local({
        library(sp)
        # Taken from the introductory vignette of the "sp" package
        Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
        Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
        Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
        Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)
        Srs1 = Polygons(list(Sr1), "s1")
        Srs2 = Polygons(list(Sr2), "s2")
        Srs3 = Polygons(list(Sr3, Sr4), "s3 and 4")
        SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
        
        p <- l_plot()
        group1_layer  <- l_layer_group(p, label = "sp package: intro example")
        poly_layer <- l_layer(p, SpP, label = "Spatial polygons", 
                              parent = group1_layer,
                              color = c("black", "red", "green", "white"), 
                              linecolor = "black")
        l_scaleto_world(p)
        
        cat(
            paste(
                "\n\nThis shows an example of spatial polygons taken",
                "from the vignette \"intro_sp\" of the sp package.  \n",
                "All polygons appear together in one layer. \n",
                "In the inspector, select the  \"Layers\" tab to see the layers. \n\n"
            )
        )
        readline("press the return key to continue: points, and individual polygons on separate layers")    
        
        x <- c(0.5, 0.5, 1.0, 1.5)
        y <- c(1.5, 0.5, 0.5, 0.5)
        xy <- cbind(x,y)
        dimnames(xy)[[1]] <- c("a", "b", "c", "d")
        pts <- SpatialPoints(xy)
        xpol <- c(0,1,1,0,0)
        ypol <- c(0,0,1,1,0)
        pol1 <- Polygon(cbind(x = xpol-1.05, y = ypol))
        pol2 <- Polygon(cbind(xpol, ypol))
        pol3 <- Polygon(cbind(xpol, ypol - 1.0))
        pol4 <- Polygon(cbind(xpol + 1.0, ypol))
        pol5 <- Polygon(cbind(xpol+.4, ypol+.1))
        
        group2_layer <- l_layer_group(p, label = "sp package: overlay example")
        points_layer <- l_layer(p, pts, label = "Spatial points", 
                                parent = group2_layer,
                                color = c("red", "green", "blue", "steelblue"), 
                                linecolor = "black")
        
        group_polys_layer <- l_layer_group(p, label = "Separate polygon layers")
        l_layer_move(group_polys_layer,  parent = group2_layer)
        
        x1 <- l_layer(p, pol1, label = "x1", 
                      parent = group_polys_layer,
                      color = "paleturquoise",
                      linecolor = "black")
        
        x2 <- l_layer(p, pol2, label = "x2", 
                      parent = group_polys_layer,
                      color = "peachpuff",
                      linecolor = "black")
        
        x3 <- l_layer(p, pol3, label = "x3", 
                      parent = group_polys_layer,
                      color = "lightsteelblue",
                      linecolor = "black")
        
        x4 <- l_layer(p, pol4, label = "x4", 
                      parent = group_polys_layer,
                      color = "plum",
                      linecolor = "black")
        
        x5 <- l_layer(p, pol5, label = "x5", 
                      parent = group_polys_layer,
                      color = "palegreen",
                      linecolor = "black")
        
        
        l_scaleto_layer(p, group2_layer)
        l_layer_raise(p, points_layer)
        l_layer_hide(p, group1_layer)
        
        cat(
            paste(
                "\n\nPoints are in a single layer, each polygon is in its own separate layer. \n",
                "Example is taken from the vignette \"over\" of the sp package.  \n",
                "Use the inspector (\"Layers\" tab) to manipulate layers. \n\n"
            )
        )
        readline("press the return key to continue: Programmatically hide the leftmost polygon (x1)")  
        l_layer_hide(p, x1)
        readline("press the return key to continue: Programmatically show the leftmost polygon (x1)")  
        l_layer_show(p, x1)
        readline(paste0("press the return key to continue: Programmatically raise the middle polygon (x2) above the topmost (x5)\n",
                        "\t... N.B. requires 3 raises"))
        
        l_layer_raise(p, x2) 
        readline("press the return key to raise the middle polygon (x2) again ... ")
        l_layer_raise(p, x2)
        readline("press the return key to raise the middle polygon (x2) the final time ... ")
        l_layer_raise(p, x2)
        readline("press the return key to continue: Programmatically lower the topmost polygon (x5) below the rightmost (x4)")  
        l_layer_lower(p, x5)
        
        readline("press the return key to continue: Show all polygons and scale to world")  
        for (layer in list(group1_layer, poly_layer, group2_layer, points_layer, group_polys_layer, 
                        x1, x2, x3, x4, x5)
        ) {
            l_layer_show(p, layer)
        }
        l_layer_raise(p, x5)
        l_layer_lower(p, x2)
        l_layer_lower(p, x2)
        l_layer_lower(p, x2)
        l_scaleto_world(p)
    })
} else {
    cat(paste0("This demo is about drawing \"sp\" package polygons, etc. in loon. \n",
               "Please install.packages(\"sp\") before trying this demo. \n",
               "The demo's code can be found at: \n")
    )
    cat(paste0(' ',
               system.file('demo','l_polygons_sp.R', package = 'loon'),'\n'))
}


