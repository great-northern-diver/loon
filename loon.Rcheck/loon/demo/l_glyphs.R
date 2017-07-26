
require(PairViz) || stop("Package PairViz is required.")

local({
    p <- with(olive, l_plot(oleic~arachidic, color=Area))

    ## Text
    readline("press the return key to continue: next are text glyphs")

    gt <- l_glyph_add_text(p, text=as.character(olive$Area))
    p['glyph'] <- gt
    
    ## Images
    
    if (loon:::.withTclImg) {
        
        readline("press the return key to continue: next are image glyphs")
        
        path <- file.path(find.package(package = "loon"), "images")
        files <- list.files(path, full.names=TRUE)
        imgs <- l_image_import_files(files)
        names(imgs) <- gsub("\\.png$", "", basename(names(imgs)))
        area <-  gsub("^.*-", "", as.character(olive$Area))
        areaimages <- imgs[match(area, names(imgs))]
        gi <- l_glyph_add_image(p, images=areaimages)
        p['glyph'] <- gi
        
        
        readline("press the return key to continue: next reuse single image")
        
        l_configure(c(p,gi), images=areaimages[1])
        ## or also just
        ## l_configure(gi, images=areaimages[1])
    }

    
    ## Serialaxes
    readline("press the return key to continue: next star glyphs")

    sa <- l_glyph_add_serialaxes(p, data=oliveAcids)
    l_configure(p, glyph=sa)
    
    readline("press the return key to continue: next configure stars")

    l_configure(sa, showEnclosing=FALSE, linewidth=2)
    l_configure(sa, showArea=FALSE)
    
    readline("press the return key to continue: next stack all Umbria glyphs")

    p['selected'] <- olive$Area == "Umbria"
    l_move_valign(p,'selected')
    l_move_halign(p,'selected')
    l_configure(p, size=40, which='selected')
    l_scaleto_selected(p)
    l_configure(sa, showEnclosing=TRUE, bboxColor="steelblue", showArea=FALSE)
    p['selected'] <- FALSE

    readline("press the return key to continue: next parallel coordinates")

    l_move_reset(p)
    p['size'] <- 4
    l_scaleto_world(p)
    l_configure(sa, axesLayout="parallel")

    readline("press the return key to continue: next change the sequence")

    l_configure(sa, sequence=as.vector(t(hpaths(names(olive)[-c(1,2)]))))
    
    readline("press the return key to continue: next show axes")

    l_configure(sa, showAxes=TRUE)
    
    readline("press the return key to continue: switch back to star glyphs")
    
    l_configure(sa, axesLayout="radial", showAxes=FALSE, showEnclosing=FALSE)
    

    ## Polygon Glyphs
    readline("press the return key to continue: add polygon glyphs")
    
    x_star <- 
        c(-0.000864304235090734, 0.292999135695765, 0.949870354364736, 
          0.474503025064823, 0.586862575626621, -0.000864304235090734, 
          -0.586430423509075, -0.474070872947277, -0.949438202247191, -0.29256698357822)
    y_star <-
        c(-1, -0.403630077787381, -0.308556611927398, 0.153846153846154, 
          0.808556611927398, 0.499567847882455, 0.808556611927398, 0.153846153846154, 
          -0.308556611927398, -0.403630077787381)
    x_cross <- 
        c(-0.258931143762604, -0.258931143762604, -0.950374531835206, 
          -0.950374531835206, -0.258931143762604, -0.258931143762604, 0.259651397291847, 
          0.259651397291847, 0.948934024776722, 0.948934024776722, 0.259651397291847, 
          0.259651397291847)
    y_cross <-
        c(-0.950374531835206, -0.258931143762604, -0.258931143762604, 
          0.259651397291847, 0.259651397291847, 0.948934024776722, 0.948934024776722, 
          0.259651397291847, 0.259651397291847, -0.258931143762604, -0.258931143762604, 
          -0.950374531835206)
    x_hexagon <-
        c(0.773552290406223, 0, -0.773552290406223, -0.773552290406223, 
          0, 0.773552290406223)
    y_hexagon <- 
        c(0.446917314894843, 0.894194756554307, 0.446917314894843, -0.447637568424085, 
          -0.892754249495822, -0.447637568424085)
    
    
    x_polygon_glyph <- lapply(as.character(olive$Region), function(region) {
        if (region == "North") 
            x_cross
        else if (region == "South")
            x_star
        else
            x_hexagon
    })    
    
    y_polygon_glyph <- lapply(as.character(olive$Region), function(region) {
        if (region == "North") 
            y_cross
        else if (region == "South")
            y_star
        else
            y_hexagon
    })    
    
    gl_pol <- l_glyph_add_polygon(p, x=x_polygon_glyph, y=y_polygon_glyph, label="polygon")
    
    p['glyph'] <- gl_pol
    
    readline("press the return key to continue: don't fill polygon glyph area")

    gl_pol['showArea'] <- FALSE

    ## Mix glyphs
    readline("press the return key to continue: mix between different glyphs")

    g <- sample(
        c(
            'circle', 'square', 'triangle', 'diamond',
            'ocircle', 'osquare', 'otriangle', 'odiamond',
            gt, 
            if (loon:::.withTclImg) gi else NULL,
            sa, gl_pol
        ),
        dim(olive)[1], replace=TRUE
    )
    p['glyph'] <- g

    ## Pointrange Glyphs
    readline("press the return key to continue: next pointrange glyphs")

    avg <- with(chickwts, tapply(weight, feed, mean))
    sd <- with(chickwts, tapply(weight, feed, sd))

    min <- with(chickwts, tapply(weight, feed, min))
    max <- with(chickwts, tapply(weight, feed, max))

    p1 <- l_plot(avg~sd)
    g.p <- l_glyph_add_pointrange(p1, ymin=min, ymax=max)

    p1['glyph'] <- g.p

})
