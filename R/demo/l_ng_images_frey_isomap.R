

require(RnavGraphImageData) || stop('You need the RnavGraphImageData package installed!')

local({
    data(frey)

    ## isomap data dimensionality reduction
    ## this takes a bit of time, hence we provide the pre-calculated dimensions
    
    ##require(vegan) || stop("library vegan is needed for this demo.")    
    freyT <- t(frey)
    dims <- 6 
    ##dise <- vegdist(freyT, method="euclidean")
    ##ord <- isomap(dise,k = 12, ndim= dims, fragmentedOK = TRUE)
    data(ordfrey)

    iso.frey <- as.data.frame(ordfrey$points)
    
    
    ## Images
    ## sample every third image
    sel <- seq(1,dim(frey)[2],3)
    frey.imgs <- l_image_import_array(frey[,sel], 28,20,
                                      img_in_row = FALSE, rotate = 90)
    l_imageviewer(frey.imgs)

    nav <- l_navgraph(iso.frey[sel,])

    gl <- l_glyph_add_image(nav$plot, images=frey.imgs, label="frey faces")
    nav$plot['glyph'] <- gl

})

cat(paste("\n\nThe source code of this demo file is located at:\n",
          system.file("demo", "l_ng_images_frey_isomap.R", package="loon"),"\n\n\n"))
