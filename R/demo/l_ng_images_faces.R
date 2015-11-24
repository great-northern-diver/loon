
require(RnavGraphImageData) || stop('You need the RnavGraphImageData package installed!')
require(PairViz) || stop('You need the PairViz package installed!')

local({
    data(faces)
    
    faces.imgs <- l_image_import_array(faces, 64, 64, img_in_row = FALSE)
    l_imageviewer(faces.imgs)
    
    group <- rep(1:40, each = 10)
    
    ## Multidimensional Scaling
    D <- L2Distance(as.matrix(faces),as.matrix(faces))

    faces.mds <- as.data.frame(cmdscale(D,k=4))
    names(faces.mds) <- paste('pc',1:4,sep = '')

    
    nav <- l_navgraph(faces.mds, color=gsub("FF$","",rainbow(40)[group]))
    
    gl <- l_glyph_add_image(nav$plot, images=faces.imgs, label="olivetti faces")
    nav$plot['glyph'] <- gl
    
})

cat(paste("\n\nThe source code of this demo file is located at:\n",
          system.file("demo", "l_ng_images_faces.R", package="loon"),"\n\n\n"))
