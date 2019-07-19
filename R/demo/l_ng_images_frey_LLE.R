

if (requireNamespace("loon.data", quietly = TRUE) &&
    requireNamespace("RDRToolbox", quietly = TRUE)) {

    local({

        data(frey, package = "loon.data")

        ## LLE
        frey.lle <- RDRToolbox::LLE(t(frey), dim=5, k=12)

        frey.lle <- as.data.frame(frey.lle)
        names(frey.lle) <- paste("V",1:5, sep="")


        ## Images
        ## sample every third image
        sel <- seq(1,dim(frey)[2],3)

        frey.imgs <-  l_image_import_array(frey[,sel],28,20, img_in_row = FALSE, rotate = 90)
        l_imageviewer(frey.imgs)

        nav <- l_navgraph(frey.lle[sel,])

        gl <- l_glyph_add_image(nav$plot, images=frey.imgs, label="frey faces")
        nav$plot['glyph'] <- gl

    })

    cat(paste("\n\nThe source code of this demo file is located at:\n",system.file("demo", "l_ng_images_frey_LLE.R", package="loon"),"\n\n\n"))
}
