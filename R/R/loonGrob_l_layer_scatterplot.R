
#' Create a grob of a scatterplot layer
#' 
#' 
#' @export
#' 
#' @examples 
#' 
#' p <- l_plot(x = c(0,1), y = c(0,1))
#' l_layer_rectangle(p, x = c(0,1), y = c(0,1))
#' 
#' g <- loonGrob(p) 
#' 
#' library(grid)
#' grid.newpage(); grid.draw(g)
#' 
#' p['glyph'] <- "ctriangle"
#' p['color'] <- "blue"
#' p['size'] <- c(10, 20)
#' p['selected'] <- c(TRUE, FALSE)
#' g <- loonGrob(p)
#' grid.newpage(); grid.draw(g)
#' 
loonGrob.l_layer_scatterplot <- function(target, name = NULL, gp = NULL, vp = NULL) {
    

    widget <- l_create_handle(attr(target, "widget"))
    states <- get_layer_states(widget)
    
    if (!any(states$active)) {
        grob(name = name, gp = gp, vp = vp)
    } else {
        
        display_order <- get_model_display_order(widget)
        
        active <- states$active[display_order]
        
        selected <- states$selected[display_order][active]
        
        s_a <- list(x = states$x[display_order][active],
                    y = states$y[display_order][active],
                    glyph = states$glyph[display_order][active],
                    color = get_display_color(states$color[display_order][active], selected),
                    size = states$size[display_order][active],
                    index = seq_along(states$x)[display_order][active]
                    )

        pch <- glyph_to_pch(s_a$glyph)
        if (!any(is.na(pch)) && !any(pch %in% 21:24)) {
            pointsGrob(x = s_a$x,
                       y = s_a$y,
                       pch = pch, 
                       gp = gpar(col = s_a$color,
                                 cex = as_r_point_size(s_a$size))
            )
        } else if (!any(is.na(pch)) && all(pch %in% 21:24)) {
            pointsGrob(x = s_a$x,
                       y = s_a$y,
                       pch = pch, 
                       gp = gpar(fill = s_a$color,
                                 col = l_getOption("foreground"),
                                 cex = as_r_point_size(s_a$size))
            )
        } else {
            
            scaleInfo <- get_glyph_scale_info(widget)
            names_scaleInfo <- names(scaleInfo)
            
            children_grobs <- lapply(seq_len(length(s_a$x)), function(i) {
                
                case_i <- list(
                    x = s_a$x[i],
                    y = s_a$y[i],
                    glyph = s_a$glyph[i],
                    color = s_a$color[i],
                    size = s_a$size[i],
                    index = s_a$index[i]
                )
                
                case_i$scaleInfo <- scaleInfo[[which(grepl(case_i$glyph, names_scaleInfo) == TRUE)]]
                
                type <- l_glyph_getType(widget, case_i$glyph)
                
                loonGlyphGrob(widget, structure(list(), class=type), case_i) 
            })
            
            
            gTree(
                children = do.call('gList', children_grobs),
                name = name, gp = gp, vp = vp
            )
        }
    } 
}

loonGlyphGrob <- function(widget, x, glyph_info) {
    UseMethod("loonGlyphGrob", x)
}

loonGlyphGrob.default <- function(widget, x, glyph_info) {
    grob()
}


loonGlyphGrob.primitive_glyph <- function(widget, x, glyph_info) {
    glyph <- glyph_info$glyph
    
    if (glyph %in% l_primitiveGlyphs()) {
        cex <- as_r_point_size(glyph_info$size)
        col <- glyph_info$color
        pch <- glyph_to_pch(glyph)
        # is there a fill colour?
        filled <- (pch %in% 21:24)
        if (filled) {
                pointsGrob(x = glyph_info$x,
                           y = glyph_info$y,
                           pch = pch, 
                           gp = gpar(fill = col, 
                                     col = l_getOption("foreground"),
                                     cex = cex)
                           )
        } else {
                pointsGrob(x = glyph_info$x,
                           y = glyph_info$y,
                           pch = pch, 
                           gp = gpar(col = col,
                                     cex = cex)
                )
        }
    }
}

#' image glyph
#' 
#' @noRd
#' 
#' @examples 
#' p <- with(olive, l_plot(palmitic ~ stearic, color = Region))
#' img_paths <- list.files(file.path(find.package(package = 'loon'), "images"), full.names = TRUE)
#' imgs <- setNames(l_image_import_files(img_paths),
#'                  tools::file_path_sans_ext(basename(img_paths)))
#' i <- pmatch(gsub("^[[:alpha:]]+-","", olive$Area), names(imgs), duplicates.ok = TRUE)
#' 
#' g <- l_glyph_add_image(p, imgs[i], label="Flags")
#' 
#' p['glyph'] <- c(g, rep('circle', p['n']-1))
#' p['selected'] <- c(TRUE, rep(FALSE, p['n']-1))
#' 
#' gr <- loonGrob(p) 
#' 
#' library(grid)
#' grid.newpage(); grid.draw(gr)
#' 
#' \dontrun{
#' p['glyph'] <- g
#' gr <- loonGrob(p) 
#' grid.newpage(); grid.draw(gr)
#' 
#' g2 <- removeGrob(gr, "image_glyph_border", global=TRUE)
#' grid.newpage(); grid.draw(g2)
#' }
loonGlyphGrob.image <-  function(widget, x, glyph_info) {
    
    gh <- l_create_handle(c(widget, glyph_info$glyph))
    
    tcl_img_i <- gh['images'][glyph_info$index]
    size_i <- glyph_info$size
    
    
    # get the scaled_image
    height <- as.numeric(tcl("image", "height", tcl_img_i))
    width <- as.numeric(tcl("image", "width", tcl_img_i))
    
    area <- as.numeric(tcl("::loon::map_image_size", size_i))
    
    scale <- sqrt(area/(width*height))
    
    image_w <- floor(scale*width)
    image_h <- floor(scale*height)
    
    scaled_img <- as.character(tkimage.create("photo"))
    tcl(tcl("set", "::loon::Options(image_scale)"),  tcl_img_i,  image_w, image_h,  scaled_img)
    
    r_img <- tcl_img_2_r_raster(scaled_img)
    
    tcl("image", "delete", scaled_img)
    
    
    
    width_p = unit(image_w/40, "cm")
    height_p = unit(image_h/40, "cm")
    
    gTree(
        children = gList(
            rectGrob(x = glyph_info$x, y = glyph_info$y, just = "centre",
                     width = width_p+unit(2, "mm"), height = height_p+unit(2, "mm"),
                     gp = gpar(
                         fill = glyph_info$color,
                         col = NA
                     ),
                     name = "image_glyph_border"),
            rasterGrob(r_img, x = glyph_info$x, y = glyph_info$y, just = "centre",
                       width = width_p, height = height_p)
        )
    )

}

tcl_img_2_r_raster <- function(img) {
    if (!(img %in% as.character(tcl("image", "names"))))
        stop("image does not exist")
    
    height <- as.numeric(tcl("image", "height", img))
    width <- as.numeric(tcl("image", "width", img))
    
    img_data <- unlist(strsplit(toupper(as.character(tcl(img, 'data'))), " "))
    img_mat <- matrix(img_data, nrow = height, byrow = TRUE)
    
    as.raster(img_mat)
    
}


loonGlyphGrob.text <-  function(widget, x, glyph_info) {
    
    gh <- l_create_handle(c(widget, glyph_info$glyph))
    
    textGrob(label = gh['text'][glyph_info$index], 
             x = glyph_info$x, 
             y = glyph_info$y,
             gp=gpar(fontsize = as_r_text_size(glyph_info$size), 
                     col = glyph_info$color ))
}

loonGlyphGrob.pointrange <-  function(widget, x, glyph_info) {
    
    gh <- l_create_handle(c(widget, glyph_info$glyph))
    
    showArea <- gh['showArea']
    
    gTree(
        children =  gList(
            {if (showArea) {
                pch <- 21
            } else {
                pch <- 19
            }
            pointsGrob(x = glyph_info$x, y = glyph_info$y,
                           gp = gpar(col = glyph_info$color, 
                                     cex = as_r_point_size(glyph_info$size)
                           ),
                           pch = pch
                           )}, 
            linesGrob(x = rep(glyph_info$x, 2), 
                      y = unit(c(gh['ymin'][glyph_info$index], 
                                 gh['ymax'][glyph_info$index]), 
                               "native"), 
                      gp = gpar(col = glyph_info$color, 
                                lwd =  gh['linewidth'][glyph_info$index]))
        )
    )
    
}

loonGlyphGrob.polygon <-  function(widget, x, glyph_info) {
    
    gh <- l_create_handle(c(widget, glyph_info$glyph))
    
    showArea <- gh['showArea'][glyph_info$index]
    linewidth <- gh['linewidth'][glyph_info$index]
    color <- glyph_info$color
    size <- glyph_info$size
    
    poly_x <- gh['x'][[glyph_info$index]] * as_r_polygonGlyph_size(size)
    poly_y <- - gh['y'][[glyph_info$index]] * as_r_polygonGlyph_size(size)
    
    x <- glyph_info$x
    y <- glyph_info$y
    
    gTree(
        vp = viewport(x = x, y = y),
        children = 
            gList(
                
                if(showArea){
                    polygonGrob(x = poly_x, 
                                y = poly_y,
                                gp = gpar(
                                    fill = color, 
                                    col =  color, 
                                    lwd = linewidth
                                ), 
                                vp = viewport(x = 0, y = 0, 
                                              width = 2 * as_r_polygonGlyph_size(size), 
                                              height = 2 * as_r_polygonGlyph_size(size)),
                                default.units = "mm"
                    )
                } else {
                    polylineGrob(x = c(poly_x, poly_x[1]), 
                                 y = c(poly_y, poly_y[1]),
                                 gp = gpar(
                                     fill = color, 
                                     col =  color, 
                                     lwd = linewidth
                                 ), 
                                 vp = viewport(x = 0, y = 0, 
                                               width = 2 * as_r_polygonGlyph_size(size), 
                                               height = 2 * as_r_polygonGlyph_size(size)),
                                 default.units = "mm"
                    )
                }
                
            )
    )
}

as_r_polygonGlyph_size <- function(size){
    if (is.numeric(size)) {
        # trial and error to choose scale for size
        size <- size
        size[size < 0.01] <- 0.01 
        size
    }
    size
}

loonGlyphGrob.serialaxes <-  function(widget, x, glyph_info) {
    
    gh <- l_create_handle(c(widget, glyph_info$glyph))
    
    # show axes or not
    showAxes <- gh['showAxes']
    showEnclosing <- gh['showEnclosing']
    showArea <- gh['showArea']
    # scaling way
    scaling <- gh['scaling']
    # line width
    linewidth <- gh['linewidth'][glyph_info$index]
    # bbox color
    bboxColor <- as_hex6color(gh['bboxColor'])
    # axes color
    axesColor <- as_hex6color(gh['axesColor'])
    # each element color
    color <- glyph_info$color
    # size
    size <- glyph_info$size
    # parallel or radial
    axesLayout <- gh['axesLayout']
    
    scaledData <- glyph_info$scaleInfo
    dimension <- dim(scaledData)[2]
    
    scaleX <- diff(c(widget['panX'], widget['panX'] + widget['deltaX']/widget['zoomX'])) * as_r_serialaxesGlyph_size(size)
    scaleY <- diff(c(widget['panY'], widget['panY'] + widget['deltaY']/widget['zoomY'])) * as_r_serialaxesGlyph_size(size)
    # position
    xpos <- as.numeric(glyph_info$x)
    ypos <- as.numeric(glyph_info$y)
    
    if(axesLayout == "parallel"){
        xaxis <- seq(-0.5 * scaleX, 0.5 * scaleX, length.out = dimension)
        yaxis <- (scaledData[glyph_info$index, ] - 0.5) * scaleY
        x <- xpos + xaxis
        y <- ypos + yaxis
        
        gTree ( children = gList(
            if(showEnclosing) {
                polylineGrob(x= (c(0, 0, 1, 0, 0, 1, 1, 1) - 0.5) * scaleX + xpos,
                             y= (c(0, 0, 0, 1, 1, 0, 1, 1) - 0.5) * scaleY + ypos, 
                             id=rep(1:4, 2),
                             gp=gpar(col = bboxColor), 
                             default.units = "native") 
            } else NULL ,
            if(showAxes) {
                polylineGrob(x = rep(x, each = 2), 
                             y = rep(c(ypos - 0.5 * scaleY, 
                                             ypos + 0.5 * scaleY), dimension),
                             id = rep(1:dimension, each = 2), 
                             gp = gpar(col = axesColor), 
                             default.units = "native")
            } else NULL ,
            if(showArea) {
                polygonGrob(x = c(x, rev(x)), 
                            y = c(y, rep(ypos - 0.5 * scaleY, dimension)), 
                            gp = gpar(fill = color, col = NA), 
                            default.units = "native")
            } else {
                linesGrob(x = x, 
                          y = y, 
                          gp = gpar(col = color), 
                          default.units = "native")}
        )
        )
    } else {
        angle <- seq(0, 2*pi, length.out = dimension + 1)[1:dimension]
        radialxais <- scaleX * scaledData[glyph_info$index,] * cos(angle)
        radialyais <- scaleY * scaledData[glyph_info$index,] * sin(angle)
        
        x <- xpos + radialxais
        y <- ypos + radialyais
        
        gTree ( children = gList( 
            if(showArea) {
                polygonGrob(x = c(x, x[1]), 
                            y = c(y, y[1]), 
                            gp = gpar(fill = color, col = NA), 
                            default.units = "native")
            } else {
                linesGrob(x = c(x, x[1]), 
                          y = c(y, y[1]), 
                          gp = gpar(col = color), 
                          default.units = "native")
            },
            if(showEnclosing) {
                polygonGrob(xpos + scaleX * cos(seq(0, 2*pi, length=101)), 
                            ypos + scaleY * sin(seq(0, 2*pi, length=101)), 
                            gp = gpar(fill = NA, col = bboxColor), 
                            default.units = "native" )
            } else NULL ,
            if(showAxes) {
                
                polylineGrob( x = c(rep(xpos, dimension) ,xpos + scaleX * cos(angle)), 
                              y = c(rep(ypos, dimension) ,ypos + scaleY * sin(angle)),
                              id = rep(1:dimension, 2),
                              gp = gpar(col =  axesColor), 
                              default.units = "native")
            } else NULL
        ) 
        )
        
    }
}

get_glyph_scale_info <- function(widget){
    
    unique_glyph <- unique(widget['glyph'])
    
    name <- sapply(unique_glyph, function(l) l_glyph_getType(widget, l) )
    
    scaleInfo <- lapply(seq_len(length(unique_glyph)), function(i){
        
        if(name[i] == "primitive_glyph") NULL # points glyph 
        else {
            
            gh <- l_create_handle(c(widget, unique_glyph[i]))
            
            if(name[i] == "serialaxes" ) {
                scaling <- gh['scaling']
                sequence <- gh['sequence']
                dat <- sapply( gh['data'], as.numeric)  # convert to numeric if not
                dat <- dat[, sequence]
                switch(scaling, 
                       "variable" = {
                           minV <- apply(dat, 2, "min")
                           maxV <- apply(dat, 2, "max")
                           t(
                               (t(dat) - minV) / (maxV  - minV) 
                           )
                       },
                       "observation" = {
                           minO <- apply(dat, 1, "min")
                           maxO <- apply(dat, 1, "max")
                           (dat - minO) / (maxO - minO)
                       }, 
                       "data" = {
                           minD <- min(dat)
                           maxD <- max(dat)
                           (dat - minD)/ (maxD - minD)
                       }, 
                       "none" = NULL)
            } else if (name[i] == "polygon") {
                # to be general 
                NULL
            } else if (name[i] == "text") {
                # to be general  
                NULL
            } else if (name[i] == "pointrange") {
                # to be general  
                NULL
            } else if (name[i] == "image") {
                # to be general  
                NULL
            } else NULL 
        }
    })
    names(scaleInfo) <- paste(name, unique_glyph)
    scaleInfo
}


as_r_serialaxesGlyph_size <- function(size){
    if (is.numeric(size)) {
        # trial and error to choose scale for size
        size <- sqrt(size) /30
        size[size == 0] <- 0.01
    }
    size
}
