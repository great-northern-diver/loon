
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
            children_grobs <- lapply(seq_len(length(s_a$x)), function(i) {
                
                case_i <- list(
                    x = s_a$x[i],
                    y = s_a$y[i],
                    glyph = s_a$glyph[i],
                    color = s_a$color[i],
                    size = s_a$size[i],
                    index = s_a$index[i]
                )
                
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
