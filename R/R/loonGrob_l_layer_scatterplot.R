#' @export
loonGrob.l_layer_scatterplot <- function(target, name = NULL, gp = NULL, vp = NULL) {
    
    widget <- l_create_handle(attr(target, "widget"))
    states <- get_layer_states(widget)
    
    browser()
    
    if (!any(states$active)) {
        grob(name = name, gp = gp, vp = vp)
    } else {
        
        order <- get_model_display_order(widget)
        
        sdf <- as.data.frame(states)[order, ]
        s_a <- sdf[sdf$active, ]
        
        children_grobs <- lapply(seq_len(nrow(s_a)), function(i) {
            
            case_i <- s_a[i,]
            
            type <- l_glyph_getType(widget, case_i$glyph)
            
            loonGlyphGrob(widget, structure(NULL, class=type), case_i) 
        })
        
        
        gTree(
            children = do.call('gList', children_grobs),
            name = name, gp = gp, vp = vp
        )
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
    
    browser()
    
    if (glyph %in% l_primitiveGlyphs()) {
        cex <- as_r_point_size(glyph_info$size)
        col <- glyph_info$color
        pch <- glyph_to_pch(glyph)
        # is there a fill colour?
        filled <- (pch %in% 21:24)
        if (filled) {
                pointsGrob(x = glyph_info$x,
                           y = glyph_info$y,
                           gp = gpar(fill = col, 
                                     col = col, 
                                     pch = pch, 
                                     cex = cex)
                           #unit(1, "char"),  default.units = "native",
                           )
        } else {
                pointsGrob(x = glyph_info$x,
                           y = glyph_info$y,
                           gp = gpar(col = col, 
                                     pch = pch, 
                                     cex = cex)
                           #unit(1, "char"),  default.units = "native",
                )
        }
        # switch(
        #     glyph,
        #     circle = {
        #         pointsGrob(x = glyph_info$x,
        #                    y = glyph_info$y,
        #                    pch = 1, size = unit(1, "char"),
        #                    default.units = "native", name = name,
        #                    gp = gpar(), vp = NULL)
        #         
        #     },
        #     ocircle = {
        #         
        #     },
        #     ccircle = {
        #         
        #     },
        #     square = {
        #         
        #     },
        #     osquare = {
        #         
        #     },
        #     csquare = {
        #         
        #     },
        #     triangle = {
        #         
        #     },
        #     otriangle = {
        #         
        #     },
        #     ctriangle = {
        #         
        #     },
        #     diamond = {
        #         
        #     },
        #     odiamond = {
        #         
        #     },
        #     cdiamond = {
        #         
        #     },
        #     stop(glyph, "is not a primitive glyph in loon.")
        # )
    }
}