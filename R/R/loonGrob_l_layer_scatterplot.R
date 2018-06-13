loonGrob.l_layer_scatterplot <- function(target, name = NULL, gp = NULL, vp = NULL) {
    s <- get_layer_states(target)
    
    if (!any(s$active)) {
        grob(name = name, gp = gp, vp = vp)
    } else {
        
        # TODO get_model_display_order
        s$size <- as_r_point_size(s$size)
        s$color <- get_model_color(widget)
        s$glyph <- glyph_to_pch(s$glyph)
        
        primitiveGlyphs <- pointsGrob(
            x = s$x[s$active], y = s$y[s$active],
            gp = gpar(col = s$col[s$active], cex = s$size[s$active]),
            pch = s$glyph[s$active]
        )
        
        gTree(
            children = gList(
                primitiveGlyphs
            ),
            name = name, gp = gp, vp = vp
        )
    } 
}