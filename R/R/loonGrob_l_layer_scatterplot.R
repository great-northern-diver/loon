#' @export
loonGrob.l_layer_scatterplot <- function(target, name = NULL, gp = NULL, vp = NULL) {
    
    widget <- l_create_handle(attr(target, "widget"))
    s <- get_layer_states(widget)
    

    if (!any(s$active)) {
        grob(name = name, gp = gp, vp = vp)
    } else {
        
        order <- get_model_display_order(widget)
        
        s <- as.data.frame(s)[order, ]
        s_a <- s[s$active, ]
        
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
    
}