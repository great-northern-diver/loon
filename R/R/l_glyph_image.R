
#' @export
l_glyph_add_image <- function(widget, images, label="", ...) {
    return(l_glyph_add(widget, "image",
                       images=images, label=label, ...))
}
