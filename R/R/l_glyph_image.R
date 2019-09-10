
#' @title Add an image glyphs
#'
#' @description Image glyphs are useful to show pictures or other sophisticated
#'   compound glyphs. Note that images in the Tk canvas support transparancy.
#'
#' @family glyph functions
#' @inheritParams l_glyph_add.default
#' @param images Tk image references, see the \code{\link{l_image_import_array}}
#'   and \code{\link{l_image_import_files}} helper functions.
#'
#'
#' @templateVar page learn_R_display_plot
#' @templateVar section images
#' @template see_l_help
#'
#' @export
#'
#' @seealso \code{\link{l_glyph_add}}, \code{\link{l_image_import_array}},
#'   \code{\link{l_image_import_files}}, \code{\link{l_make_glyphs}}
#'
#' @examples
#' \dontrun{
#' p <- with(olive, l_plot(palmitic ~ stearic, color = Region))
#' img_paths <- list.files(file.path(find.package(package = 'loon'), "images"), full.names = TRUE)
#' imgs <- setNames(l_image_import_files(img_paths),
#'                  tools::file_path_sans_ext(basename(img_paths)))
#' i <- pmatch(gsub("^[[:alpha:]]+-","", olive$Area), names(imgs), duplicates.ok = TRUE)
#'
#' g <- l_glyph_add_image(p, imgs[i], label="Flags")
#' p['glyph'] <- g
#' }
#'
l_glyph_add_image <- function(widget, images, label="", ...) {
    structure(
        l_glyph_add.default(widget, "image", images=images, label=label, ...),
        class = c("l_glyph_image", "l_glyph", "loon")
    )
}

