

#' @title Make arbitrary glyphs with R graphic devices
#'
#' @description Loon's primitive glyph types are limited in terms of compound
#'   shapes. With this function you can create each point glyph as a png and
#'   re-import it as a tk img object to be used as point glyphs in loon. See the
#'   examples.
#'
#' @param data list where each element contains a data object used for the
#'   \code{draw_fun}
#' @param draw_fun function that draws a glyph using \R base graphics or the
#'   grid (including ggplot2 and lattice) engine
#' @param width width of each glyph in pixel
#' @param height height of each glyph in pixel
#' @param ... additional arguments passed on to the \code{\link{png}} function
#'
#' @return vector with tk img object references
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' \dontrun{
#' if (requireNamespace("maps", quietly = TRUE)) {
#'   data(minority)
#'   p <- l_plot(minority$long, minority$lat)
#'
#'   canada <- maps::map("world",  "Canada", fill=TRUE, plot=FALSE)
#'   l_map <- l_layer(p, canada, asSingleLayer=TRUE)
#'   l_scaleto_world(p)
#'
#'   img <- l_make_glyphs(lapply(1:nrow(minority), function(i)minority[i,]), function(m) {
#'       par(mar=c(1,1,1,1)*.5)
#'       mat <- as.matrix(m[1,1:10]/max(m[1:10]))
#'       barplot(height = mat,
#'               beside = FALSE,
#'               ylim = c(0,1),
#'               axes= FALSE,
#'               axisnames=FALSE)
#'   }, width=120, height=120)
#'
#'   l_imageviewer(img)
#'
#'   g <- l_glyph_add_image(p, img, "barplot")
#'   p['glyph'] <- g
#'
#' }
#'
#' ## with grid
#' if (requireNamespace("grid", quietly = TRUE)) {
#'
#'   li <- l_make_glyphs(runif(6), function(x) {
#'       if(any(x>1 | x<0))
#'           stop("out of range")
#'       grid::pushViewport(grid::plotViewport(grid::unit(c(1,1,1,1)*0, "points")))
#'       grid::grid.rect(gp=grid::gpar(fill=NA))
#'       grid::grid.rect(0, 0, height = grid::unit(x, "npc"), just = c("left", "bottom"),
#'                 gp=grid::gpar(col=NA, fill="steelblue"))
#'   })
#'
#'   l_imageviewer(li)
#'
#'   p <- l_plot(1:6)
#'   g <- l_glyph_add_image(p, li, "bars")
#'   p['glyph'] <- g
#' }
#'
#' }
#' }
l_make_glyphs <- function(data, draw_fun, width=50, height=50, ...) {
    tmp <- tempdir()
    files <- Map(function(data_i, i) {

        file <- file.path(tmp, paste0("img", i, ".png"))

        grDevices::png(file, width=width, height = height, ...)

        draw_fun(data_i)

        grDevices::dev.off()

        file
    }, data, 1:length(data))

    unlist(lapply(files, function(f)l_image_import_files(f)), use.names = FALSE)
}


