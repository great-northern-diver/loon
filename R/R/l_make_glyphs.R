

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
#' Note: type is not allowed in this list.
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
#'
#' ##  A more familiar example?
#' ##  The periodic table
#'
#' data("elements", package = "loon.data")
#'
#' # A draw function for each element
#' draw_element_box <- function(symbol,
#'                              name, number,
#'                              mass_number,
#'                              mass, col) {
#'    if (missing(col)) col <- "white"
#'    oldPar <-  par(bg = col, mar = rep(1, 4))
#'
#'    plot(NA, xlim = c(0,1), ylim = c(0, 1), axes=FALSE, ann = FALSE)
#'    text(0.5, 0.6, labels = symbol, cex = 18)
#'    text(0.15, 1, labels = number, cex = 6, adj= c(0.5,1))
#'    text(0.5, 0.25, labels = name, cex = 6)
#'    text(0.5, 0.11, labels = mass_number, cex = 3)
#'    text(0.5, 0.01, labels = mass, cex = 3)
#'    box()
#'
#'    par(oldPar)
#'    }
#'
#' # Get the categories
#' colIDs <- paste(elements$Category, elements$Subcategory)
#' # Get a loon palette function
#' colFn <- color_loon()
#' # Get colors identified with categories
#' tableCols <- colFn(colIDs)
#' #
#' # A function to an element box image for each element.
#'
#' make_element_boxes <- function(elements, cols, width = 500, height = 500) {
#'    if (missing(cols)) cols <- rep("white", nrow(elements))
#'    listOfElements <- lapply(1:nrow(elements),
#'                             FUN = function(i) {
#'                                 list(vals = elements[i,],
#'                                      col = cols[i])
#'                             })
#'    # glyphs created here
#'    l_make_glyphs(listOfElements,
#'                  draw_fun = function(element){
#'                      x <- element$vals
#'                      col <- element$col
#'                      draw_element_box(symbol = x$Symbol,
#'                                       name = x$Name,
#'                                       number = x$Number,
#'                                       mass_number = x$Mass_number,
#'                                       mass = x$Mass,
#'                                       col = col)
#'                  },
#'                  width = width,
#'                  height = height)
#'    }
#'
#' # Construct the glyphs
#' boxGlyphs <- make_element_boxes(elements, cols = tableCols)
#'
#' # Get a couple of plots
#' periodicTable <- l_plot(x = elements$x, y = elements$y,
#'                         xlabel = "", ylabel = "",
#'                         title = "Periodic Table of the Elements",
#'                         linkingGroup = "elements",
#'                         color = tableCols)
#'
#' # Add the images as possible glyphs
#'
#' bg <- l_glyph_add_image(periodicTable,
#'                         images = boxGlyphs,
#'                         label = "Symbol boxes")
#'
#' # Set this to be the glyph
#' periodicTable['glyph'] <- bg
#' #
#' # Get a second plot that shows the periodicity
#' #
#' # First some itemlabels
#' elementLabels <- with(elements,
#'                       paste("   ", Number, Symbol, "\n",
#'                             "   ", Name, "\n",
#'                             "   ", Mass
#'                             )
#'                       )
#'
#' periodicPlot  <- l_plot(x = elements$Mass, y = elements$Density,
#'                         xlabel = "Mass", ylabel = "Density",
#'                         itemLabel = elementLabels,
#'                         showItemLabels = TRUE,
#'                         linkingGroup = "elements",
#'                         color = tableCols)
#'
#' # Add the images as possible glyphs to this plot as well
#'
#' bg2 <- l_glyph_add_image(periodicPlot,
#'                          images = boxGlyphs,
#'                          label = "Symbol boxes")
#'
#' # Could set this to be the glyph
#' periodicPlot['glyph'] <- bg2
#'
#'
#' }
#'
#'
l_make_glyphs <- function(data, draw_fun, width=50, height=50, ...) {

    dotargs <- list(...)
    if (!is.null(dotargs$type)) stop("The 'type' argument is not allowed.")

    tmp <- tempdir()
    files <- Map(function(data_i, i) {

        file <- file.path(tmp, paste0("img", i, ".png"))

        grDevices::png(file, width=width, height = height, type = "cairo", ...)

        draw_fun(data_i)

        grDevices::dev.off()

        file
    }, data, 1:length(data))

    unlist(lapply(files, function(f)l_image_import_files(f)), use.names = FALSE)
}


