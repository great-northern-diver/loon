
#' @rdname loonGrob
#'
#' @examples
#'
#' \dontrun{
#' ## histogram examples
#'
#' h <- l_hist(iris$Sepal.Length, color=iris$Species)
#'
#' g <- loonGrob(h)
#'
#' library(grid)
#' grid.newpage(); grid.draw(g)
#'
#' h['showStackedColors'] <- TRUE
#'
#' g <- loonGrob(h)
#'
#' grid.newpage(); grid.draw(g)
#'
#' h['colorStackingOrder'] <- c("selected", unique(h['color']))
#'
#' g <- loonGrob(h)
#' grid.newpage(); grid.draw(g)
#'
#' h['colorStackingOrder'] <- rev(h['colorStackingOrder'])
#'
#' # To print directly use either
#' plot(h)
#' # or
#' grid.loon(h)
#' }
#'
#' @export
loonGrob.l_layer_histogram <- function(target, name = NULL, gp = NULL, vp = NULL) {

    widget <- l_create_handle(attr(target, "widget"))

    # get binning data from Tcl
    # this returns a nested list with one list per bin
    #
    # the list elements per bin have
    #  count (is count if yshows is frequency and density if yshows is density )
    #  points
    #  x0
    #  x1

    yshows <- widget['yshows']
    swapAxes <- widget['swapAxes']
    x <- widget['x']

    if(length(x) == 0)
        return(
            gTree(
                children = gList(),
                name = if(is.null(name)) "histogram" else name,
                gp = gp, vp = vp
            )
        )

    bins <- l_getBinData(widget)

    sel_color <- l_getOption("select-color")

    showStackedColors <- widget['showStackedColors']
    showOutlines <- widget['showOutlines']
    colorOutline <- if(showOutlines) as_hex6color(widget["colorOutline"]) else NA

    colorStackingOrder <- widget['colorStackingOrder']
    if(length(colorStackingOrder) == 1) {
        if(colorStackingOrder == "selected") {
            colorStackingOrder <- c("selected",
                                    levels(as.factor(widget['color'])))
            }
    }

    b_bins <- lapply(bins, function(bin) {

        w <- bin$x1 - bin$x0
        x <- bin$x0 + w/2
        y0 <- 0

        nam <- names(bin$count[-1])

        if (showStackedColors) {
            first <- intersect(colorStackingOrder, nam)
            rest <- setdiff(nam, c("all", first))
            bnames <- c(first, rest)

            if (swapAxes) {
                Map(function(height, col) {
                    if (col == "selected") col <- sel_color

                    col <- as_hex6color(col)

                    g <- rectGrob(x = y0, y = x, width = height, height = w,
                                  gp = gpar(fill = col, col = colorOutline),
                                  just = c("left", "center"),
                                  default.units = "native")
                    y0 <<- y0 + height
                    g
                }, bin$count[bnames], bnames)

            } else {
                Map(function(height, col) {
                    if (col == "selected") col <- sel_color

                    col <- as_hex6color(col)

                    g <- rectGrob(x = x, y = y0, width = w, height = height,
                                  gp = gpar(fill = col, col = colorOutline),
                                  just = c("center", "bottom"),
                                  default.units = "native")
                    y0 <<- y0 + height
                    g
                }, bin$count[bnames], bnames)
            }
        } else {
            newCount <- list()
            if("selected" %in% nam) {
               newCount$selected <- bin$count$selected
               newCount$unselected <- bin$count$all - bin$count$selected
            } else newCount$unselected <- bin$count$all
            bnames <- names(newCount)

            if(swapAxes) {
                Map(function(height, col) {
                    col <- if (col == "selected") sel_color else "thistle"
                    g <- rectGrob(x = y0, y = x, width = height, height = w,
                                  gp = gpar(fill = col, col = colorOutline),
                                  just = c("left", "center"),
                                  default.units = "native")
                    y0 <<- y0 + height
                    g
                }, newCount[bnames], bnames)
            } else {
                Map(function(height, col) {
                    col <- if (col == "selected") sel_color else "thistle"
                    g <- rectGrob(x = x, y = y0, width = w, height = height,
                                  gp = gpar(fill = col, col = colorOutline),
                                  just = c("center", "bottom"),
                                  default.units = "native")
                    y0 <<- y0 + height
                    g
                }, newCount[bnames], bnames)
            }
        }
    })

    args <- unlist(unname(b_bins),  recursive = FALSE)

    gTree(
        children = do.call(gList, if(is.null(args)) list() else args),
        name = if(is.null(name)) "histogram" else name,
        gp = gp, vp = vp
    )

}
