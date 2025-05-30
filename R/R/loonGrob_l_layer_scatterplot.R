#' @rdname loonGrob
#'
#' @examples
#' if(interactive()) {
#'
#' ## l_plot scatterplot examples
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
#'}
#' @export
loonGrob.l_layer_scatterplot <- function(target, name = NULL, gp = NULL, vp = NULL) {

    widget <- l_create_handle(attr(target, "widget"))
    states <- get_layer_states(widget)

    if (!any(states$active)) {
        # No active points in scatterplot
        points <- grob(name = if(is.null(name)) "points: missing glyphs" else name,
                       gp = gp, vp = vp)
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

        if (!any(is.na(pch))) {

            color <- s_a$color
            fill <- rep(NA, length(color))
            size <- s_a$size

            pointsWithBorders <- pch %in% 21:24
            fill[pointsWithBorders] <- color[pointsWithBorders]
            color[pointsWithBorders] <- loon::l_getOption("foreground")

            # deal with repeat aesthetic attributes
            if(len_unique(pch) == 1) pch <- pch[1L]
            if(len_unique(fill) == 1) fill <- fill[1L]
            if(len_unique(color) == 1) color <- color[1L]
            if(len_unique(size) == 1L) size <- size[1L]

            points <- pointsGrob(name = "points: primitive glyphs",
                                 x = s_a$x,
                                 y = s_a$y,
                                 pch = pch,
                                 gp = gpar(fill = fill,
                                           col = color,
                                           fontsize = as_grid_size(size, "points",
                                                                   pch = pch))
            )

        } else {
            # possibly some NAs (means some points are text, polygons, images, etc.)
            scaleInfo <- get_glyph_scale_info(widget)
            names_scaleInfo <- names(scaleInfo)

            children_grobs <- lapply(seq_len(length(s_a$x)),
                                     function(i) {

                                         case_i <- list(x = s_a$x[i],
                                                        y = s_a$y[i],
                                                        glyph = s_a$glyph[i],
                                                        color = s_a$color[i],
                                                        size = s_a$size[i],
                                                        index = s_a$index[i]
                                         )

                                         type <- l_glyph_getType(widget, case_i$glyph)

                                         scaleInfoLocation <- (grepl(case_i$glyph, names_scaleInfo) == TRUE)

                                         if (sum(scaleInfoLocation) > 1 &
                                             case_i$glyph %in% c("circle", "square", "triangle")
                                         ) { # Then there has been some redundant selection
                                             scaleInfoLocation <-  scaleInfoLocation &
                                                 (grepl("ocircle", names_scaleInfo) != TRUE) &
                                                 (grepl("ccircle", names_scaleInfo) != TRUE) &
                                                 (grepl("osquare", names_scaleInfo) != TRUE) &
                                                 (grepl("csquare", names_scaleInfo) != TRUE) &
                                                 (grepl("otriangle", names_scaleInfo) != TRUE) &
                                                 (grepl("ctriangle", names_scaleInfo) != TRUE)
                                         }

                                         case_i$scaleInfo <- scaleInfo[[which(scaleInfoLocation == TRUE)]]

                                         loonGlyphGrob(widget, structure(list(), class=type), case_i)
                                     })

            points <- gTree(children = do.call('gList', children_grobs),
                            name = "points: mixed glyphs")

        }
    }
    gTree(children = gList(points),
          name = if(is.null(name)) "scatterplot" else name,
          gp = gp, vp = vp
    )
}
#' @title Create a grob glyph from a loon widget
#'
#' @description
#' A generic function used by \code{loonGrob} specialized for particular loon widgets.
#' Used to construct the various point symbol types of the plot.
#' Different \code{S3} methods are implemented for various loon point glyphs.
#'
#' @param widget the loon widget.
#' @param x argument used to dispatch the method -- an empty structure of class equal to that
#'          returned by \code{l_glyph_getType}.
#' @param glyph_info a named list of pertinent components of the glyph including
#' its x and y locations in the plot as well as other information relevant to the
#' particular glyph.
#'
#' @return A grob for that glyph.
#'
#' @seealso \code{\link{loonGrob}}
#'
#' @export
loonGlyphGrob <- function(widget, x, glyph_info) {
    UseMethod("loonGlyphGrob", x)
}

#' @export
loonGlyphGrob.default <- function(widget, x, glyph_info) {
    grob()
}


#' @export
loonGlyphGrob.primitive_glyph <- function(widget, x, glyph_info) {
    glyph <- glyph_info$glyph

    if (glyph %in% l_primitiveGlyphs()) {
        pch <- glyph_to_pch(glyph)
        col <- glyph_info$color
        fontsize <- as_grid_size(glyph_info$size, "points",
                                 pch = pch)
        # is there a fill colour?
        filled <- (pch %in% 21:24)
        if (filled) {
            pointsGrob(x = glyph_info$x,
                       y = glyph_info$y,
                       pch = pch,
                       gp = gpar(fill = col,
                                 col = l_getOption("foreground"),
                                 fontsize = fontsize),
                       name = paste0("primitive_glyph ", glyph_info$index)
            )
        } else {
            pointsGrob(x = glyph_info$x,
                       y = glyph_info$y,
                       pch = pch,
                       gp = gpar(col = col,
                                 fontsize = fontsize),
                       name = paste0("primitive_glyph ", glyph_info$index)
            )
        }
    }
}

#' image glyph
#'
#' @noRd
#'
#' @examples
#' if(interactive()) {
#'
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
#'}
#' \dontrun{
#' p['glyph'] <- g
#' gr <- loonGrob(p)
#' grid.newpage(); grid.draw(gr)
#'
#' g2 <- removeGrob(gr, "image_border", global=TRUE)
#' grid.newpage(); grid.draw(g2)
#' }

#' @export
loonGlyphGrob.image <-  function(widget, x, glyph_info) {

    gh <- l_create_handle(c(widget, glyph_info$glyph))

    tcl_img_i <- gh['images'][glyph_info$index]
    size_i <- glyph_info$size

    # get the scaled_image
    height <- as.numeric(tcl("image", "height", tcl_img_i))
    width <- as.numeric(tcl("image", "width", tcl_img_i))
    ratio <- height/width
    # area <- as.numeric(tcl("::loon::map_image_size", size_i))
    # scale <- sqrt(area/(width*height))
    # image_w <- floor(scale*width)
    # image_h <- floor(scale*height)
    # width_p <- unit(as_r_image_size(image_w), "mm")
    # height_p <- unit(as_r_image_size(image_h), "mm")

    scaled_img <- as.character(tkimage.create("photo"))
    tcl(tcl("set", "::loon::Options(image_scale)"),  tcl_img_i,
        round(width), round(height),  scaled_img)

    r_img <- tcl_img_2_r_raster(scaled_img)

    tcl("image", "delete", scaled_img)

    # THIS IS A HACK
    height_p <- as_grid_size(size_i,
                             type = "image",
                             adjust = 0.6,
                             ratio = ratio)
    width_p <- height_p/ratio

    gTree(
        children = gList(
            rectGrob(x = glyph_info$x, y = glyph_info$y, just = "centre",
                     width = unit(width_p, "cm") + unit(2, "mm"),
                     height = unit(height_p, "cm") + unit(2, "mm"),
                     gp = gpar(
                         fill = glyph_info$color,
                         col = NA
                     ),
                     name = "image_border"),
            rasterGrob(r_img, x = glyph_info$x, y = glyph_info$y, just = "centre",
                       width = unit(width_p, "cm"),
                       height = unit(height_p, "cm"),
                       name = "image")
        ),
        name = paste0("image_glyph ", glyph_info$index)
    )

}

#' @title A tk Image Object to a Raster Object
#' @description Turn a tk image object to an R \code{raster} object
#' @param img a tk image object
#' @export
#' @examples
#' if(requireNamespace("grid")) {
#' puglia <- list.files(file.path(find.package(package = 'loon'), "images"),
#'                      full.names = TRUE)[1L]
#' # `img` is a tk image object
#' img <- setNames(l_image_import_files(puglia),
#'                 tools::file_path_sans_ext(basename(puglia)))
#' raster <- tcl_img_2_r_raster(img)
#' grid::grid.newpage()
#' grid::grid.raster(raster)
#' }

tcl_img_2_r_raster <- function(img) {
    if (!(img %in% as.character(tcl("image", "names"))))
        stop("image does not exist")

    height <- as.numeric(tcl("image", "height", img))
    width <- as.numeric(tcl("image", "width", img))

    img_data <- unlist(strsplit(toupper(as.character(tcl(img, 'data'))), " "))
    img_mat <- matrix(img_data, nrow = height, byrow = TRUE)

    as.raster(img_mat)
}


#' @export
loonGlyphGrob.text <-  function(widget, x, glyph_info) {

    gh <- l_create_handle(c(widget, glyph_info$glyph))

    textGrob(label = gh['text'][glyph_info$index],
             x = glyph_info$x,
             y = glyph_info$y,
             gp=gpar(fontsize = as_grid_size(glyph_info$size, "texts"),
                     col = glyph_info$color),
             name = paste0("text_glyph ", glyph_info$index)
    )
}

#' @export
loonGlyphGrob.pointrange <-  function(widget, x, glyph_info) {

    gh <- l_create_handle(c(widget, glyph_info$glyph))

    showArea <- gh['showArea']

    pch <- if (showArea) 21 else 19

    gTree(
        children =  gList(
            pointsGrob(x = glyph_info$x, y = glyph_info$y,
                       gp = gpar(col = glyph_info$color,
                                 fontsize = as_grid_size(glyph_info$size, "points",
                                                         pch = pch)),
                       pch = pch,
                       name = "point"),
            linesGrob(x = rep(glyph_info$x, 2),
                      y = unit(c(gh['ymin'][glyph_info$index],
                                 gh['ymax'][glyph_info$index]),
                               "native"),
                      gp = gpar(col = glyph_info$color,
                                lwd =  as_grid_size(gh['linewidth'][glyph_info$index], "lines")),
                      name = "range")
        ),
        name = paste0("pointrange_glyph ", glyph_info$index)
    )

}

#' @export
loonGlyphGrob.polygon <-  function(widget, x, glyph_info) {

    gh <- l_create_handle(c(widget, glyph_info$glyph))

    showArea <- gh['showArea'][glyph_info$index]
    linewidth <- gh['linewidth'][glyph_info$index]
    color <- glyph_info$color
    size <- glyph_info$size

    # THIS IS A HACK
    poly_x <- gh['x'][[glyph_info$index]] * as_grid_size(size, "polygon", 0.6)
    poly_y <- - gh['y'][[glyph_info$index]] * as_grid_size(size, "polygon", 0.6)

    x <- glyph_info$x
    y <- glyph_info$y

    if(showArea){
        polygonGrob(x = x + unit(poly_x, "cm"),
                    y = y + unit(poly_y, "cm"),
                    gp = gpar(
                        fill = color,
                        col =  color,
                        lwd = as_grid_size(linewidth, "lines")
                    ),
                    name = paste0("polygon_glyph: showArea ", glyph_info$index)
        )
    } else {
        polylineGrob(x = x + unit(c(poly_x, poly_x[1]), "cm"),
                     y = y + unit(c(poly_y, poly_y[1]), "cm"),
                     gp = gpar(
                         fill = color,
                         col = color,
                         lwd = as_grid_size(linewidth, "lines")
                     ),
                     name = paste0("polygon_glyph ", glyph_info$index)
        )
    }
}



#' @export
loonGlyphGrob.serialaxes <-  function(widget, x, glyph_info) {

    gh <- l_create_handle(c(widget, glyph_info$glyph))

    # show axes or not
    showAxes <- gh['showAxes']
    showEnclosing <- gh['showEnclosing']
    showArea <- gh['showArea']
    # scaling method
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

    p <- length(gh['sequence'])
    andrews <- gh['andrews']

    # position
    xpos <- glyph_info$x
    ypos <- glyph_info$y

    if(axesLayout == "parallel") {

        # default setting
        # height:width = 1:2
        scaleX <- as_grid_size(size, "parallel", p = p)
        scaleY <- scaleX/2

        lineXaxis <- seq(-0.5 * scaleX, 0.5 * scaleX, length.out = dimension)
        lineYaxis <- (scaledData[glyph_info$index, ] - 0.5) * scaleY

        xaxis <- if(andrews) {
            pth(lineXaxis, p)
        } else {
            lineXaxis
        }

        gTree (
            children = gList(
                condGrob(
                    test = showEnclosing,
                    grobFun = polylineGrob,
                    name = "boundary",
                    x = xpos + unit((c(0, 0, 1, 0, 0, 1, 1, 1) - 0.5) * scaleX, "cm"),
                    y = ypos + unit((c(0, 0, 0, 1, 1, 0, 1, 1) - 0.5) * scaleY, "cm"),
                    id = rep(1:4, 2),
                    gp = gpar(col = bboxColor)
                ),
                condGrob(
                    test = showAxes,
                    grobFun = polylineGrob,
                    name = "axes",
                    x = xpos + rep(unit(xaxis, "cm"), each = 2),
                    y = ypos + rep(unit(c(- 0.5 * scaleY, 0.5 * scaleY), "cm"), p),
                    id = rep(1:p, each = 2),
                    gp = gpar(col = axesColor)
                ),
                if(showArea) {
                    polygonGrob(x = xpos + unit(c(lineXaxis, rev(lineXaxis)), "cm"),
                                y = ypos + unit(c(lineYaxis, rep(-0.5 * scaleY, dimension)), "cm"),
                                gp = gpar(fill = color, col = NA),
                                name = "polyline: showArea")
                } else {
                    linesGrob(x = xpos + unit(lineXaxis, "cm"),
                              y = ypos + unit(lineYaxis, "cm"),
                              gp = gpar(col = color),
                              name = "polyline")
                }
            ), name = paste0("serialaxes_glyph: parallel ",  glyph_info$index)
        )
    } else {

        # diameter is returned
        # convert to radius
        scaleX <- as_grid_size(size, "radial")/2
        scaleY <- as_grid_size(size, "radial")/2

        angle <- seq(0, 2*pi, length.out = dimension + 1)[1:dimension]
        radialxaxis <- scaleX * scaledData[glyph_info$index,] * cos(angle)
        radialyaxis <- scaleY * scaledData[glyph_info$index,] * sin(angle)

        if (showEnclosing) {
            xaxis <- scaleX * cos(angle)
            yaxis <- scaleY * sin(angle)
        } else {
            xaxis <- radialxaxis
            yaxis <- radialyaxis
        }

        if(andrews) {
            xaxis <- pth(xaxis, p, TRUE)
            yaxis <- pth(yaxis, p, TRUE)
        }

        gTree(
            children = gList(
                if(showArea) {
                    polygonGrob(x = xpos + unit(c(radialxaxis, radialxaxis[1]), "cm"),
                                y = ypos + unit(c(radialyaxis, radialyaxis[1]), "cm"),
                                gp = gpar(fill = color, col = NA),
                                name = "polyline: showArea")
                } else {
                    linesGrob(x = xpos + unit(c(radialxaxis, radialxaxis[1]), "cm"),
                              y = ypos + unit(c(radialyaxis, radialyaxis[1]), "cm"),
                              gp = gpar(col = color),
                              name = "polyline")
                },
                condGrob(
                    test = showEnclosing,
                    grobFun = polylineGrob,
                    name = "boundary",
                    x = xpos + unit(scaleX * cos(seq(0, 2*pi, length=101)), "cm"),
                    y = ypos + unit(scaleY * sin(seq(0, 2*pi, length=101)), "cm"),
                    gp = gpar(col = bboxColor)
                ),
                condGrob(
                    test = showAxes,
                    grobFun = polylineGrob,
                    name = "axes",
                    x = xpos + unit(c(rep(0, p), xaxis), "cm"),
                    y = ypos + unit(c(rep(0, p), yaxis), "cm"),
                    id = rep(1:p, 2),
                    gp = gpar(col = axesColor)
                )
            ),
            name = paste0("serialaxes_glyph: radial ", glyph_info$index)
        )
    }
}

pth <- function(x, p, circle = FALSE) {
    len <- length(x)
    if(len == p) return(x)
    # In a circle, the first one and the last one are identical
    if(circle) {
        x[round(seq(1, len, length.out = p + 1))][- (p + 1)]
    } else {
        x[round(seq(1, len, length.out = p))]
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

                dat <- char2num.data.frame(gh['data'])
                seqName <- gh['sequence']
                andrews <- gh['andrews']
                andrewsSeriesLength <-  gh['andrewsSeriesLength']

                scaledActiveData <- get_scaledData(
                    data = dat, # convert to numeric
                    sequence = seqName,
                    scaling = gh['scaling'],
                    displayOrder = 1:dim(dat)[1])

                if(andrews) {
                    fourierTrans <- andrews(k = length(seqName), length.out = andrewsSeriesLength)
                    scaledActiveData <- as.matrix(scaledActiveData) %*% fourierTrans$matrix

                    dataRange <- range(scaledActiveData)
                    d <- if(diff(dataRange) == 0) 1 else diff(dataRange)

                    scaledActiveData <- (scaledActiveData - min(scaledActiveData))/d
                }

                scaledActiveData

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

len_unique <- function(x, incomparables = FALSE, ...) {
    length(unique(x, incomparables, ...))
}

# if (!any(is.na(pch)) && !any(pch %in% 21:24)) {
#     # No NAs and no points with borders
#     points <- pointsGrob(name = "points: primitive glyphs without borders",
#                          x = s_a$x,
#                          y = s_a$y,
#                          pch = pch,
#                          gp = gpar(col = s_a$color,
#                                    fontsize = as_grid_size(s_a$size,
#                                                            "points",
#                                                            pch = pch))
#     )
# } else if (!any(is.na(pch)) && all(pch %in% 21:24)) {
#     # No NAs and ALL points with borders
#     points <- pointsGrob(name = "points: primitive glyphs with borders",
#                          x = s_a$x,
#                          y = s_a$y,
#                          pch = pch,
#                          gp = gpar(fill = s_a$color,
#                                    col = l_getOption("foreground"),
#                                    fontsize = as_grid_size(s_a$size, "points",
#                                                            pch = pch))
#     )
# } else {...}
