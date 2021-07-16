as_grid_size <- function(size,
                         type = c("points", "texts", "images",
                                  "radial", "parallel", "polygon",
                                  "lines"),
                         adjust = 1, # A HACK
                         ...) {

    type <- match.arg(type)
    # **`size` is a loon size**
    # **returned a ggplot size**
    if (!is.numeric(size)) {
        s <- as.numeric(loon::l_getOption("size"))
        area <- ifelse(s < 1, 8, 12 * s)
        # pixel(unit)
        diameter.px <- sqrt(area/pi) * 2
        # pixel(unit) to pt(unit)
        diameter.pt <- diameter.px * px2pt(adjust = adjust)

        warning(
            "The class of the `size` is ",
            class(size),
            " which is not numerical. It will be set as the default `loon` point size ",
            diameter.pt,
            call. = FALSE
        )
        return(diameter.pt)
    }

    # From Adrian Waddell's Thesis
    # Glyph Type:
    switch(
        type,
        points = {
            ## Non-primitive Glyph
            ## size < 1 --> 8 (area in pixel)
            ## size >= 1 --> 12 * size (area in pixel)
            area <- ifelse(size < 1, 8, 12 * size)
            # pixel(unit)
            diameter.px <- sqrt(area/pi) * 2
            # pixel(unit) to pt(unit)
            diameter.px * px2pt(adjust = adjust)
        },
        texts = {
            ## Text Glyph
            ## size < 1 --> 2 (area in pixel)
            ## size >= 1 --> 2 + size (area in pixel)
            area <- ifelse(size < 1, 2, 2 + size)
            # pixel(unit) to pt(unit)
            area * px2pt(adjust = adjust)
        },
        images = {
            args <- list(...)
            # ratio = height/width
            ratio <- args$ratio
            ## Image Glyph
            ## size < 1 --> 20 (area in pixel)
            ## size >= 1 --> 600 * size (area in pixel)
            area <- ifelse(size < 1, 20, 600 * size)
            # height
            height.px <- sqrt(area * ratio)
            # output unit is cm
            height.px * px2cm(adjust = adjust)
        },
        polygon = {
            # output unit is cm
            ifelse(size < 1, 4, 6 * sqrt(size)) * px2cm(adjust = adjust)
        },
        radial = {
            area <- ifelse(size < 1, 25, 400 * size)
            # pixel(unit)
            diameter.px <- sqrt(area/pi) * 2
            # output unit is cm
            diameter.px * px2cm(adjust = adjust)
        },
        parallel = {
            args <- list(...)
            # ratio = height/width
            p <- args$p
            area <- ifelse(size < 1, 9 * (p - 1), 64 * (p - 1) * size)
            # height:width = 1:2
            # return height
            sqrt(area/2) * px2cm(adjust = adjust)
        },
        lines = {
            # output unit is mm
            # suppose the unit of loon is in px
            # size/(cm2px()/10 * ggplot2::.pt)
            # suppose the unit of loon is in mm
            size * px2pt(adjust = adjust)
        }
    )
}

pt2px <- function(adjust = 1) 4/3 * adjust
px2pt <- function(adjust = 1) 3/4 * adjust
cm2px <- function(adjust = 1) {
    grid::convertUnit(grid::unit(1, "cm"), "pt",
                      valueOnly = TRUE) * pt2px(adjust = adjust)
}
px2cm <- function(adjust = 1) {
    grid::convertUnit(grid::unit(1, "pt"), "cm",
                      valueOnly = TRUE) * px2pt(adjust = adjust)
}
