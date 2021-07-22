#' @title Turn a \code{loon} size to a \code{grid} size
#' @description The size of \code{loon} is determined by pixel (px), while, in
#' \code{grid} graphics, the size is determined by pointsize (pt)
#' @param size input \code{loon} size
#' @param type glyph type; one of "points", "texts", "images",
#' "radial", "parallel", "polygon", "lines".
#' @param adjust a pixel (px) at 96\code{DPI} (dots per inch) is equal to 0.75 point.
#' However, for different machines, the \code{DPI} is slightly different.
#' Argument \code{adjust} is used to twist the size. IT IS A HACK and should be removed
#' in the later version.
#' @param ... some arguments used to specify the size, e.g. \code{pch} for "points",
#' \code{ratio} for "image" and \code{p} for "parallel".
#'
#' @export
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
            pch <- list(...)$pch
            if(is.null(pch)) pch <- 19
            ## Non-primitive Glyph
            ## size < 1 --> 8 (area in pixel)
            ## size >= 1 --> 12 * size (area in pixel)
            area <- ifelse(size < 1, 8, 12 * size)
            len <- length(area)

            if(length(pch) != len) {
                pch <- rep_len(pch, len)
            } else NULL

            side.px <- vapply(seq(len),
                   function(i) {
                       p <- pch[i]
                       a <- area[i]
                       if(p %in% c(19, 1, 21)) {
                           # circle diameter
                           sqrt(a/pi) * 2
                       } else if(p %in% c(15, 0, 22)) {
                           # square side
                           sqrt(a)
                       } else if(p %in% c(17, 2, 24)) {
                           # triangle side
                           sqrt(a * 4/sqrt(3))
                       } else if(p %in% c(18, 5, 23)) {
                           # diamond side
                           sqrt(a)
                       } else {
                           # default
                           # circle diameter
                           sqrt(a/pi) * 2
                       }
                   }, numeric(1L))

            side.px * px2pt(adjust = adjust)
        },
        texts = {
            ## Text Glyph
            ## size < 1 --> 2 (area in pixel)
            ## size >= 1 --> 2 + size (area in pixel)
            s <- ifelse(size < 1, 2, 2 + size)
            # pixel(unit) to pt(unit)
            s * px2pt(adjust = adjust)
        },
        images = {
            args <- list(...)
            # ratio = height/width
            ratio <- args$ratio
            if(is.null(ratio)) ratio <- 1
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
            if(is.null(p)) p <- 5
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
    # grid::convertUnit(grid::unit(1, "cm"), "pt",
    #                   valueOnly = TRUE)
    28.45276 * pt2px(adjust = adjust)
}
px2cm <- function(adjust = 1) {
    # grid::convertUnit(grid::unit(1, "pt"), "cm",
    #                   valueOnly = TRUE)
    0.03514598 * px2pt(adjust = adjust)
}
