
# Convenience Function to internally work with glyphs
l_glyph <- function(widget, ...) {
    l_throwErrorIfNotLoonWidget(widget)
    as.character(tcl(widget, "glyph", ...))
}


#' @title Relabel Glyph
#'
#' @description Change the label of a glyph. Note that the label is only
#'   displayed in the glyph inspector.
#' @family glyph functions
#'
#' @template param_widget
#' @param id glyph id
#' @param label new label
#' @export
#'
#'
#' @examples
#' p <- l_plot(iris, color = iris$Species)
#' g <- l_glyph_add_text(p, iris$Species, "test_label")
#' p['glyph'] <- g
#' l_glyph_relabel(p, g, "Species")
l_glyph_relabel <- function(widget, id, label) {
    l_glyph(widget, "relabel", id, label)
    invisible()
}


#' @title Delete a Glyph
#'
#' @description Delete a glyph from the plot.
#' @family glyph functions
#'
#' @inheritParams l_glyph_relabel
#'
#' @seealso \code{\link{l_glyph_add}}
#'
#' @export
l_glyph_delete <- function(widget, id) {
    l_glyph(widget, "delete", id)
    invisible()
}

#' @title List glyphs ids
#'
#' @description List all the non-primitive glyph ids attached to display.
#' @family glyph functions
#'
#' @inheritParams l_glyph_relabel
#'
#' @seealso \code{\link{l_glyph_add}}
#'
#' @export
l_glyph_ids <- function(widget) {
    l_glyph(widget, "ids")
}


#' @title Get Glyph Label
#'
#' @description Returns the label of a glyph
#' @family glyph functions
#'
#' @inheritParams l_glyph_relabel
#'
#' @seealso \code{\link{l_glyph_add}}, \code{\link{l_glyph_ids}},
#'   \code{\link{l_glyph_relabel}}
#'
#' @export
l_glyph_getLabel <- function(widget, id) {
    paste(l_glyph(widget, "getLabel", id), collapse=' ')
}

#' @title Get Glyph Type
#'
#' @description Query the type of a glyph
#' @family glyph functions
#'
#' @inheritParams l_glyph_relabel
#'
#' @seealso \code{\link{l_glyph_add}}
#'
#' @export
l_glyph_getType <- function(widget, id) {

    if (id %in% l_primitiveGlyphs()) {
        "primitive_glyph"
    } else {
        l_glyph(widget, "getType", id)
    }
}
#' @title The primitive glyphs available to a scatterplot or graph display
#'
#' @description Returns a vector of the available primitive glyphs.
#' @family glyph functions
#'
#'
#' @details The scatterplot and graph displays both have the n-dimensional state
#'   \code{'glyph'} that assigns each data point or graph node a glyph (i.e. a
#' visual representation).
#'
#' Loon distinguishes between primitive and non-primitive glyphs: the primitive
#' glyphs are always available for use whereas the non-primitive glyphs need to
#' be first specified and added to a plot before they can be used.
#'
#' The primitive glyphs are:
#'
#' \ifelse{html}{\figure{point_glyph_primitive_types.png}{options: alt="Primitive Glyphs"}}{
#' \tabular{l}{
#'   \code{'circle'}, \code{'ocircle'}, \code{'ccircle'}\cr
#'   \code{'square'}, \code{'osquare'}, \code{'csquare'}\cr
#'   \code{'triangle'}, \code{'otriangle'}, \code{'ctriangle'}\cr
#'   \code{'diamond'}, \code{'odiamond'}, \code{'cdiamond'}
#' }
#'
#' Note that the letter \code{'o'} stands for outline only, and the letter
#' \code{'c'} stands for contrast and adds an outline with the
#' \code{'foreground'} color (black by default).
#' }
#'
#' @templateVar page learn_R_display_plot
#' @templateVar section glyphs
#' @template see_l_help
#'
#'
#' @return A character vector of the names of all primitive glyphs in loon.
#'
#' @export
l_primitiveGlyphs <- function() {
    c("circle",
      "ocircle" ,
      "ccircle",
      "square" ,
      "osquare",
      "csquare",
      "triangle",
      "otriangle",
      "ctriangle",
      "diamond",
      "odiamond",
      "cdiamond")
}


#' @title Add non-primitive glyphs to a scatterplot or graph display
#'
#' @description Generic method for adding user-defined glyphs. See details for
#'   more information about non-primitive and primitive glyphs.
#' @family glyph functions
#'
#' @template param_widget
#' @param type object used for method dispatch
#' @param ... arguments passed on to method
#'
#' @details The scatterplot and graph displays both have the n-dimensional state
#'   \code{'glyph'} that assigns each data point or graph node a glyph (i.e. a
#' visual representation).
#'
#' Loon distinguishes between primitive and non-primitive glyphs: the primitive
#' glyphs are always available for use whereas the non-primitive glyphs need to
#' be first specified and added to a plot before they can be used.
#'
#' The primitive glyphs are:
#'
#' \ifelse{html}{\figure{point_glyph_primitive_types.png}{options: alt="Primitive Glyphs"}}{
#' \tabular{l}{
#'   \code{'circle'}, \code{'ocircle'}, \code{'ccircle'}\cr
#'   \code{'square'}, \code{'osquare'}, \code{'csquare'}\cr
#'   \code{'triangle'}, \code{'otriangle'}, \code{'ctriangle'}\cr
#'   \code{'diamond'}, \code{'odiamond'}, \code{'cdiamond'}
#' }
#'
#' Note that the letter \code{'o'} stands for outline only, and the letter
#' \code{'c'} stands for contrast and adds an outline with the
#' \code{'foreground'} color (black by default).
#' }
#'
#' The non-primitive glyph types and their creator functions are:
#'
#' \if{html}{\figure{display_plot_glyphs_nonprimitive.png}{options: alt="Non-Primitive Glyphs"}}
#'
#' \tabular{ll}{
#'   Type \tab R creator function\cr
#'   Text \tab \code{\link{l_glyph_add_text}}\cr
#'   Serialaxes \tab \code{\link{l_glyph_add_serialaxes}}\cr
#'   Pointranges \tab \code{\link{l_glyph_add_pointrange}}\cr
#'   Images \tab \code{\link{l_glyph_add_image}}\cr
#'   Polygon \tab \code{\link{l_glyph_add_polygon}}
#' }
#'
#' When adding non-primitive glyphs to a display, the number of glyphs needs to
#' match the dimension \code{n} of the plot. In other words, a glyph needs to be
#' defined for each observations. See in the examples.
#'
#' Currently loon does not support compound glyphs. However, it is possible to
#' cunstruct an arbitrary glyph using any system and save it as a png and then
#' re-import them as as image glyphs using \code{\link{l_glyph_add_image}}.
#'
#' @templateVar page learn_R_display_plot
#' @templateVar section glyphs
#' @template see_l_help
#'
#'
#' @return String with glyph id. Every set of non-primitive glyphs has an id
#'   (character).
#'
#' @export
#'
#' @seealso \code{\link{l_glyph_add_text}}, \code{\link{l_make_glyphs}}
#'
#' @examples
#' # Simple Example with Text Glyphs
#' p <- with(olive, l_plot(stearic, eicosenoic, color=Region))
#' g <- l_glyph_add_text(p, text=olive$Area, label="Area")
#' p['glyph'] <- g
#'
#' \dontrun{
#' demo("l_glyphs", package="loon")
#' }
#'
#' # create a plot that demonstrates the primitive glyphs and the text glyphs
#' p <- l_plot(x=1:15, y=rep(0,15), size=10, showLabels=FALSE)
#' text_glyph <- l_glyph_add_text(p, text=letters [1:15])
#' p['glyph'] <- c(
#'     'circle', 'ocircle', 'ccircle',
#'     'square', 'osquare' , 'csquare',
#'     'triangle', 'otriangle', 'ctriangle',
#'     'diamond', 'odiamond', 'cdiamond',
#'     rep(text_glyph, 3)
#' )
l_glyph_add <- function(widget, type, ...) {
    UseMethod("l_glyph_add", type)
}


#' @title Default method for adding non-primitive glyphs
#'
#' @description Generic function to write new glyph types using loon's primitive
#'   glyphs
#' @family glyph functions
#'
#' @template param_widget
#' @param type loon-native non-primitive glyph type, one of \code{'text'},
#'   \code{'serialaxes'}, \code{'image'}, \code{'[polygon'}, or
#'   \code{'pointrange'}
#' @param label label of a glyph (currently shown only in the glyph inspector)
#' @param ... state arguments
#'
#' @export
#'
l_glyph_add.default <- function(widget, type, label="", ...) {
    ## as.vector strips attributes
    structure(
        l_glyph(widget, "add", type, label=label, ...),
        widget = as.vector(widget),
        class = c(paste0("l_glyph_", type), "l_glyph", "loon")
    )

}


#' @title Add a Text Glyph
#'
#' @description Each text glyph can be a multiline string.
#' @family glyph functions
#'
#' @inheritParams l_glyph_add.default
#' @param text the text strings for each observartion. If the object is a factor
#'   then the labels get extracted with \code{\link{as.character}}.
#'
#' @export
#'
#' @seealso \code{\link{l_glyph_add}}
#'
#' @examples
#' p <- l_plot(iris, color = iris$Species)
#' g <- l_glyph_add_text(p, iris$Species, "test_label")
#' p['glyph'] <- g
l_glyph_add_text <- function(widget, text, label="", ...) {

    if (is.factor(text))
        text <- as.character(text)

    l_glyph_add.default(widget, "text", text=text, label=label, ...)
}


#' @title Add a Pointrange Glyph
#'
#' @description Pointrange glyphs show a filled circle at the x-y location and
#'   also a y-range.
#' @family glyph functions
#'
#' @inheritParams l_glyph_add.default
#' @param ymin vector with lower y-yalue of the point range.
#' @param ymax vector with upper y-yalue of the point range.
#' @param linewidth line with in pixel.
#' @param showArea boolean, show a filled point or just the outline point
#'
#' @export
#'
#' @seealso \code{\link{l_glyph_add}}
#'
#' @examples
#' p <- l_plot(x = 1:3, color = c('red', 'blue', 'green'), showScales=TRUE)
#' g <- l_glyph_add_pointrange(p, ymin=(1:3)-(1:3)/5, ymax=(1:3)+(1:3)/5)
#' p['glyph'] <- g
l_glyph_add_pointrange <- function(widget, ymin, ymax, linewidth=1, showArea = TRUE, label="", ...) {
    l_glyph_add.default(widget, "pointrange",
                        ymin=ymin, ymax=ymax, linewidth=linewidth, showArea = showArea,
                        label=label, ...)
}


#' @title Add a Polygon Glyph
#'
#' @description Add one polygon per scatterplot point.
#' @family glyph functions
#'
#' @details A polygon can be a useful point glyph to visualize arbitrary shapes
#'   such as airplanes, animals and shapes that are not available in the
#'   primitive glyph types (e.g. cross). The \code{l_glyphs} demo has an example
#'   of polygon glyphs which we reuse here.
#'
#' @inheritParams l_glyph_add.default
#' @param x nested list of x-coordinates of polygons (relative to ), one list element for each
#'   scatterplot point.
#' @param y nested list of y-coordinates of polygons, one list element for each
#'   scatterplot point.
#' @param linewidth linewidth of outline.
#' @param showArea boolean, show a filled polygon or just the outline
#'
#'
#' @export
#'
#' @seealso \code{\link{l_glyph_add}}
#'
#' @examples
#' x_star <-
#'     c(-0.000864304235090734, 0.292999135695765, 0.949870354364736,
#'       0.474503025064823, 0.586862575626621, -0.000864304235090734,
#'       -0.586430423509075, -0.474070872947277, -0.949438202247191,
#'       -0.29256698357822)
#' y_star <-
#'     c(-1, -0.403630077787381, -0.308556611927398, 0.153846153846154,
#'       0.808556611927398, 0.499567847882455, 0.808556611927398,
#'       0.153846153846154, -0.308556611927398, -0.403630077787381)
#' x_cross <-
#'     c(-0.258931143762604, -0.258931143762604, -0.950374531835206,
#'       -0.950374531835206, -0.258931143762604, -0.258931143762604,
#'       0.259651397291847, 0.259651397291847, 0.948934024776722,
#'       0.948934024776722, 0.259651397291847, 0.259651397291847)
#' y_cross <-
#'     c(-0.950374531835206, -0.258931143762604, -0.258931143762604,
#'       0.259651397291847, 0.259651397291847, 0.948934024776722,
#'       0.948934024776722, 0.259651397291847, 0.259651397291847,
#'       -0.258931143762604, -0.258931143762604, -0.950374531835206)
#' x_hexagon <-
#'     c(0.773552290406223, 0, -0.773552290406223, -0.773552290406223,
#'       0, 0.773552290406223)
#' y_hexagon <-
#'     c(0.446917314894843, 0.894194756554307, 0.446917314894843,
#'       -0.447637568424085, -0.892754249495822, -0.447637568424085)
#'
#' p <- l_plot(1:3, 1:3)
#'
#' gl <- l_glyph_add_polygon(p, x = list(x_star, x_cross, x_hexagon),
#'                           y = list(y_star, y_cross, y_hexagon))
#'
#' p['glyph'] <- gl
#'
#' gl['showArea'] <- FALSE
l_glyph_add_polygon <- function(widget, x, y, linewidth = 1, showArea=TRUE, label="", ...) {

    if (is.list(x))
        x <- l_Rlist2nestedTclList(x)
    if (is.list(y))
        y <- l_Rlist2nestedTclList(y)

    l_glyph_add.default(widget, "polygon",
                        x=x, y=y, linewidth = linewidth, showArea=showArea, label=label, ...)
}


#' @title Add a Serialaxes Glyph
#'
#' @description Serialaxes glyph show either a star glyph or a parralel
#'   coordinate glyph for each point.
#'
#' @family glyph functions
#' @inheritParams l_glyph_add.default
#' @inheritParams l_serialaxes
#' @param linewidth linewidth of outline
#' @param axesColor color of axes
#' @param showEnclosing boolean, circle (axesLayout=radial) or sqaure
#'   (axesLayout=parallel) to show bounding box/circle of the glyph (or showing
#'   unit circle or rectangle with height 1 if scaling=none)
#' @param bboxColor color of bounding box/circle
#'
#' @export
#'
#' @examples
#' p <- with(olive, l_plot(oleic, stearic, color=Area))
#' gs <- l_glyph_add_serialaxes(p, data=olive[,-c(1,2)], showArea=FALSE)
#' p['glyph'] <- gs
l_glyph_add_serialaxes <- function(widget,
                                   data,
                                   sequence,
                                   linewidth=1,
                                   scaling="variable",
                                   axesLayout="radial",
                                   showAxes=FALSE,
                                   axesColor="gray70",
                                   showEnclosing=FALSE,
                                   bboxColor="gray70",
                                   label="",
                                   ...) {

    if(missing(sequence)) {
        sequence <- names(data)
    }

    l_glyph_add.default(widget, "serialaxes",
                        data = l_data(data),
                        sequence=sequence,
                        linewidth=linewidth,
                        scaling=scaling,
                        axesLayout=axesLayout,
                        showAxes=showAxes,
                        axesColor=axesColor,
                        showEnclosing=showEnclosing,
                        bboxColor=bboxColor,
                        label=label ,...)
}


