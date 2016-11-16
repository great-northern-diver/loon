
#' @title Convenience Function to internally work with glyphs
l_glyph <- function(widget, ...) {
    l_throwErrorIfNotLoonWidget(widget)
    as.character(tcl(widget, "glyph", ...))
}

#' @export
l_glyph_relabel <- function(widget, id, label) {
    l_glyph(widget, "relabel", id, label)
    invisible()
}

#' @export
l_glyph_delete <- function(widget, id) {
    l_glyph(widget, "delete", id)
    invisible()
}

#' @export
l_glyph_ids <- function(widget) {
    l_glyph(widget, "ids")
}

#' @export
l_glyph_getLabel <- function(widget, id) {
    paste(l_glyph(widget, "getLabel", id), collapse=' ')
}

#' @export
l_glyph_getType <- function(widget, id) {
    l_glyph(widget, "getType", id)
}


#' @title Add non-primitive glyphs to a scatterplot or graph display
#' 
#' @description Generic method for adding user-defined glyphs. See details for
#'   more information about non-primitive and primitive glyphs.
#' 
#' @template param_widget
#' @param x object used for method dispatch
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
#' When adding non-primitive glyphs to a display, the number of gplyphs needs to
#' match the dimension \code{n} of the plot. In other words, a glyph needs to be
#' defined for each observations.
#' 
#' @templateVar page learn_R_display_plot
#' @templateVar section glyphs
#' @template see_l_help
#' 
#' 
#' @return Every non-primitive glyphs has an id (character).
#' 
#' @export
#' 
#' @seealso \code{\link{l_glyph_add_text}}
#' 
#' @examples
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
l_glyph_add <- function(widget, x, ...) {
    UseMethod("l_glyph_add", x)
}


#' @title Default method for adding non-primitive and loon-native glyphs
#'
#' @description 
#' 
#' 
#' @template param_widget
#' @param x loon-native non-primitive glyph type, one of \code{'text'}, 
#'   \code{'serialaxes'}, \code{'image'}, \code{'[polygon'}, or
#'   \code{'pointrange'}
#' @param ... state arguments
#'   
#' 
#' @seealso \code{\link{l_glyph_add}}  
#' @export
#' 
#' 
l_glyph_add.default <- function(widget, x, ...) {
    ## as.vector strips attributes
    structure(l_glyph(widget, "add", x, ...),
              widget=as.vector(widget), class = c("loon","l_glyph"))
}


#' @export
l_glyph_add_text <- function(widget, text, label="", ...) {
    return(l_glyph_add.default(widget, "text",
                       text=text, label=label, ...))
}


#' @export
l_glyph_add_pointrange <- function(widget, ymin, ymax, linewidth=1, label="", ...) {
    return(l_glyph_add.default(widget, "pointrange",
                       ymin=ymin, ymax=ymax, linewidth=linewidth,
                       label=label, ...))
}


#' @export
l_glyph_add_polygon <- function(widget, x, y, showArea=TRUE, label="", ...) {

    if (is.list(x))
        x <- l_Rlist2nestedTclList(x)
    if (is.list(y))
        y <- l_Rlist2nestedTclList(y)
    
    return(l_glyph_add.default(widget, "polygon",
                       x=x, y=y, showArea=showArea, label=label, ...))
}


#' @export
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
    
    return(l_glyph_add.default(widget, "serialaxes",
                       data=l_data(data),
                       sequence=sequence,
                       linewidth=linewidth,
                       scaling=scaling,
                       axesLayout=axesLayout,
                       showAxes=showAxes,
                       axesColor=axesColor,
                       showEnclosing=showEnclosing,
                       bboxColor=bboxColor,
                       label=label ,...))
}


