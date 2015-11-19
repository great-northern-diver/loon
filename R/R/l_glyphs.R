
#' @export
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


#' @export
l_glyph_add <- function(widget, type, ...) {
    ## as.vector strips attributes
    structure(l_glyph(widget, "add", type, ...),
              widget=as.vector(widget), class = c("loon","l_glyph"))
}

#' @export
l_glyph_add_text <- function(widget, text, label="", ...) {
    return(l_glyph_add(widget, "text",
                       text=text, label=label, ...))
}


#' @export
l_glyph_add_pointrange <- function(widget, ymin, ymax, linewidth=1, label="", ...) {
    return(l_glyph_add(widget, "pointrange",
                       ymin=ymin, ymax=ymax, linewidth=linewidth,
                       label=label, ...))
}


#' @export
l_glyph_add_polygon <- function(widget, x, y, showArea=TRUE, label="", ...) {

    if (is.list(x))
        x <- l_Rlist2nestedTclList(x)
    if (is.list(y))
        y <- l_Rlist2nestedTclList(y)
    
    return(l_glyph_add(widget, "polygon",
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
    
    return(l_glyph_add(widget, "serialaxes",
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


