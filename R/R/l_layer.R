
#' @export
l_layer <- function(widget, x, ...) {
    UseMethod("l_layer", x, ...)
}

#' @export
l_layer.density <- function(widget, x, ...) {
    l_layer_line(widget, x$x, x$y, ...)
}

l_layer_add <- function(widget, type, ...) {
    structure(as.character(tcl(widget, "layer" , type, ...)),
              widget=as.vector(widget), class=c("loon","l_layer"))
}



#' @title List ids of layers in Plot
#'
#' @inheritParams l_widget
#'
#' @export
l_layer_ids <- function(widget) {
    l_throwErrorIfNotLoonWidget(widget)
    
    return(as.character(tcl(widget, 'layer', 'ids')))
}

#' 
#' @templateVar type group
#' @template title_layer
#'
#' @description
#' A group layer can contain other layers. If the group layer is
#' invisible, then so are all its children.
#'
#' @template layerid
#'
#' @inheritParams l_widget
#' @param label label used in the layers inspector
#' @param parent group layer
#' @param index of the newly added layer in its parent group
#'
#' @template return_layerid
#' 
#' @export
#'
#' @template examples_layer
#' 

l_layer_group <- function(widget, label="group", parent="root", index=0) {

    l_throwErrorIfNotLoonWidget(widget)
    
    l_layer_add(widget, 'group',
                label=label, parent=parent, index=index)
}


#' @templateVar type polygon
#' @template title_layer
#' 
#' @inheritParams l_layer_group
#' 
#' @export
l_layer_polygon <- function(widget, x, y,
                            color="gray80", linecolor="black", linewidth=1,
                            label="polygon", parent="root", index=0, ...) {
    l_throwErrorIfNotLoonWidget(widget)
    l_layer_add(widget, 'polygon',
                x=x, y=y, color=color,
                linecolor=linecolor,
                linewidth=linewidth,
                label=label, parent=parent, index=index, ...)
}


#' @templateVar type polygons
#' @template title_layer
#' 
#' @inheritParams l_layer_group
#' 
#' @export
l_layer_polygons <- function(widget, x, y,
                             color="gray80", linecolor="black", linewidth=1,
                             label="polygons", parent="root", index=0,...) {
    l_throwErrorIfNotLoonWidget(widget)
    l_layer_add(widget, 'polygons',
                x=l_Rlist2nestedTclList(x),
                y=l_Rlist2nestedTclList(y),
                color=color,
                linecolor=linecolor,
                linewidth=linewidth,
                label=label, parent=parent, index=index, ...)
}


#' @templateVar type polygons
#' @template title_layer
#' 
#' @inheritParams l_layer_group
#' 
#' @export
l_layer_rectangles <- function(widget, x, y,
                             color="gray80", linecolor="black", linewidth=1,
                             label="rectangles", parent="root", index=0,...) {
    l_throwErrorIfNotLoonWidget(widget)
    l_layer_add(widget, 'rectangles',
                x=l_Rlist2nestedTclList(x),
                y=l_Rlist2nestedTclList(y),
                color=color,
                linecolor=linecolor,
                linewidth=linewidth,
                label=label, parent=parent, index=index, ...)
}

#' @templateVar type polygons
#' @template title_layer
#' 
#' @inheritParams l_layer_group
#' 
#' @export
l_layer_lines <- function(widget, x, y,
                          color="black", linewidth=1,
                          label="lines", parent="root", index=0,...) {
    l_throwErrorIfNotLoonWidget(widget)
    l_layer_add(widget, 'lines',
                x=l_Rlist2nestedTclList(x),
                y=l_Rlist2nestedTclList(y),
                color=color,
                linewidth=linewidth,
                label=label, parent=parent, index=index, ...)
}




#' @templateVar type polygon
#' @template title_layer
#' 
#' @inheritParams l_layer_group
#' 
#' @export
l_layer_rectangle <- function(widget, x, y,
                              color="gray80", linecolor="black", linewidth=1,
                              label="rectangle", parent="root", index=0, ...) {
    l_throwErrorIfNotLoonWidget(widget)
    
    l_layer_add(widget, 'rectangle',
                x=x, y=y, color=color,
                linecolor=linecolor,
                linewidth=linewidth,
                label=label, parent=parent, index=index, ...)
}


#' @templateVar type polygon
#' @template title_layer
#' 
#' @inheritParams l_layer_group
#' 
#' @export
l_layer_oval <- function(widget, x, y,
                         color="gray80", linecolor="black", linewidth=1,
                         label="oval", parent="root", index=0, ...) {
    l_throwErrorIfNotLoonWidget(widget)

    l_layer_add(widget, 'oval',
                x=x, y=y, color=color,
                linecolor=linecolor,
                linewidth=linewidth,
                label=label, parent=parent, index=index, ...)
}



#' @templateVar type line
#' @template title_layer
#' 
#' @inheritParams l_layer_group
#' 
#' @export
l_layer_line <- function(widget, x, y=NULL, color="black",
                         linewidth=1, dash="",
                         label="line", parent="root", index=0, ...) {
    
    l_throwErrorIfNotLoonWidget(widget)

    xy <- try(xy.coords(x, y))

    l_layer_add(widget, 'line',
                x=xy$x, y=xy$y, color=color,
                linewidth=linewidth, dash=dash,
                label=label, parent=parent, index=index, ...)
}


#' @templateVar type points
#' @template title_layer
#' 
#' @inheritParams l_layer_group
#' 
#' @export
l_layer_points <- function(widget, x, y=NULL, color="gray60", size=6,
                           label="points", parent="root", index=0, ...) {
    
    l_throwErrorIfNotLoonWidget(widget)

    xy <- try(xy.coords(x, y))
    
    l_layer_add(widget, 'points',
                x=xy$x, y=xy$y, color=color,
                size=size,
                label=label, parent=parent, index=index, ...)
}

#' @export
l_layer_text <- function(widget, x, y, text, color="gray60", size=6, angle=0,
                         label="text", parent="root", index=0, ...) {
    
    l_throwErrorIfNotLoonWidget(widget)
    l_layer_add(widget, 'text',
                x=x, y=y, text=text, color=color,
                size=size, angle=angle,
                label=label, parent=parent, index=index, ...)
}

#' @templateVar type text
#' @template title_layer
#' 
#' @inheritParams l_layer_group
#' 
#' @export
l_layer_texts <- function(widget, x, y, text, color="gray60", size=6, angle=0,
                         label="texts", parent="root", index=0, ...) {

    l_throwErrorIfNotLoonWidget(widget)
    l_layer_add(widget, 'texts',
                x=x, y=y, text=text, color=color,
                size=size, angle=angle,
                label=label, parent=parent, index=index, ...)
}








#' Delete a layer
#' @export
l_layer_delete <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'delete', layer)
}

#' Delete a layer and all its descendants 
#' @export
l_layer_expunge <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'expunge', layer)
}

#' Move a layer
#'
#' @inheritParams l_layer_group
#' 
#' @export
l_layer_move <- function(widget, layer, parent="root", index="0") {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'move', layer, parent, index)
    invisible()
}

#' Hide a layer
#' @export
l_layer_hide <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'hide', layer)
    invisible()
}

#' Show or un-hide a layer
#' @export
l_layer_show <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'show', layer)
    invisible()
}

#' Relabel a layer
#' @export
l_layer_relabel <- function(widget, layer, label) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'relabel', layer, label)
    invisible()
}

#' Move a layer above its sibling
#' @export
l_layer_raise <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'raise', layer)
    invisible()
}

#' Lower a layer below its sibling
#' @export
l_layer_lower <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'lower', layer)
    invisible()
}

#' Move layer into its parent group
#' @export
l_layer_promote <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'promote', layer)
    invisible()
}

#' Move layer into its right hand side sibling layer group
#' @export
l_layer_demote <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'demote', layer)
    invisible()
}

#' Returns the bounding box of a layer
#' @export
l_layer_bbox <- function(widget, layer="root") {
    l_throwErrorIfNotLoonWidget(widget)
    as.numeric(tcl(widget, 'layer', 'bbox', layer))
}


#' @export
l_layer_getLabel <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    return(sapply(layer, FUN=function(l){paste(as.character(tcl(widget, 'layer', 'getLabel', l)), collapse=' ')}))
}

#' @export
l_layer_getChildren <- function(widget, parent='root') {
    l_throwErrorIfNotLoonWidget(widget)    
    as.character(tcl(widget,'layer','getChildren', parent))
}

#' @export
l_layer_getParent <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)    
    as.character(tcl(widget,'layer','getParent', parent))
}


#' @export
l_layer_printTree <- function(widget) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'printTree')
    invisible()
}

#' @export
l_layer_isVisible <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    as.logical(as.character(tcl(widget, 'layer', 'isVisible', layer)))
}

#' @export
l_layer_getType <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    as.character(tcl(widget, 'layer', 'getType', layer))
}

#' @export
l_layer_index <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    as.numeric(tcl(widget, 'layer', 'index', layer))
}

#' @export
l_layer_layerVisibility <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    as.numeric(tcl(widget, 'layer', 'layerVisibility', layer))
}

#' @export
l_layer_groupVisibility <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    as.character(tcl(widget, 'layer', 'groupVisibility', layer))
}



