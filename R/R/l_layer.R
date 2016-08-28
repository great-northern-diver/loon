#' @title Loon layers
#'   
#' @description Loon supports layering of visuals and groups of visuals. The 
#'   \code{l_layer} function is a generic method.
#'   
#' @aliases print.l_layer
#'   
#' @details loon's displays that use the main graphics model (i.e. histogram, 
#'   scatterplot and graph displays) support layering of visual information. The
#'   following table lists the layer types and functions for layering on a 
#'   display.
#'   
#'   \tabular{lll}{
#'   \strong{Type} \tab \strong{Description} \tab \strong{Creator Function}\cr
#'   group \tab a group can be a parent of other layers \tab \code{\link{l_layer_group}}\cr
#'   polygon \tab one polygon \tab \code{\link{l_layer_polygon}}\cr
#'   text \tab one text string \tab \code{\link{l_layer_text}}\cr
#'   line \tab one line (i.e. connected line segments) \tab \code{\link{l_layer_line}}\cr
#'   rectangle \tab one rectangle \tab \code{\link{l_layer_rectangle}}\cr
#'   oval \tab one oval \tab \code{\link{l_layer_oval}}\cr
#'   points \tab n points (filled) circle \tab \code{\link{l_layer_points}}\cr
#'   texts \tab n text strings \tab \code{\link{l_layer_text}}\cr
#'   polygons \tab n polygons \tab \code{\link{l_layer_polygons}}\cr
#'   rectangles \tab n rectangles \tab \code{\link{l_layer_rectangles}}\cr
#'   lines \tab n sets of connected line segments \tab \code{\link{l_layer_lines}}\cr 
#'   }
#'   
#'   Every layer within a display has a unique id. The visuals of the data in a 
#'   display present the default layer of that display and has the layer id 
#'   \code{'model'}. For example, the \code{'model'} layer of a scatterplot 
#'   display visualizes the scatterplot glyphs. Functions useful to query layers
#'   are
#'   
#'   \tabular{ll}{
#'   \strong{Function} \tab \strong{Description}\cr
#'   \code{\link{l_layer_ids}} \tab List layer ids\cr
#'   \code{\link{l_layer_getType}} \tab Get layer type
#'   }
#'   
#'   Layers are arranged in a tree structure with the tree root having the layer
#'   id \code{'root'}. The rendering order of the layers is according to a 
#'   depth-first traversal of the layer tree. This tree also maintains a label 
#'   and a visibility flag for each layer. The layer tree, layer ids, layer 
#'   labels and the visibility of each layer are visualized in the layers 
#'   inspector. If a layer is set to be invisible then it is not rendered on the
#'   display. If a group layer is set to be invisible then all its children are 
#'   not rendered; however, the visibility flag of the children layers remain 
#'   unchanged. Relevant functions are:
#'   
#'   \tabular{ll}{
#'   \strong{Function} \tab \strong{Description}\cr
#'   \code{\link{l_layer_getParent}} \tab Get parent layer id of a layer\cr
#'   \code{\link{l_layer_getChildren}} \tab Get children of a group layer\cr
#'   \code{\link{l_layer_index}} \tab Get the order index of a layer among its siblings\cr
#'   \code{\link{l_layer_printTree}} \tab Print out the layer tree\cr
#'   \code{\link{l_layer_move}} \tab Move a layer\cr
#'   \code{\link{l_layer_lower}} \tab Switch the layer place with its sibling to the right\cr
#'   \code{\link{l_layer_raise}} \tab Switch the layer place with its sibling to the left\cr
#'   \code{\link{l_layer_demote}} \tab Moves the layer up to be a left sibling of its parent\cr
#'   \code{\link{l_layer_promote}} \tab Moves the layer to be a child of its right group layer sibling \cr
#'   \code{\link{l_layer_hide}} \tab Set the layers visibility flag to \code{FALSE}\cr
#'   \code{\link{l_layer_show}} \tab Set the layers visibility flag to \code{TRUE}\cr
#'   \code{\link{l_layer_isVisible}} \tab Return visibility flag of layer\cr
#'   \code{\link{l_layer_layerVisibility}} \tab Returns logical value for whether layer is actually seen\cr
#'   \code{\link{l_layer_groupVisibility}} \tab Returns \code{all}, \code{part} or \code{none} for expressing which part of the layers children are visible. \cr
#'   \code{\link{l_layer_delete}} \tab Delete a layer. If the layer is a group move all its children layers to the layers parent. \cr
#'   \code{\link{l_layer_expunge}} \tab Delete layer and all its children layer.\cr
#'   \code{\link{l_layer_getLabel}} \tab Get layer label.\cr
#'   \code{\link{l_layer_relabel}} \tab Change layer label.\cr
#'   \code{\link{l_layer_bbox}} \tab Get the bounding box of a layer.
#'   }
#'   
#'   All layers have states that can be queried and modified using the same 
#'   functions as the ones used for displays (i.e. \code{\link{l_cget}}, 
#'   \code{\link{l_configure}}, \code{\link{'['}} and \code{\link{'[<-'}}). The 
#'   last group of layer types in the above table have n-dimensional states, 
#'   where the actual value of n can be different for every layer in a display.
#'   
#'   The difference between the model layer and the other layers is that the 
#'   model layer has a \emph{selected} state, responds to selection gestures and
#'   supports linking.
#'   
#'   
#' @templateVar page learn_R_layer
#' @template see_l_help_page
#'   
#' @template param_widget
#' @param x object that should be layered
#' @param ... additional arguments, often state definition for the basic 
#'   layering function
#'   
#' @seealso \code{\link{l_info_states}}
#'      
#' @template return_layerid
#'   
#' @export
#' 
#' @template examples_layer
l_layer <- function(widget, x, ...) {
    UseMethod("l_layer", x, ...)
}


#' @title Layer Method for Kernel Density Estimation
#'   
#' @description Layer a line that represents a kernel density estimate.
#'
#' @inheritParams l_layer   
#' @param x object from \code{\link{density}} of class \code{"density"} 
#' 
#' @template return_layerid
#'   
#' @export l_layer.density
#'   
#' @seealso \code{\link[stats]{density}}, \code{\link{l_layer}}
#'   
#' @examples  
#' d <- density(faithful$eruptions, bw = "sj")
#' h <- l_hist(x = faithful$eruptions, yshows="density")
#' l <- l_layer.density(h, d, color="steelblue", linewidth=3)
#' 
l_layer.density <- function(widget, x, ...) {
    l_layer_line(widget, x$x, x$y, ...)
}



# helper function to add a layer
l_layer_add <- function(widget, type, ...) {
    structure(as.character(tcl(widget, "layer" , type, ...)),
              widget=as.vector(widget), class=c("loon","l_layer"))
}



#' @title List ids of layers in Plot
#'   
#' @description Every layer within a display has a unique id. This function 
#'   returns a list of all the layer ids for a widget.
#'   
#' @inheritParams l_layer
#' 
#' @details 
#' 
#' @templateVar page learn_R_layer.html
#' @templateVar section add-move-delete-layers
#' @template see_l_help
#' 
#' @return vector with layer ids in rendering order. To create a layer handle
#'   object use \code{\link{l_create_handle}}.
#'   
#' @template seealso_layers  
#' 
#' @export
#' 
#' @template examples_layer_regression_fit
l_layer_ids <- function(widget) {
    l_throwErrorIfNotLoonWidget(widget)
    
    return(as.character(tcl(widget, 'layer', 'ids')))
}

#' @title layer a group node
#'   
#' @templateVar type group
#' @template title_layer
#'   
#' @description A group layer can contain other layers. If the group layer is 
#'   invisible, then so are all its children.
#' 
#' @templateVar page learn_R_layer
#' @template see_l_help_page
#'   
#' @inheritParams l_widget
#' @param label label used in the layers inspector
#' @param parent group layer
#' @param index of the newly added layer in its parent group
#'   
#' @template return_layerid
#' 
#' @template seealso_layers  
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
#' @param x x coordinates
#' @param y y coordinates
#' @param color fill color of polygon
#' @param linecolor outline color of polygon
#' @param linewidth linewidth of outline
#' @param ... additional state initialization arguments, see 
#'   \code{\link{l_info_states}} for more info on the states of a polygon 
#'   object.
#'   
#' @templateVar page learn_R_layer
#' @template see_l_help_page
#'   
#' @template seealso_layers
#'   
#' @export
#' 
#' @template examples_layer_regression_fit
#'   
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
#' @param x list with vectors with x coordinates
#' @param y list with vectors with y coordinates
#' @param color vector with polygon fill colors
#' @param linecolor vector with polygon outline colors
#' @param linewidth vector with line widths of polygons
#' @param ... additional state initialization arguments, see 
#'   \code{\link{l_info_states}} for more info on the states of a polygon 
#'   object.
#'
#' @templateVar page learn_R_layer
#' @template see_l_help_page
#'   
#' @template seealso_layers
#' 
#' @export
#' 
#' @examples 
#' 
#' p <- l_plot()
#' 
#' l <- l_layer_polygons(
#'      p,
#'      x = list(c(1,2,1.5), c(3,4,6,5,2), c(1,3,5,3)),
#'      y = list(c(1,1,2), c(1,1.5,1,4,2), c(3,5,6,4)),
#'      color = c('red', 'green', 'blue'),
#'      linecolor = ""
#' )
#' l_scaleto_world(p)
#' 
#' l_info_states(l, "color")
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


#' @templateVar type rectangles
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

#' @templateVar type lines
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




#' @templateVar type rectangle
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


#' @templateVar type oval
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

#' @templateVar type text
#' @template title_layer
#' 
#' @inheritParams l_layer_group
#' 
#' @export
l_layer_text <- function(widget, x, y, text, color="gray60", size=6, angle=0,
                         label="text", parent="root", index=0, ...) {
    
    l_throwErrorIfNotLoonWidget(widget)
    l_layer_add(widget, 'text',
                x=x, y=y, text=text, color=color,
                size=size, angle=angle,
                label=label, parent=parent, index=index, ...)
}

#' @templateVar type texts
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




#' @title Delete a layer
#' 
#' @export
l_layer_delete <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'delete', layer)
}

#' @title Delete a layer and all its descendants
#'  
#' @export
l_layer_expunge <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'expunge', layer)
}

#' @title Move a layer
#'
#' @inheritParams l_layer_group
#' 
#' @export
l_layer_move <- function(widget, layer, parent="root", index="0") {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'move', layer, parent, index)
    invisible()
}

#' @title Set the layers visibility flag to \code{TRUE}
#' 
#' @export
l_layer_hide <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'hide', layer)
    invisible()
}

#' @title Show or un-hide a layer
#' 
#' @export
l_layer_show <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'show', layer)
    invisible()
}

#' @title Change layer label
#' 
#' @export
l_layer_relabel <- function(widget, layer, label) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'relabel', layer, label)
    invisible()
}

#' @title Switch the layer place with its sibling to the left
#' 
#' @export
l_layer_raise <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'raise', layer)
    invisible()
}

#' @title Switch the layer place with its sibling to the right
#' 
#' @export
l_layer_lower <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'lower', layer)
    invisible()
}

#' @title Moves the layer to be a child of its right group layer sibling
#' 
#' @export
l_layer_promote <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'promote', layer)
    invisible()
}

#' @title Moves the layer up to be a left sibling of its parent
#' 
#' @export
l_layer_demote <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'demote', layer)
    invisible()
}

#' @title Get the bounding box of a layer.
#' 
#' @export
l_layer_bbox <- function(widget, layer="root") {
    l_throwErrorIfNotLoonWidget(widget)
    as.numeric(tcl(widget, 'layer', 'bbox', layer))
}


#' @title Get layer label.
#' 
#' @export
l_layer_getLabel <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    return(sapply(layer, FUN=function(l){paste(as.character(tcl(widget, 'layer', 'getLabel', l)), collapse=' ')}))
}

#' @title Get children of a group layer
#' 
#' @export
l_layer_getChildren <- function(widget, parent='root') {
    l_throwErrorIfNotLoonWidget(widget)    
    as.character(tcl(widget,'layer','getChildren', parent))
}

#' @title Get parent layer id of a layer
#' 
#' @export
l_layer_getParent <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)    
    as.character(tcl(widget,'layer','getParent', parent))
}


#' @title Print the layer tree
#' 
#' @export
l_layer_printTree <- function(widget) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'printTree')
    invisible()
}

#' @title Return visibility flag of layer
#' 
#' @export
l_layer_isVisible <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    as.logical(as.character(tcl(widget, 'layer', 'isVisible', layer)))
}

#' @title Get layer type
#' 
#' @export
l_layer_getType <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    as.character(tcl(widget, 'layer', 'getType', layer))
}

#' @title Get the order index of a layer among its siblings
#' 
#' @export
l_layer_index <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    as.numeric(tcl(widget, 'layer', 'index', layer))
}

#' @title Returns logical value for whether layer is actually seen
#' 
#' @export
l_layer_layerVisibility <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    as.numeric(tcl(widget, 'layer', 'layerVisibility', layer))
}

#' @title Returns all, part or none for expressing which part of the layers
#'   children are visible.
#'   
#' @export
l_layer_groupVisibility <- function(widget, layer) {
    l_throwErrorIfNotLoonWidget(widget)
    as.character(tcl(widget, 'layer', 'groupVisibility', layer))
}



