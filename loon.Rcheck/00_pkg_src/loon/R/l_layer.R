#' @title Loon layers
#'   
#' @description Loon supports layering of visuals and groups of visuals. The 
#'   \code{l_layer} function is a generic method.
#'   
#' @details loon's displays that use the main graphics model (i.e. histogram, 
#'   scatterplot and graph displays) support layering of visual information. The
#'   following table lists the layer types and functions for layering on a 
#'   display.
#'   
#'   \tabular{lll}{ \strong{Type} \tab \strong{Description} \tab \strong{Creator
#'   Function}\cr group \tab a group can be a parent of other layers \tab
#'   \code{\link{l_layer_group}}\cr polygon \tab one polygon \tab
#'   \code{\link{l_layer_polygon}}\cr text \tab one text string \tab
#'   \code{\link{l_layer_text}}\cr line \tab one line (i.e. connected line
#'   segments) \tab \code{\link{l_layer_line}}\cr rectangle \tab one rectangle
#'   \tab \code{\link{l_layer_rectangle}}\cr oval \tab one oval \tab
#'   \code{\link{l_layer_oval}}\cr points \tab n points (filled) circle \tab
#'   \code{\link{l_layer_points}}\cr texts \tab n text strings \tab
#'   \code{\link{l_layer_text}}\cr polygons \tab n polygons \tab
#'   \code{\link{l_layer_polygons}}\cr rectangles \tab n rectangles \tab
#'   \code{\link{l_layer_rectangles}}\cr lines \tab n sets of connected line
#'   segments \tab \code{\link{l_layer_lines}}\cr }
#'   
#'   Every layer within a display has a unique id. The visuals of the data in a 
#'   display present the default layer of that display and has the layer id 
#'   \code{'model'}. For example, the \code{'model'} layer of a scatterplot 
#'   display visualizes the scatterplot glyphs. Functions useful to query layers
#'   are
#'   
#'   \tabular{ll}{ \strong{Function} \tab \strong{Description}\cr 
#'   \code{\link{l_layer_ids}} \tab List layer ids\cr 
#'   \code{\link{l_layer_getType}} \tab Get layer type }
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
#'   \tabular{ll}{ \strong{Function} \tab \strong{Description}\cr 
#'   \code{\link{l_layer_getParent}} \tab Get parent layer id of a layer\cr 
#'   \code{\link{l_layer_getChildren}} \tab Get children of a group layer\cr 
#'   \code{\link{l_layer_index}} \tab Get the order index of a layer among its
#'   siblings\cr \code{\link{l_layer_printTree}} \tab Print out the layer
#'   tree\cr \code{\link{l_layer_move}} \tab Move a layer\cr 
#'   \code{\link{l_layer_lower}} \tab Switch the layer place with its sibling to
#'   the right\cr \code{\link{l_layer_raise}} \tab Switch the layer place with
#'   its sibling to the left\cr \code{\link{l_layer_demote}} \tab Moves the
#'   layer up to be a left sibling of its parent\cr 
#'   \code{\link{l_layer_promote}} \tab Moves the layer to be a child of its
#'   right group layer sibling \cr \code{\link{l_layer_hide}} \tab Set the
#'   layers visibility flag to \code{FALSE}\cr \code{\link{l_layer_show}} \tab
#'   Set the layers visibility flag to \code{TRUE}\cr 
#'   \code{\link{l_layer_isVisible}} \tab Return visibility flag of layer\cr 
#'   \code{\link{l_layer_layerVisibility}} \tab Returns logical value for
#'   whether layer is actually seen\cr \code{\link{l_layer_groupVisibility}}
#'   \tab Returns \code{all}, \code{part} or \code{none} for expressing which
#'   part of the layers children are visible. \cr \code{\link{l_layer_delete}}
#'   \tab Delete a layer. If the layer is a group move all its children layers
#'   to the layers parent. \cr \code{\link{l_layer_expunge}} \tab Delete layer
#'   and all its children layer.\cr \code{\link{l_layer_getLabel}} \tab Get
#'   layer label.\cr \code{\link{l_layer_relabel}} \tab Change layer label.\cr 
#'   \code{\link{l_layer_bbox}} \tab Get the bounding box of a layer. }
#'   
#'   All layers have states that can be queried and modified using the same 
#'   functions as the ones used for displays (i.e. \code{\link{l_cget}}, 
#'   \code{\link{l_configure}}, \code{`[`} and \code{`[<-`}). The 
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
#' @seealso \code{\link{l_info_states}}, \code{\link{l_scaleto_layer}},
#'   \code{\link{l_scaleto_world}}
#'   
#' @template return_layerid
#'   
#' @export
#' 
#' @examples 
#' # l_layer is a generic method
#' newFoo <- function(x, y, ...) {
#'   r <- list(x=x, y=y, ...)
#'   class(r) <- 'foo'
#'   return(r)
#' }
#' 
#' l_layer.foo <- function(widget, x) {
#'     x$widget <- widget
#'     id <- do.call('l_layer_polygon', x)
#'     return(id)
#' }
#' 
#' p <- l_plot()
#' 
#' obj <- newFoo(x=c(1:6,6:2), y=c(3,1,0,0,1,3,3,5,6,6,5), color='yellow')
#' 
#' id <- l_layer(p, obj)
#' 
#' l_scaleto_world(p)
l_layer <- function(widget, x, ...) {
    UseMethod("l_layer", x)
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
#' @seealso \code{\link[stats]{density}}, \code{\link{l_layer}}
#' 
#' @export
#' @export l_layer.density
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
#' @templateVar page learn_R_layer
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
#' @template param_color
#' @param linecolor outline color
#' @param linewidth linewidth of outline
#' @param ... additional state initialization arguments, see 
#'   \code{\link{l_info_states}}
#'   
#' @templateVar page learn_R_layer
#' @template see_l_help_page
#' 
#' @template return_layerid
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
#' @param color vector with fill colors, if empty string \code{""}, then the
#'   fill is transparant
#' @param linecolor vector with outline colors
#' @param linewidth vector with line widths
#' @param ... additional state initialization arguments, see 
#'   \code{\link{l_info_states}}
#'
#' @templateVar page learn_R_layer
#' @template see_l_help_page
#'   
#' @template return_layerid
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


#' @templateVar type rectangle
#' @template title_layer
#' 
#' @inheritParams l_layer_polygon
#' 
#' @templateVar page learn_R_layer
#' @template see_l_help_page
#'   
#' @template return_layerid
#' 
#' @template seealso_layers
#' 
#' @export
#' 
#' @examples
#' p <- l_plot()
#' l <- l_layer_rectangle(p, x=c(2,3), y=c(1,10), color='steelblue')
#' l_scaleto_layer(l)
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


#' @templateVar type rectangles
#' @template title_layer
#' 
#' @inheritParams l_layer_polygons
#' 
#' @templateVar page learn_R_layer
#' @template see_l_help_page
#'   
#' @template return_layerid
#' 
#' @template seealso_layers
#' 
#' @export
#' 
#' @examples 
#' 
#' p <- l_plot()
#' 
#' l <- l_layer_rectangles(
#'      p,
#'      x = list(c(0,1), c(1,2), c(2,3), c(5,6)),
#'      y = list(c(0,1), c(1,2), c(0,1), c(3,4)),
#'      color = c('red', 'blue', 'green', 'orange'),
#'      linecolor = "black"
#' )
#' l_scaleto_world(p)
#' 
#' l_info_states(l)
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



#' @templateVar type line
#' @template title_layer
#'   
#' @inheritParams l_layer_polygon
#' @param x the coordinates of line. Alternatively, a single plotting structure,
#'   function or any \emph{R} object with a plot method can be provided as x and
#'   y are passed on to \code{\link{xy.coords}}
#' @param y the y coordinates of the line, optional if x is an appropriate 
#'   structure.
#' @param color color of line
#' @param dash dash pattern of line, see 
#'   \url{https://www.tcl.tk/man/tcl8.6/TkCmd/canvas.htm#M26}
#'   
#' @templateVar page learn_R_layer
#' @template see_l_help_page
#'   
#' @template return_layerid
#' 
#' @template seealso_layers
#'   
#' @export
#' @importFrom grDevices xy.coords
#' 
#' 
#' @examples 
#' 
#' p <- l_plot()
#' l <- l_layer_line(p, x=c(1,2,3,4), y=c(1,3,2,4), color='red', linewidth=2)
#' l_scaleto_world(p)
#' 
#' # object
#' p <- l_plot()
#' l <- l_layer_line(p, x=nhtemp)
#' l_scaleto_layer(l)
#' 
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


#' @templateVar type lines
#' @template title_layer
#' 
#' @inheritParams l_layer_polygons
#' @param color color of lines
#' 
#' @templateVar page learn_R_layer
#' @template see_l_help_page
#'   
#' @template return_layerid
#' 
#' @template seealso_layers
#' 
#' @export
#' 
#' @examples
#' s <- Filter(function(df)nrow(df) > 1, split(UsAndThem, UsAndThem$Country))
#' sUaT <- Map(function(country){country[order(country$Year),]} , s)
#' xcoords <- Map(function(x)x$Year, sUaT)
#' ycoords <- Map(function(x)x$LifeExpectancy, sUaT)
#' region <- sapply(sUaT, function(x)as.character(x$Geographic.Region[1]))
#' 
#' p <- l_plot(showItemLabels=TRUE)
#' l <- l_layer_lines(p, xcoords, ycoords, itemLabel=names(sUaT), color=region)
#' l_scaleto_layer(l)
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






#' @templateVar type oval
#' @template title_layer
#' 
#' @inheritParams l_layer_polygon
#' 
#' @templateVar page learn_R_layer
#' @template see_l_help_page
#'   
#' @template return_layerid
#' 
#' @template seealso_layers
#' 
#' @export
#' 
#' @examples 
#' p <- l_plot()
#' l <- l_layer_oval(p, c(1,5), c(2,12), color='steelblue')
#' l_configure(p, panX=0, panY=0, deltaX=20, deltaY=20)
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




#' @templateVar type points
#' @template title_layer
#' 
#' @description Scatter points layer
#' 
#' @inheritParams l_layer_line
#' @param color color of points
#' @param size size point, as for scatterplot model layer
#' 
#' @templateVar page learn_R_layer 
#' @template see_l_help_page
#'   
#' @template return_layerid
#' 
#' @template seealso_layers
#' @export
#' 
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
#' @description layer a single character string
#'   
#' @inheritParams l_layer_polygon
#' @param x coordinate
#' @param y coordinate
#' @param text character string
#' @param color color of text
#' @param size size of the font
#' @param angle rotation of text
#'   
#' @details As a side effect of \code{Tcl}'s text-based design, it is best to
#'   use \code{l_layer_text} if one would like to layer a single character
#'   string (and not \code{\link{l_layer_texts}} with \code{n=1}).
#'   
#' @templateVar page learn_R_layer
#' @template see_l_help_page
#'   
#' @template return_layerid
#' 
#' @template seealso_layers
#' @export
#' 
#' @examples 
#' p <- l_plot()
#' l <- l_layer_text(p, 0, 0, "Hello World")
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
#' @inheritParams l_layer_line
#' @param size font size
#' @param angle text rotation
#' @param text vector with text strings
#' 
#' 
#' @templateVar page learn_R_layer 
#' @template see_l_help_page
#'   
#' @template return_layerid
#' 
#' @template seealso_layers
#' @export
#' 
#' @examples 
#' p <- l_plot()
#' l <- l_layer_texts(p, x=1:3, y=3:1, text=c("This is", "a", "test"), size=20)
#' l_scaleto_world(p)
l_layer_texts <- function(widget, x, y, text, color="gray60", size=6, angle=0,
                         label="texts", parent="root", index=0, ...) {

    l_throwErrorIfNotLoonWidget(widget)
    
    xy <- try(xy.coords(x, y))
    
    l_layer_add(widget, 'texts',
                x=xy$x, y=xy$y, text=text, color=color,
                size=size, angle=angle,
                label=label, parent=parent, index=index, ...)
}



layer_cmd <- function(widget, layer, cmd, ...) {
    
    if (is(widget, "l_layer")) {
        layer <- as.vector(widget)
        widget <- attr(widget, "widget")
    } 
    
    l_throwErrorIfNotLoonWidget(widget)

    invisible(as.numeric(
        tcl(widget, 'layer', cmd, layer, ...)
    ))
}



#' @title Delete a layer
#' 
#' @description All but the \code{'model'} and the \code{'root'} layer can be 
#'   dynamically deleted. If a group layer gets deleted with
#'   \code{l_layer_delete} then all its children layers get moved into their
#'   grandparent group layer.
#' 
#' @param widget widget path or layer object of class \code{'l_layer'}
#' @param layer layer id. If the widget argument is of class \code{'l_layer'}
#'   then the layer argument is not used
#' 
#' @template return_layer_cmd
#' 
#' @template seealso_layers
#' @export
#' 
#' @examples 
#' p <- l_plot()
#' l1 <- l_layer_rectangle(p, x = 0:1, y = 0:1, color='red')
#' l_layer_delete(l1)
#' 
#' l2 <- l_layer_rectangle(p, x = 0:1, y = 0:1, color='yellow')
#' l_layer_delete(p,l2)
l_layer_delete <- function(widget, layer) {
    layer_cmd(widget, layer, 'delete')
}

#' @title Delete a layer and all its descendants
#' 
#' @description Delete a group layer and all it's descendants. Note that the
#'   \code{'model'} layer cannot be deleted.
#' 
#' @inheritParams l_layer_delete
#' @template return_layer_cmd
#' 
#' @seealso \code{\link{l_layer}}, \code{\link{l_layer_delete}}
#' @export
#' 
#' @examples 
#' 
#' p <- l_plot()
#' g <- l_layer_group(p)
#' l1 <- l_layer_rectangle(p, x=0:1, y=0:1, parent=g, color="", linecolor="orange", linewidth=2)
#' l2 <- l_layer_line(p, x=c(0,.5,1), y=c(0,1,0), parent=g, color="blue")
#' 
#' l_layer_expunge(p, g)
#' 
#' # or l_layer_expunge(g)
l_layer_expunge <- function(widget, layer) {
    layer_cmd(widget, layer, 'expunge')
}

#' @title Move a layer
#' 
#' @description The postition of a layer in the layer tree determines the 
#'   rendering order. That is, the non-group layers are rendered in order of a 
#'   Depth-first traversal of the layer tree. The toplevel group layer is called
#'   \code{'root'}.
#' 
#' @inheritParams l_layer_delete
#' @param parent if parent layer is not specified it is set to the current
#'   parent layer of the layer
#' @template param_index
#' 
#' @template return_layer_cmd
#' 
#' @seealso \code{\link{l_layer}}, \code{\link{l_layer_printTree}},
#'   \code{\link{l_layer_index}}
#' @export
#' 
#' @examples 
#' p <- l_plot()
#' 
#' l <- l_layer_rectangle(p, x=0:1, y=0:1, color="steelblue")
#' g <- l_layer_group(p)
#' l_layer_printTree(p)
#' 
#' l_layer_move(l, parent=g)
#' l_layer_printTree(p)
#' 
#' l_layer_move(p, 'model', parent=g)
#' l_layer_printTree(p)
l_layer_move <- function(widget, layer, parent, index="0") {
    
    if(missing(parent))
        parent <- l_layer_getParent(widget, layer)
    
    layer_cmd(widget, layer, 'move', parent, index)
}

#' @title Hide a Layer
#' 
#' @description A hidden layer is not rendered. If a group layer is set to
#'   be hidden then all its descendants are not rendered either.
#' 
#' @inheritParams l_layer_delete
#' @template return_layer_cmd
#'
#' @template descr_layer_visibility
#' 
#' @seealso \code{\link{l_layer}}, \code{\link{l_layer_show}}, 
#'   \code{\link{l_layer_isVisible}}, \code{\link{l_layer_layerVisibility}},
#'   \code{\link{l_layer_groupVisibility}}
#' 
#' @export
#' 
#' @examples 
#' p <- l_plot()
#' 
#' l <- l_layer_rectangle(p, x=0:1, y=0:1, color="steelblue")
#' l_layer_hide(p, l)
l_layer_hide <- function(widget, layer) {
    layer_cmd(widget, layer, 'hide')
}

#' @title Show or unhide a Layer
#' 
#' @description Hidden or invisible layers are not rendered. This function 
#'   unhides invisible layer so that they are rendered again. 
#' 
#' @inheritParams l_layer_delete
#' @template return_layer_cmd
#' 
#' @template descr_layer_visibility
#' 
#' @seealso \code{\link{l_layer}}, \code{\link{l_layer_hide}}, 
#'   \code{\link{l_layer_isVisible}}, \code{\link{l_layer_layerVisibility}},
#'   \code{\link{l_layer_groupVisibility}}
#' @export
#' 
#' @examples 
#' p <- l_plot()
#' 
#' l <- l_layer_rectangle(p, x=0:1, y=0:1, color="steelblue")
#' l_layer_hide(p, l)
#' 
#' l_layer_show(p, l)
l_layer_show <- function(widget, layer) {
    layer_cmd(widget, layer, 'show')
}

#' @title Change layer label
#' 
#' @template descr_layer_labels
#' 
#' @inheritParams l_layer_delete
#' @param label new label of layer
#' 
#' @template details_layer_label
#' 
#' @template return_layer_cmd
#' 
#' @seealso \code{\link{l_layer}}, \code{\link{l_layer_getLabel}}
#' @export
#' 
#' @examples 
#' p <- l_plot()
#' 
#' l <- l_layer_rectangle(p, x=0:1, y=0:1, label="A rectangle")
#' l_layer_getLabel(p, l)
#' 
#' l_layer_relabel(p, l, label="A relabelled rectangle")
#' l_layer_getLabel(p, l)
l_layer_relabel <- function(widget, layer, label) {
    layer_cmd(widget, layer, 'relabel', label)
}

#' @title Switch the layer place with its sibling to the left
#' 
#' @description Change the layers position within its parent layer group by 
#'   decreasing the \code{index} of the layer by one if possible. This means 
#'   that the raised layer will be rendered after (or on top) of its sibling
#'   layer to the left.
#' 
#' @inheritParams l_layer_delete
#' @template return_layer_cmd
#' 
#' @seealso \code{\link{l_layer}}, \code{\link{l_layer_lower}}, \code{\link{l_layer_move}} 
#' @export
#' 
#' @examples 
#' p <- l_plot()
#' 
#' l1 <- l_layer_rectangle(p, x=0:1, y=0:1)
#' l2 <- l_layer_oval(p, x=0:1, y=0:1, color='thistle')
#' 
#' l_aspect(p) <- 1
#' 
#' l_layer_raise(p, l1)
l_layer_raise <- function(widget, layer) {
    layer_cmd(widget, layer, 'raise') 
}

#' @title Switch the layer place with its sibling to the right
#'
#' @description Change the layers position within its parent layer group by 
#'   increasing the \code{index} of the layer by one if possible. This means 
#'   that the raised layer will be rendered before (or on below) of its sibling
#'   layer to the right.
#' 
#' @inheritParams l_layer_delete
#' @template return_layer_cmd
#' 
#' @seealso \code{\link{l_layer}}, \code{\link{l_layer_raise}}, \code{\link{l_layer_move}} 
#' 
#' @export
#' @examples 
#' p <- l_plot()
#' 
#' l1 <- l_layer_rectangle(p, x=0:1, y=0:1)
#' l2 <- l_layer_oval(p, x=0:1, y=0:1, color='thistle')
#' 
#' l_aspect(p) <- 1
#' 
#' l_layer_lower(p, l2)
l_layer_lower <- function(widget, layer) {
    layer_cmd(widget, layer, 'lower') 
}



#' @title Moves the layer up to be a left sibling of its parent
#' 
#' @description Moves the layer down the layer tree (towards the root layer) if
#'   the parent layer is not the root layer.
#' 
#' @inheritParams l_layer_delete
#' @template return_layer_cmd
#'  
#' @export
#' 
#' @examples 
#' p <- l_plot()
#' 
#' g1 <- l_layer_group(p)
#' g2 <- l_layer_group(p, parent=g1)
#' l1 <- l_layer_oval(p, x=0:1, y=0:1, parent=g2)
#' 
#' l_layer_printTree(p)
#' l_layer_promote(p, l1)
#' l_layer_printTree(p)
#' l_layer_promote(p, l1)
#' l_layer_printTree(p)
l_layer_promote <- function(widget, layer) {
    layer_cmd(widget, layer, 'promote')
}

#' @title Moves the layer to be a child of its right group layer sibling
#' 
#' @description Moves the layer up the layer tree (away from the root layer) if
#'   there is a sibling group layer to the right of the layer.
#' 
#' @inheritParams l_layer_delete
#' @template return_layer_cmd
#' 
#' 
#' @export
#' @examples 
#' p <- l_plot()
#' 
#' g1 <- l_layer_group(p)
#' g2 <- l_layer_group(p, parent=g1)
#' l1 <- l_layer_oval(p, x=0:1, y=0:1)
#'
#' l_layer_printTree(p) 
#' l_layer_demote(p, l1)
#' l_layer_printTree(p)
#' l_layer_demote(p, l1)
#' l_layer_printTree(p)
l_layer_demote <- function(widget, layer) {
    layer_cmd(widget, layer, 'demote')
}


layer_get <- function(widget, layer, what, convert=as.character){

    if (is(widget, "l_layer")) {
        layer <- as.vector(widget)
        widget <- attr(widget, "widget")
    }
    
    l_throwErrorIfNotLoonWidget(widget)
    
    convert(tcl(widget, 'layer', what, layer))
}

#' @title Get the bounding box of a layer.
#' 
#' @description The bounding box of a layer returns the coordinates of the
#'   smallest rectangle that encloses all the elements of the layer.
#' 
#' @inheritParams l_layer_delete
#' 
#' @return Numeric vector of length 4 with (xmin, ymin, xmax, ymax) of the
#'   bounding box
#' 
#' @export
#' 
#' @examples 
#' p <- with(iris, l_plot(Sepal.Length ~ Sepal.Width, color=Species))
#' l_layer_bbox(p, layer='model')
#' 
#' l <- l_layer_rectangle(p, x=0:1, y=30:31)
#' l_layer_bbox(p, l)
#' 
#' l_layer_bbox(p, 'root')
l_layer_bbox <- function(widget, layer="root") {
    layer_get(widget, layer, 'bbox', as.numeric)
}


#' @title Get layer label.
#' 
#' @template descr_layer_labels
#' 
#' @inheritParams l_layer_delete
#' 
#' @template details_layer_label
#' 
#' @return Named vector of length 1 with layer label as value and layer id as
#'   name.
#' 
#' @seealso \code{\link{l_layer}}, \code{\link{l_layer_relabel}}
#' @export
#' 
#' @examples 
#' p <- l_plot()
#' l1 <- l_layer_rectangle(p, x=0:1, y=0:1, label="a rectangle")
#' l_layer_getLabel(p, 'model')
#' l_layer_getLabel(p, l1)
l_layer_getLabel <- function(widget, layer) {
    sapply(layer, FUN=function(l){
        paste(layer_get(widget, layer, 'getLabel'), collapse=' ')
    })
}



#' @title Get children of a group layer
#' 
#' @description Returns the ids of a group layer's children.
#' 
#' @inheritParams l_layer_delete
#' 
#' @return Character vector with ids of the childrens. To create layer handles
#'   (i.e. objects of class \code{'l_layer'}) use the
#'   \code{\link{l_create_handle}} function.
#' 
#' @seealso \code{\link{l_layer}}, \code{\link{l_layer_getParent}}
#' @export
#' 
#' @examples
#' p <- l_plot()
#' 
#' g <- l_layer_group(p)
#' l1 <- l_layer_rectangle(p, x=0:1, y=0:1, parent=g)
#' l2 <- l_layer_oval(p, x=0:1, y=0:1, color='thistle', parent=g)
#' 
#' l_layer_getChildren(p, g)
l_layer_getChildren <- function(widget, layer='root') {
    layer_get(widget, layer, 'getChildren')
}


#' @title Get parent layer id of a layer
#' 
#' @description The toplevel parent is the \code{'root'} layer.
#' 
#' @inheritParams l_layer_delete
#' 
#' @seealso \code{\link{l_layer}}, \code{\link{l_layer_getChildren}}
#' @export
#' 
#' @examples 
#' p <- with(iris, l_plot(Sepal.Length ~ Sepal.Width, color=Species))
#' 
#' l_layer_getParent(p, 'model')
l_layer_getParent <- function(widget, layer) {
    layer_get(widget, layer, 'getParent')
}


#' @title Print the layer tree
#' 
#' @description Prints the layer tree (i.e. the layer ids) to the prompt. Group 
#'   layers are prefixed with a \code{'+'}. The \code{'root'} layer is not
#'   listed.
#' 
#' @template param_widget
#' 
#' @return empty string
#' 
#' @seealso \code{\link{l_layer}}, \code{\link{l_layer_getChildren}}, \code{\link{l_layer_getParent}}
#' @export
#' 
#' @examples 
#' p <- l_plot()
#' l_layer_rectangle(p, x=0:1, y=0:1)
#' g <- l_layer_group(p)
#' l_layer_oval(p, x=0:1, y=0:1, parent=g)
#' l_layer_line(p, x=0:1, y=0:1, parent=g)
#' l_layer_printTree(p)
l_layer_printTree <- function(widget) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, 'layer', 'printTree')
    invisible("")
}

#' @title Return visibility flag of layer
#'
#' @description Hidden or invisible layers are not rendered. This function
#'   queries whether a layer is visible/rendered or not.
#' 
#' @inheritParams l_layer_delete
#'
#' @return \code{TRUE} or \code{FALSE} depending whether the layer is visible or
#'   not.
#' 
#' @template descr_layer_visibility
#'   
#' @seealso \code{\link{l_layer}}, \code{\link{l_layer_show}}, 
#'   \code{\link{l_layer_hide}}, \code{\link{l_layer_layerVisibility}},
#'   \code{\link{l_layer_groupVisibility}}
#' @export
#' 
#' @examples 
#' p <- l_plot()
#' l <- l_layer_rectangle(p, x=0:1, y=0:1)
#' l_layer_isVisible(p, l)
#' l_layer_hide(p, l)
#' l_layer_isVisible(p, l)
l_layer_isVisible <- function(widget, layer) {
    layer_get(widget, layer, 'isVisible', function(x)as.logical(as.character(x)))
}

#' @title Get layer type
#' 
#' @description To see the manual page of \code{\link{l_layer}} for all the
#'   primitive layer types.
#' 
#' @inheritParams l_layer_delete
#' 
#' @templateVar page learn_R_layer
#' @template see_l_help_page
#' 
#' @return One of: \code{'group'}, \code{'polygon'}, \code{'text'},
#'   \code{'line'}, \code{'rectangle'}, \code{'oval'}, \code{'points'},
#'   \code{'texts'}, \code{'polygons'}, \code{'rectangles'}, \code{'lines'} and 
#'   \code{'scatterplot'}, \code{'histogram'}, \code{'serialaxes'} and
#'   \code{'graph'}.
#'
#' @seealso \code{\link{l_layer}}
#' @export
#' 
#' @examples 
#' p <- l_plot()
#' l <- l_layer_rectangle(p, x=0:1, y=0:1)
#' l_layer_getType(p, l)
#' l_layer_getType(p, 'model')
l_layer_getType <- function(widget, layer) {
    layer_get(widget, layer, 'getType')
}

#' @title Get the order index of a layer among its siblings
#' 
#' @description The index determines the rendering order of the children layers
#'   of a parent. The layer with index=0 is rendered first.
#' 
#' @inheritParams l_layer_delete
#' 
#' @details Note that the index for layers is 0 based.
#' 
#' @return numeric value
#' @seealso \code{\link{l_layer}}, \code{\link{l_layer_move}}
#' @export
l_layer_index <- function(widget, layer) {
    layer_get(widget, layer, 'index', as.numeric)
}

#' @title Returns logical value for whether layer is actually seen
#' 
#' @description Although the visibility flag for a layer might be set to
#'   \code{TRUE} it won't be rendered as on of its ancestor group layer is set
#'   to be invisible. The \code{l_layer_visibility} returns \code{TRUE} if the
#'   layer and all its ancestor layers have their visibility flag set to true
#'   and the layer is actually rendered.
#' 
#' @inheritParams l_layer_delete
#' 
#' @template descr_layer_visibility
#' 
#' @return \code{TRUE} if the layer and all its ancestor layers have their 
#'   visibility flag set to true and the layer is actually rendered, otherwise
#'   \code{FALSE}.
#' 
#' @seealso \code{\link{l_layer}}, \code{\link{l_layer_show}}, 
#'   \code{\link{l_layer_hide}}, \code{\link{l_layer_isVisible}},
#'   \code{\link{l_layer_groupVisibility}}
#' 
#' @export
l_layer_layerVisibility <- function(widget, layer) {
    layer_get(widget, layer, 'layerVisibility', as.numeric)
}

#' @title Queries visibility status of decendants
#' 
#' @description Query whether all, part or none of the group layers descendants
#'   are visible.
#' 
#' @inheritParams l_layer_delete
#' 
#' @return \code{'all'}, \code{'part'} or \code{'none'} depending on the
#'   visibility status of the descendants.
#' 
#' @template descr_layer_visibility
#' 
#' @seealso \code{\link{l_layer}}, \code{\link{l_layer_show}}, 
#'   \code{\link{l_layer_hide}}, \code{\link{l_layer_isVisible}},
#'   \code{\link{l_layer_layerVisibility}}
#' @export
#' 
#' @examples
#' 
#' p <- l_plot()
#' 
#' g <- l_layer_group(p)
#' l1 <- l_layer_rectangle(p, x=0:1, y=0:1, parent=g)
#' l2 <- l_layer_oval(p, x=0:1, y=0:1, parent=g)
#' 
#' l_layer_groupVisibility(p, g)
#' l_layer_hide(p, l2)
#' l_layer_groupVisibility(p, g)
#' l_layer_hide(p, l1)
#' l_layer_groupVisibility(p, g)
#' l_layer_hide(p, g)
#' l_layer_groupVisibility(p, g)
l_layer_groupVisibility <- function(widget, layer) {
    layer_get(widget, layer, 'groupVisibility')
}
