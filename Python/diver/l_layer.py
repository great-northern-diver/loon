## \file
from .tk import tk
from .l_throwErrorIfNotLoonWidget import *
from .nestedLists import *
from .loon_class import *
from .l_throwErrorIfNotLoonWidget import l_throwErrorIfNotLoonWidget
from multipledispatch import dispatch
from sklearn.neighbors import KernelDensity
from scipy.stats import gaussian_kde
import numpy as np

@dispatch(object,object)
def l_layer(widget,x,**options):
    '''Loon layers

    Loon supports layering of visuals and groups of visuals. The 
    `l_layer` function is a generic method.

    Details: 
        loon's displays that use the main graphics model (i.e. histogram, 
        scatterplot and graph displays) support layering of visual information. The
        following table lists the layer types and functions for layering on a 
        display.

        =======             ========================================          ====================
        *Type*              *Description*                                     *Creator Function*
        =======             ========================================          ====================
        group               a group can be a parent of other layers           `l_layer_group`
        polygon             one polygon                                       `l_layer_polygon`
        text                one text string                                   `l_layer_text`
        line                one line (i.e. connected line segments)           `l_layer_line`
        rectangle           one rectangle                                     `l_layer_rectangle`
        oval                one oval                                          `l_layer_oval`
        points              n points (filled) circle                          `l_layer_points`
        texts               n text strings                                    `l_layer_text`
        polygons            n polygons                                        `l_layer_polygons`
        rectangles          n rectangles                                      `l_layer_rectangles`
        lines               n sets of connected line segments                 `l_layer_lines`
       =======             ========================================          ====================
        
        Every layer within a display has a unique id. The visuals of the data in a 
        display present the default layer of that display and has the layer id 
        `'model'`. For example, the `'model'` layer of a scatterplot 
        display visualizes the scatterplot glyphs. Functions useful to query layers
        are

        ============                 ==================
        *Function*                   *Description* 
        ============                 ==================
        l_layer_ids                  List layer ids   
        l_layer_getType              Get layer type    
        ============                 ==================                        
        
        Layers are arranged in a tree structure with the tree root having the layer
        id `'root'`. The rendering order of the layers is according to a 
        depth-first traversal of the layer tree. This tree also maintains a label 
        and a visibility flag for each layer. The layer tree, layer ids, layer 
        labels and the visibility of each layer are visualized in the layers 
        inspector. If a layer is set to be invisible then it is not rendered on the
        display. If a group layer is set to be invisible then all its children are 
        not rendered; however, the visibility flag of the children layers remain 
        unchanged. Relevant functions are:

        =========================       =============================================================================================
        *Function*                      *Description* 
        =========================       =============================================================================================
        l_layer_getParent               Get parent layer id of a layer   
        l_layer_getChildren             Get children of a group layer
        l_layer_index                   Get the order index of a layer among its siblings
        l_layer_printTree               Print out the layer tree
        l_layer_move                    Move a layer
        l_layer_lower                   Switch the layer place with its sibling to the right
        l_layer_raise                   Switch the layer place with its sibling to the left
        l_layer_demote                  Moves the layer up to be a left sibling of its parent
        l_layer_promote                 Moves the layer to be a child of its right group layer sibling
        l_layer_hide                    Set the layers visibility flag to `False`
        l_layer_show                    Set the layers visibility flag to `True`
        l_layer_isVisible               Return visibility flag of layer
        l_layer_layerVisibility	        Returns logical value for whether layer is actually seen
        l_layer_groupVisibility	        Returns `all`, `part` or `none` for expressing which part of the layers children are visible.
        l_layer_delete	                Delete a layer. If the layer is a group move all its children layers to the layers parent.
        l_layer_expunge	                Delete layer and all its children layer.
        l_layer_getLabel	            Get layer label.
        l_layer_relabel	                Change layer label.
        l_layer_bbox	                Get the bounding box of a layer.        
        =========================       =============================================================================================       

        All layers have states that can be queried and modified using the same 
        functions as the ones used for displays (i.e. `l_cget`, 
        `l_configure`). The 
        last group of layer types in the above table have n-dimensional states, 
        where the actual value of n can be different for every layer in a display.
           
        The difference between the model layer and the other layers is that the 
        model layer has a *selected* state, responds to selection gestures and
        supports linking.
    Args:
        x: object that should be layered
        **options: additional arguments, often state definition for the basic layering function
    
    See Also: 
        `l_info_states`, `l_scaleto_layer`, `l_scaleto_world`

    Examples:
        >>> class foo:
        >>>    def __init__(self,x,y,color):
        >>>        self.x = x
        >>>        self.y = y
        >>>        self.color = color 
        >>> obj = foo(x = list(range(1,7)) + list(range(6,1,-1)), y = [3,1,0,0,1,3,3,5,6,6,5], color = 'yellow')
        >>> from multipledispatch import dispatch            
        >>> @dispatch(object,foo)
        >>> def l_layer(widget, x, **options):
        >>>    l = l_layer_polygon(widget,x = x.x,y = x.y,color = x.color)
        >>>    return(l)
        >>> p = l_plot()
        >>> l = l_layer(p,obj)
        >>> l_scaleto_world(p)
    '''
    print('No support type of ' + str(type(x)))



@dispatch(object,(gaussian_kde,KernelDensity))
def l_layer(widget, x, **options):
    '''Layer Method for Kernel Density Estimation

    Layer a line that represents a kernel density estimate.

    Args:
        widget: target widget
        x: an density object of scipy.stats.gaussian_kde or sklearn.neighbors.KernelDensity
    
    Examples:
        >>> from diver import l_layer,iris,l_hist
        >>> from scipy.stats import gaussian_kde
        >>> import numpy as np
        >>> d = gaussian_kde(iris.iloc[:,1])
        >>> h = l_hist(iris.iloc[:,1],yshows='density')
        >>> l = l_layer(h, d, color="steelblue", linewidth=3)
    '''
    if(not isinstance(widget,loon)):
        widget = loon(widget)
    if(isinstance(x,gaussian_kde)):
        x_grid = np.linspace(min(widget['x']),max(widget['x']),512)
        y = x.evaluate(x_grid)
        l = l_layer_line(widget, list(x_grid), list(y), **options)
    else:
        x_grid = np.linspace(min(widget['x']),max(widget['x']),512)
        log_y = x.score_samples(x_grid[:,np.newaxis])  
        l = l_layer_line(widget, list(x_grid), list(np.exp(log_y)), **options)
    return l 

def l_layer_add(widget, Type, **kwargs):
    # helper function to add a layer
    opt = []
    if(not isinstance(widget,str)):
        widget = widget.plot
    for key, value in kwargs.items():
        opt.append('-' + key)
        opt.append(value)       
    res = tk.tk.call(widget,'layer',Type,*opt)[0]
    res = loon_l_layer(widget,Type,res)
    return(res)

def l_layer_ids(widget):
    '''List ids of layers in Plot

    Every layer within a display has a unique id. This function
    returns a list of all the layer ids for a widget.

    Returns:
        vector with layer ids in rendering order. To create a layer handle
        object use `l_create_handle`.
    '''
    if(not isinstance(widget,str)):
        widget = widget.plot
    l_throwErrorIfNotLoonWidget(widget)
    return tk.tk.call(widget, 'layer', 'ids')

def l_layer_group(widget, label="group", parent="root", index=0):
    '''layer a group node

    A group layer can contain other layers. If the group layer is
    invisible, then so are all its children.
    
    Args:
        label: label used in the layers inspector
        parent: group layer
        index: of the newly added layer in its parent group
    '''
    l_throwErrorIfNotLoonWidget(widget)
    if(isinstance(parent, loon_l_layer)):
        parent = parent.id

    res = l_layer_add(widget, 'group', label=label, parent=parent, index=index)
    return(res)

def l_layer_polygon(widget, x, y,
                            color="gray80", linecolor="black", linewidth=1,
                            label="polygon", parent="root", index=0,**options):
    '''
    layger a polygon
    '''
    if(isinstance(widget,loon_class.loon)):
        widget = widget.plot
    if(isinstance(parent, loon_l_layer)):
        parent = parent.id

    l_throwErrorIfNotLoonWidget(widget)
    res =l_layer_add(widget, 'polygon',
                x=x, y=y, color=color,
                linecolor=linecolor,
                linewidth=linewidth,
                label=label, parent=parent, index=index, **options)
    return res

def l_layer_polygons(widget, x, y,
                             color="gray80", linecolor="black", linewidth=1,
                             label="polygons", parent="root", index=0,**options):
    '''layer multiple polygons at the same time 

    Args:
        x: list with lists with x coordinates
        y: list with lists with y coordinates
        color: list with fill colors, if empty string '', then the
                fill is transparant
        linecolor: list with outline colors
        linewidth: list with line widths
        **options additional state initialization arguments, see `l_info_states` 

    Examples:
        >>> p = l_plot()
        >>> l = l_layer_polygons(p,[[1,2,1.5], [3,4,6.0,5,2], [1.0,3,5,3]],
                    y = [[1,1,2.0], [1,1.5,1,4,2], [3.0,5,6,4]],
                    color = ['red', 'green', 'blue'],
                    linecolor = "")
        >>> l_scaleto_world(p)
        >>> l_info_states(l, "color")
    '''
    if(isinstance(widget,loon_class.loon)):
        widget = widget.plot
    if(isinstance(parent, loon_l_layer)):
        parent = parent.id
    l_throwErrorIfNotLoonWidget(widget)
    res = l_layer_add(widget, 'polygons',
                x=l_list2nestedTclList(x),
                y=l_list2nestedTclList(y),
                color=color,
                linecolor=linecolor,
                linewidth=linewidth,
                label=label, parent=parent, index=index, **options)
    return res

def l_layer_rectangle(widget, x, y,
                              color="gray80", linecolor="black", linewidth=1,
                              label="rectangle", parent="root", index=0, **options):
    '''layer a rectangle

    Examples:
        >>> p = l_plot()
        >>> l = l_layer_rectangle(p, x=[2,3], y=[1,10], color='steelblue')  
        >>> l_scaleto_layer(l)
    '''
    if(isinstance(widget,loon_class.loon)):
        widget = widget.plot    
    if(isinstance(parent, loon_l_layer)):
        parent = parent.id
    l_throwErrorIfNotLoonWidget(widget)
    
    res = l_layer_add(widget, 'rectangle',
                x=x, y=y, color=color,
                linecolor=linecolor,
                linewidth=linewidth,
                label=label, parent=parent, index=index, **options)
    return res

def l_layer_rectangles(widget, x, y,
                             color="gray80", linecolor="black", linewidth=1,
                             label="rectangles", parent="root", index=0,**options):
    '''layer multiple rectangles at the same time 

    Examples:
        >>> p = l_plot()
        >>> l = l_layer_rectangles(
                p,
                x = [[0,1], [1,2], [2,3], [5,6]],
                y = [[0,1], [1,2], [0,1], [3,4]],
                color = ['red', 'blue', 'green', 'orange'],
                linecolor = "black")
        >>> l_scaleto_world(p)
        >>> l_info_states(l)
    '''
    if(isinstance(parent, loon_l_layer)):
        parent = parent.id
    l_throwErrorIfNotLoonWidget(widget)
    res = l_layer_add(widget, 'rectangles',
                x=l_list2nestedTclList(x),
                y=l_list2nestedTclList(y),
                color=color,
                linecolor=linecolor,
                linewidth=linewidth,
                label=label, parent=parent, index=index, **options)
    return res

def l_layer_line(widget, x, y=None, color="black",
                         linewidth=1, dash="",
                         label="line", parent="root", index=0, **options):
    '''layer a line 

    Args:
        x: the coordinates of line.
        y: the y coordinates of the line
        color: color of line
        dash: dash pattern of line, see https://www.tcl.tk/man/tcl8.6/TkCmd/canvas.htm#M26
    
    Examples:
        >>> p = l_plot()
        >>> l = l_layer_line(p, x=[1,2,3,4], y=[1,3,2,4], color='red', linewidth=2)
        >>> l_scaleto_world(p)
    '''
    if(isinstance(parent, loon_l_layer)):
        parent = parent.id
    if(isinstance(widget,loon_class.loon)):
        widget = widget.plot    
    l_throwErrorIfNotLoonWidget(widget)
    
    res = l_layer_add(widget, 'line',
                x=x, y=y, color=color,
                linewidth=linewidth, dash=dash,
                label=label, parent=parent, index=index, **options)
    return res

def l_layer_lines(widget, x, y,
                          color="black", linewidth=1,
                          label="lines", parent="root", index=0,**options):
    '''layer multiple lines at the same time 

    Examples:
        >>> s = [rows for _, rows in UsAndThem.groupby('Country') if rows.shape[0] > 1]
        >>> s1 = [x.sort_values('Year') for x in s]
        >>> xcoords = [x.Year for x in s1]
        >>> ycoords = [x.LifeExpectancy for x in s1]
        >>> region = [x['Geographic.Region'].iloc[0] for x in s1]
        >>> p = l_plot(showItemLabels=True)
        >>> l = l_layer_lines(p, xcoords, ycoords, color=region)
        >>> l_scaleto_layer(l)
    '''
    if(isinstance(parent, loon_l_layer)):
        parent = parent.id
    if(isinstance(widget,loon_class.loon)):
        widget = widget.plot     
    l_throwErrorIfNotLoonWidget(widget)
    res = l_layer_add(widget, 'lines',
                x=l_list2nestedTclList(x),
                y=l_list2nestedTclList(y),
                color=color,
                linewidth=linewidth,
                label=label, parent=parent, index=index, **options)
    return res

def l_layer_oval(widget, x, y,
                         color="gray80", linecolor="black", linewidth=1,
                         label="oval", parent="root", index=0, **options):
    '''layer oval 

    Examples:
        >>> p = l_plot()
        >>> l = l_layer_oval(p, [1,5], [2,12], color='steelblue')
        >>> l_configure(p, panX=0, panY=0, deltaX=20, deltaY=20)
    '''
    if(isinstance(widget,loon_class.loon)):
        widget = widget.plot    
    if(isinstance(parent, loon_l_layer)):
        parent = parent.id
    l_throwErrorIfNotLoonWidget(widget)

    res = l_layer_add(widget, 'oval',
                x=x, y=y, color=color,
                linecolor=linecolor,
                linewidth=linewidth,
                label=label, parent=parent, index=index, **options)
    return res 


def l_layer_points(widget, x, y=None, color="gray60", size=6,
                    label="points", parent="root", index=0,**options):
    '''layer points 

    Args:
        color: color of points
        size: size point, as for scatterplot model layer
    '''
    l_throwErrorIfNotLoonWidget(widget)
    if(y == None):
        y = list(range(len(x)))
    if(isinstance(parent, loon_l_layer)):
        parent = parent.id
    l_layer_add(widget, 'points',
                x=x, y=y, color=color,
                size=size,
                label=label, parent=parent, index=index, **options)

def l_layer_text(widget, x, y, text, color="gray60", size=6, angle=0,
                         label="text", parent="root", index=0, **options):
    '''layer text 

    layer a single character string
    
    Args:
        x: x coordinate
        y: y coordinate
        text: character string
        color: color of text
        size: size of the font
        angle: rotation of text
    
    Details: 
        As a side effect of `Tcl`'s text-based design, it is best to
        use `l_layer_text` if one would like to layer a single character
        string (and not `l_layer_texts` with `n=1`).
    
    Examples:
        >>> p = l_plot()
        >>> l = l_layer_text(p, 0, 0, "Hello World")
    '''
    if(isinstance(widget,loon_class.loon)):
        widget = widget.plot    
    if(isinstance(parent, loon_l_layer)):
        parent = parent.id

    l_throwErrorIfNotLoonWidget(widget)
    res = l_layer_add(widget, 'text',
                x=x, y=y, text=text, color=color,
                size=size, angle=angle,
                label=label, parent=parent, index=index, **options)
    return res

def l_layer_texts(widget, x, y, text, color="gray60", size=6, angle=0,
                         label="texts", parent="root", index=0, **options):
    '''layer multiple texts at the same time 
    
    Layer a vector of character strings.
    
    Args:
        x: list of x coordinates
        y: list of y coordinates
        text: list with text strings
        color: color of text
        size: font size
        angle: text rotation

    Details: 
        As a side effect of `Tcl`'s text-based design, it is best to
        use `l_layer_text` if one would like to layer a single character
        string (and not `l_layer_texts` with `n=1`).
    
    Examples:
        >>> p = l_plot()
        >>> l = l_layer_texts(p, x=[1,2,3], y=[3,2,1], text=["This is", "a", "test"], size=20)
        >>> l_scaleto_world(p)
    '''
    if(isinstance(widget,loon_class.loon)):
        widget = widget.plot   
    if(isinstance(parent, loon_l_layer)):
        parent = parent.id
    l_throwErrorIfNotLoonWidget(widget)
    
    res = l_layer_add(widget, 'texts',
                x=x, y=y, text=text, color=color,
                size=size, angle=angle,
                label=label, parent=parent, index=index, **options)

def layer_cmd(widget, layer, cmd = None,*args,**options):    
    if(isinstance(widget,loon)):
        widget = widget.plot
    if(isinstance(layer,loon_l_layer)):
        layer = layer.id
    l_throwErrorIfNotLoonWidget(widget)
    opt = []    
    for i in args:
        opt.append(i)

    for key, value in options.items():
        opt.append('-' + key)
        opt.append(value)
    
    tk.tk.call(widget, 'layer', cmd, layer, *opt)


def l_layer_delete(widget, layer):
    '''Delete a layer

    All but the `'model'` and the `'root'` layer can be 
    dynamically deleted. If a group layer gets deleted with
    `l_layer_delete` then all its children layers get moved into their
    grandparent group layer.
    
    Args:
        widget: widget path or loon object of class `loon`
        layer: layer id or the layer object of calls `loon_l_layer`

    Examples:
        >>> p = l_plot()
        >>> l1 = l_layer_rectangle(p, x = [0,1], y = [0,1], color='red')
        >>> l_layer_delete(p,l1)
        >>> l2 = l_layer_rectangle(p, x = [0,1], y = [0,1], color='yellow')
        >>> l_layer_delete(p,l2)
    '''
    layer_cmd(widget, layer, 'delete')

def l_layer_expunge(widget, layer):
    '''Delete a layer and all its descendants

    Delete a group layer and all it's descendants. Note that the
    `model` layer cannot be deleted.
    
    See Also:
        `l_layer`, `l_layer_delete`

    Examples:
        >>> p = l_plot()
        >>> g = l_layer_group(p)
        >>> l1 = l_layer_rectangle(p, x=[0,1], y=[0,1], parent=g.id, color="", linecolor="orange", linewidth=2)
        >>> l2 = l_layer_line(p, x=[0,.5,1], y=[0,1,0], parent=g.id, color="blue")
        >>> l_layer_expunge(p, g)
    '''
    layer_cmd(widget, layer, 'expunge')

def l_layer_move(widget, layer, parent = None, index="0"):
    '''Move a layer

    The postition of a layer in the layer tree determines the 
    rendering order. That is, the non-group layers are rendered in order of a 
    Depth-first traversal of the layer tree. The toplevel group layer is called
    `root`.

    Args:
        parent: if parent layer is not specified it is set to the current
                parent layer of the layer
    
    See Also:
        `l_layer`, `l_layer_printTree`, `l_layer_index`

    Examples:
        >>> p = l_plot() 
        >>> l = l_layer_rectangle(p, x=[0,1], y=[0,1], color="steelblue")
        >>> g = l_layer_group(p)
        >>> l_layer_printTree(p)
        >>> l_layer_move(p,l, parent=g)
        >>> l_layer_printTree(p)
        >>> l_layer_move(p, 'model', parent=g)
        >>> l_layer_printTree(p)
    '''
    if(parent == None):
        parent = l_layer_getParent(widget, layer)
    if(isinstance(parent, loon_l_layer)):
        parent = parent.id
    layer_cmd(widget, layer, 'move', parent, index)

def l_layer_hide(widget, layer):
    '''Hide a Layer

    A hidden layer is not rendered. If a group layer is set to
    be hidden then all its descendants are not rendered either.
    
    See Also:
        `l_layer`, `l_layer_show`, `l_layer_isVisible`, 
        `l_layer_layerVisibility`,`l_layer_groupVisibility`
    
    Examples:
        >>> p = l_plot()
        >>> l = l_layer_rectangle(p, x=[0,1], y=[0,1], color="steelblue")
        >>> l_layer_hide(p, l)
    '''
    layer_cmd(widget, layer, 'hide')


def l_layer_show(widget, layer):
    '''Show or unhide a Layer

    Hidden or invisible layers are not rendered. This function 
    unhides invisible layer so that they are rendered again. 
    
    See Also: 
        `l_layer`, `l_layer_hide`, `l_layer_isVisible`,
        `l_layer_layerVisibility`, `l_layer_groupVisibility`

    Examples:
        >>> p = l_plot()
        >>> l = l_layer_rectangle(p, x=[0,1], y=[0,1], color="steelblue")
        >>> l_layer_hide(p, l) 
        >>> l_layer_show(p, l)
    '''
    layer_cmd(widget, layer, 'show')

def l_layer_relabel(widget, layer, label):
    '''Change layer label
    
    See Also:
        `l_layer`, `l_layer_getLabel`

    Examples:
        >>> p = l_plot()
        >>> l = l_layer_rectangle(p, x=[0,1], y=[0,1], label="A rectangle")
        >>> l_layer_getLabel(p, l)
        >>> l_layer_relabel(p, l, label="A relabelled rectangle")
        >>> l_layer_getLabel(p, l)
    '''
    layer_cmd(widget, layer, 'relabel', label)

def l_layer_raise(widget, layer):
    '''Switch the layer place with its sibling to the left

    Change the layers position within its parent layer group by 
    decreasing the `index` of the layer by one if possible. This means 
    that the raised layer will be rendered after (or on top) of its sibling
    layer to the left.
    
    See Also:
        `l_layer`, `l_layer_lower`, `l_layer_move` 

    Examples:
        >>> p = l_plot()
        >>> l1 = l_layer_rectangle(p, x=[0,1], y=[0,1])
        >>> l2 = l_layer_oval(p, x=[0,1], y=[0,1], color='thistle')
        >>> l_setAspect(p,1)
        >>> l_layer_raise(p, l1)
    '''
    layer_cmd(widget, layer, 'raise') 


def l_layer_lower(widget, layer):
    '''Switch the layer place with its sibling to the right

    Change the layers position within its parent layer group by 
    increasing the `index` of the layer by one if possible. This means 
    that the raised layer will be rendered before (or on below) of its sibling
    layer to the right.
    
    See Also:
        `l_layer`, `l_layer_raise`, `l_layer_move`

    Examples:
        >>> p = l_plot()
        >>> l1 = l_layer_rectangle(p, x=[0,1], y=[0,1])
        >>> l2 = l_layer_oval(p, x=[0,1], y=[0,1], color='thistle') 
        >>> l_setAspect(p,1)
        >>> l_layer_lower(p, l2)
    '''
    layer_cmd(widget, layer, 'lower') 


def l_layer_promote(widget, layer):
    '''Moves the layer up to be a left sibling of its parent

    Moves the layer down the layer tree (towards the root layer) if
    the parent layer is not the root layer.

    Examples: 
        >>> p = l_plot()
        >>> g1 = l_layer_group(p)
        >>> g2 = l_layer_group(p, parent=g1)
        >>> l1 = l_layer_oval(p, x=[0,1], y=[0,1], parent=g2)
        >>> l_layer_printTree(p)
        >>> l_layer_promote(p, l1)
        >>> l_layer_printTree(p)
        >>> l_layer_promote(p, l1)
        >>> l_layer_printTree(p)
    '''
    layer_cmd(widget, layer, 'promote')

def l_layer_demote(widget, layer):
    '''Moves the layer to be a child of its right group layer sibling

    Moves the layer up the layer tree (away from the root layer) if
    there is a sibling group layer to the right of the layer.
    
    Examples: 
        >>> p = l_plot()
        >>> g1 = l_layer_group(p)
        >>> g2 = l_layer_group(p, parent=g1)
        >>> l1 = l_layer_oval(p, x=[0,1], y=[0,1])
        >>> l_layer_printTree(p) 
        >>> l_layer_demote(p, l1)
        >>> l_layer_printTree(p)
        >>> l_layer_demote(p, l1)
        >>> l_layer_printTree(p)
    '''
    layer_cmd(widget, layer, 'demote')


def layer_get(widget, layer, what, convert=str):
    #if (is(widget, "l_layer")) {
    #    layer <- as.vector(widget)
    #    widget <- attr(widget, "widget")
    #}
    if(isinstance(widget,loon)):
        widget = widget.plot 
    if(isinstance(widget, loon_l_layer)):
        exit('widget should be a loon class object')   
    if(isinstance(layer,loon_l_layer)):
        layer = layer.id
    l_throwErrorIfNotLoonWidget(widget)    
    res = tk.tk.call(widget, 'layer', what, layer)
    if(isinstance(res,tuple)):
        res = np.array(res).flatten()
        res = [convert(x) for x in res]
    else:
        res = convert(res)
    return res

def l_layer_bbox(widget, layer="root"):
    '''Get the bounding box of a layer.

    The bounding box of a layer returns the coordinates of the
    smallest rectangle that encloses all the elements of the layer.
    
    Returns:
        Numeric list of length 4 with (xmin, ymin, xmax, ymax) of the
        bounding box
    
    Examples:
        >>> p = l_plot(iris['Sepal.Length'], iris['Sepal.Width'], color=iris['Species'])
        >>> l_layer_bbox(p, layer='model')
        >>> l = l_layer_rectangle(p, x=[0,1], y=[30,31])
        >>> l_layer_bbox(p, l)
        >>> l_layer_bbox(p, 'root')
    '''
    res = layer_get(widget, layer, 'bbox', int)
    return res


def l_layer_getLabel(widget, layer):
    '''Get layer label.

    Returns:
        Named vector of length 1 with layer label as value and layer id as name.

    See Also:
        `l_layer`, `l_layer_relabel`
    
    Examples:
        >>> p = l_plot()
        >>> l1 = l_layer_rectangle(p, x=[0,1], y=[0,1], label="a rectangle")
        >>> l_layer_getLabel(p, 'model')
        >>> l_layer_getLabel(p, l1)
    '''
    res = layer_get(widget,layer,'getLabel')
    return res


def l_layer_getChildren(widget, layer='root'):
    '''Get children of a group layer    
    
    Returns the ids of a group layer's children.
    
    Returns:
        Character vector with ids of the childrens. To create layer handles
        (i.e. objects of class `l_layer` use the
        `l_create_handle` function.
    
    See Also: 
        `l_layer`, `l_layer_getParent`

    Examples:
        >>> p = l_plot()
        >>> g = l_layer_group(p)
        >>> l1 = l_layer_rectangle(p, x=[0,1], y=[0,1], parent=g)
        >>> l2 = l_layer_oval(p, x=[0,1], y=[0,1], color='thistle', parent=g) 
        >>> l_layer_getChildren(p, g)
    '''
    res = layer_get(widget, layer, 'getChildren')
    return res

def l_layer_getParent(widget, layer):
    '''Get parent layer id of a layer

    The toplevel parent is the `root` layer.

    See Also:
        `l_layer`, `l_layer_getChildren`

    Examples: 
        >>> p = l_plot(iris['Sepal.Length'],iris['Sepal.Width'], color=iris['Species']) 
        >>> l_layer_getParent(p, 'model')
    '''
    res = layer_get(widget, layer, 'getParent')
    return res

def l_layer_printTree(widget):
    '''Print the layer tree

    Prints the layer tree (i.e. the layer ids) to the prompt. Group 
    layers are prefixed with a `'+'`. The `'root'` layer is not
    listed.
    
    Returns: 
        empty string

    See Also:
        `l_layer`, `l_layer_getChildren`, `l_layer_getParent`

    Examples:
        >>> p = l_plot()
        >>> l_layer_rectangle(p, x=[0,1], y=[0,1])
        >>> g = l_layer_group(p)
        >>> l_layer_oval(p, x=[0,1], y=[0,1], parent=g)
        >>> l_layer_line(p, x=[0,1], y=[0,1], parent=g)
        >>> l_layer_printTree(p)
    '''
    if(isinstance(widget,loon)):
        widget = widget.plot
    l_throwErrorIfNotLoonWidget(widget)
    tk.tk.call(widget, 'layer', 'printTree')
    return ''

def l_layer_isVisible(widget, layer):
    '''Return visibility flag of layer

    Hidden or invisible layers are not rendered. This function
    queries whether a layer is visible/rendered or not.
    
    Returns: 
        `True` or `False` depending whether the layer is visible or not.

    See Also:
        `l_layer`, `l_layer_show`, `l_layer_hide`, `l_layer_layerVisibility`,`l_layer_groupVisibility`
    
    Examples:
        >>> p = l_plot()
        >>> l = l_layer_rectangle(p, x=[0,1], y=[0,1])
        >>> l_layer_isVisible(p, l)
        >>> l_layer_hide(p, l)
        >>> l_layer_isVisible(p, l)
    '''
    res = layer_get(widget, layer, 'isVisible',bool)
    return res

def l_layer_getType(widget, layer):
    '''Get layer type
    
    To see the manual page of `l_layer` for all the
    primitive layer types.
    
    Returns:
        One of: `group`, `polygon`, `text`,
        `line`, `rectangle`, `oval`, `points`,
        `texts`, `polygons`, `rectangles`, `lines` and 
        `scatterplot`, `histogram`, `serialaxes` and `graph`.

    See Also: 
        `l_layer` 

    Examples: 
        >>> p = l_plot()
        >>> l = l_layer_rectangle(p, x=[0,1], y=[0,1])
        >>> l_layer_getType(p, l)
        >>> l_layer_getType(p, 'model')
    '''
    res = layer_get(widget, layer, 'getType')
    return res

def l_layer_index(widget, layer):
    '''Get the order index of a layer among its siblings

    The index determines the rendering order of the children layers
    of a parent. The layer with index=0 is rendered first.
    
    Details: 
        Note that the index for layers is 0 based.

    Returns: 
        numeric value 
    
    See Also: 
        `l_layer`, `l_layer_move`
    '''
    res = layer_get(widget, layer, 'index', int)
    return res

def l_layer_layerVisibility(widget, layer):
    '''Returns logical value for whether layer is actually seen

    Although the visibility flag for a layer might be set to
    `True` it won't be rendered as on of its ancestor group layer is set
    to be invisible. The `l_layer_visibility` returns `True` if the
    layer and all its ancestor layers have their visibility flag set to true
    and the layer is actually rendered.
    
    Returns: 
        `True` if the layer and all its ancestor layers have their 
        visibility flag set to true and the layer is actually rendered, otherwise
        `False`.
    
    See Also:
        `l_layer`, `l_layer_show`, `l_layer_hide`, `l_layer_isVisible`, `l_layer_groupVisibility`
    '''
    res = layer_get(widget, layer, 'layerVisibility', int)
    return res

def l_layer_groupVisibility(widget, layer):
    '''Queries visibility status of decendants

    Query whether all, part or none of the group layers descendants
    are visible.
    
    Returns:
        `'all'`, `'part'` or `'none'` depending on the
        visibility status of the descendants.
    
    See Also: 
        `l_layer`, `l_layer_show`, `l_layer_hide`, `l_layer_isVisible`, `l_layer_layerVisibility`
 
    Examples: 
        >>> p = l_plot()
        >>> g = l_layer_group(p)
        >>> l1 = l_layer_rectangle(p, x=[0,1], y=[0,1], parent=g)
        >>> l2 = l_layer_oval(p, x=[0,1], y=[0,1], parent=g)
        >>> l_layer_groupVisibility(p, g)
        >>> l_layer_hide(p, l2)
        >>> l_layer_groupVisibility(p, g)
        >>> l_layer_hide(p, l1)
        >>> l_layer_groupVisibility(p, g)
        >>> l_layer_hide(p, g)
        >>> l_layer_groupVisibility(p, g)
    '''
    res = layer_get(widget, layer, 'groupVisibility')
    return res