## \file
from .l_throwErrorIfNotLoonWidget import l_throwErrorIfNotLoonWidget
from .tk import tk
from .loon_class import loon, loon_l_glyph
from .helper import opts_to_list
from .nestedLists import l_list2nestedTclList
from .retrieve_name import retrieve_name
from .l_data import l_data
import pandas as pd 
import numpy as np
# Convenience Function to internally work with glyphs
def l_glyph(widget,*args ,**options):
    if(isinstance(widget, loon)):
        widget = widget.plot   
    l_throwErrorIfNotLoonWidget(widget)
      
    opt = opts_to_list(options)
    opt = list(args) + opt 
    res = tk.tk.call(widget, "glyph",*opt)
    return res 

def l_glyph_relabel(widget, id, label):
    '''Relabel Glyph

    Change the label of a glyph. Note that the label is only
    displayed in the glyph inspector.
    
    Examples:
        >>> p = l_plot(iris, color = iris.Species)
        >>> g = l_glyph_add_text(p, iris.Species, "test_label")
        >>> p['glyph'] = g
        >>> l_glyph_relabel(p, g, "Species")
    '''
    if(isinstance(id,loon_l_glyph)):
        id = id.id
    l_glyph(widget, "relabel", id, label)



def l_glyph_delete(widget, id):
    '''Delete a Glyph

    Delete a glyph from the plot.

    See Also: 
        `l_glyph_add`
    '''
    if(isinstance(id,loon_l_glyph)):
        id = id.id 
    l_glyph(widget, "delete", id)

def l_glyph_ids(widget):
    '''List glyphs ids

    List all the non-primitive glyph ids attached to display.
    
    See Also:
        `l_glyph_add`
    '''
    l_glyph(widget, "ids")


def l_glyph_getLabel(widget, id):
    '''Get Glyph Label
    
    Returns the label of a glyph
    
    See Also:
        `l_glyph_add`, `l_glyph_ids`, `l_glyph_relabel`
    '''
    if(isinstance(id,loon_l_glyph)):
        id = id.id
    res = l_glyph(widget, "getLabel", id)
    return res

def l_glyph_getType(widget, id):
    '''Get Glyph Type

    Query the type of a glyph    
    
    See Also: 
        `l_glyph_add`
    '''
    if(isinstance(id,loon_l_glyph)):
        id = id.id
    
    if (id in l_primitiveGlyphs()):
        return "primitive_glyph"
    else:
        return l_glyph(widget, "getType", id)
    

def l_primitiveGlyphs():
    '''The primitive glyphs available to a scatterplot or graph display

    Returns a vector of the available primitive glyphs.
    
    Details: 
        The scatterplot and graph displays both have the n-dimensional state
        `'glyph'` that assigns each data point or graph node a glyph (i.e. a
        visual representation).

        Loon distinguishes between primitive and non-primitive glyphs: the primitive
        glyphs are always available for use whereas the non-primitive glyphs need to
        be first specified and added to a plot before they can be used.

        The primitive glyphs are:
        ![](point_glyph_primitive_types.png)

        Note that the letter `'o'` stands for outline only, and the letter
        `'c'` stands for contrast and adds an outline with the
        `'foreground'` color (black by default).
    '''
    return ["circle",
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
            "cdiamond"]



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
# def l_glyph_add(widget, type, ...) {
#     UseMethod("l_glyph_add", type)
# }


def l_glyph_add(widget, type, label="", **options):
    '''Default method for adding non-primitive glyphs

    Generic function to write new glyph types using loon's primitive
    glyphs

    Args:
        type: loon-native non-primitive glyph type, one of `text`,
              `serialaxes'}, `image`, `polygon`, or `pointrange`
        label: label of a glyph (currently shown only in the glyph inspector)
        **options: state arguments
    '''
    ## as.vector strips attributes
    if(isinstance(widget,loon)):
        widget = widget.plot
    res = loon_l_glyph(widget = widget, 
                        id = l_glyph(widget, "add",type, label=label, **options),
                        Type = "l_glyph_" + type)
    return res 


def l_glyph_add_text(widget, text, label="", **options):
    '''Add a Text Glyph

    Each text glyph can be a multiline string.    
    
    Args: 
        text: the text strings for each observartion.
    
    See Also: 
        `l_glyph_add`

    Examples:
        >>> p = l_plot(iris, color = iris.Species)
        >>> g = l_glyph_add_text(p, iris.Species, "test_label")
        >>> p['glyph'] = g
    '''
    if(isinstance(text,pd.Series)):
        text = list(text)
    res = l_glyph_add(widget, "text", text=text, label=label, **options)
    return res 

def l_glyph_add_pointrange(widget, ymin, ymax, linewidth=1, showArea = True, label="", **options):
    '''Add a Pointrange Glyph

    Pointrange glyphs show a filled circle at the x-y location and
    also a y-range.
    
    Args:
        ymin: list with lower y-yalue of the point range.
        ymax: list with upper y-yalue of the point range.
        linewidth: line with in pixel.
        showArea: boolean, show a filled point or just the outline point
    
    See Also:
        l_glyph_add

    Examples:
        >>> p =  l_plot(x = [1,2,3], color = ['red', 'blue', 'green'], showScales=True)
        >>> g = l_glyph_add_pointrange(p, ymin=np.array([1,2,3]) - np.array([1,2,3])/5, ymax=np.array([1,2,3]) + np.array([1,2,3])/5)
        >>> p['glyph'] =  g
    '''
    if(isinstance(ymin,np.ndarray)):
        ymin = list(ymin)
    if(isinstance(ymax,np.ndarray)):
        ymax = list(ymax)
    res = l_glyph_add(widget, "pointrange",
                        ymin=ymin, ymax=ymax, linewidth=linewidth, showArea = showArea,
                        label=label, **options)
    return res 

def l_glyph_add_polygon(widget, x, y, linewidth = 1, showArea=True, label="", **options):    
    '''Add a Polygon Glyph

    Add one polygon per scatterplot point.

    Details: 
        A polygon can be a useful point glyph to visualize arbitrary shapes
        such as airplanes, animals and shapes that are not available in the
        primitive glyph types (e.g. cross). The `l_glyphs` demo has an example
        of polygon glyphs which we reuse here.    

    Args:
        x: nested list of x-coordinates of polygons (relative to ), one list element for each
           scatterplot point.
        y: nested list of y-coordinates of polygons, one list element for each
           scatterplot point.
        linewidth: linewidth of outline.
        showArea: boolean, show a filled polygon or just the outline
    
    See Also: 
        `l_glyph_add`

    Examples:
        >>> x_star = [-0.000864304235090734, 0.292999135695765, 0.949870354364736,
                0.474503025064823, 0.586862575626621, -0.000864304235090734,
                -0.586430423509075, -0.474070872947277, -0.949438202247191,
                -0.29256698357822]
        >>> y_star = [-1, -0.403630077787381, -0.308556611927398, 0.153846153846154,
                    0.808556611927398, 0.499567847882455, 0.808556611927398,
                    0.153846153846154, -0.308556611927398, -0.403630077787381]
        >>> x_cross = [-0.258931143762604, -0.258931143762604, -0.950374531835206,
                    -0.950374531835206, -0.258931143762604, -0.258931143762604,
                    0.259651397291847, 0.259651397291847, 0.948934024776722,
                    0.948934024776722, 0.259651397291847, 0.259651397291847]
        >>> y_cross = [-0.950374531835206, -0.258931143762604, -0.258931143762604,
                    0.259651397291847, 0.259651397291847, 0.948934024776722,
                    0.948934024776722, 0.259651397291847, 0.259651397291847,
                    -0.258931143762604, -0.258931143762604, -0.950374531835206]
        >>> x_hexagon = [0.773552290406223, 0, -0.773552290406223, -0.773552290406223,
                    0, 0.773552290406223]
        >>> y_hexagon = [0.446917314894843, 0.894194756554307, 0.446917314894843,
                    -0.447637568424085, -0.892754249495822, -0.447637568424085]
        >>> p = l_plot([1,2,3])
        >>> gl = l_glyph_add_polygon(p, x = [x_star, x_cross, x_hexagon], y = [y_star, y_cross, y_hexagon])
        >>> p['glyph'] = gl
        >>> gl['showArea'] = False
    '''
    if (isinstance(x[0],list)):
        x = l_list2nestedTclList(x)
    if (isinstance(y[0],list)):
        y = l_list2nestedTclList(y)

    res = l_glyph_add(widget, "polygon",
                        x=x, y=y, linewidth = linewidth, showArea=showArea, label=label, **options)
    return res 


def l_glyph_add_serialaxes(widget,
                           data,
                           sequence=None,
                           linewidth=1,
                           scaling="variable",
                           axesLayout="radial",
                           showAxes=False,
                           axesColor="gray70",
                           showEnclosing=False,
                           bboxColor="gray70",
                           label="",
                           **options):
    '''Add a Serialaxes Glyph

    Serialaxes glyph show either a star glyph or a parralel
    coordinate glyph for each point.

    Args:
        linewidth: linewidth of outline
        axesColor: color of axes
        showEnclosing: boolean, circle (axesLayout=radial) or sqaure
          (axesLayout=parallel) to show bounding box/circle of the glyph (or showing
          unit circle or rectangle with height 1 if scaling=none)
        bboxColor: color of bounding box/circle
    
    Examples:
        >>> p = l_plot(olive.oleic, olive.stearic, color=olive.Area)
        >>> gs = l_glyph_add_serialaxes(p, data=olive.iloc[:,2:olive.shape[1]], showArea=False)
        >>> p['glyph'] = gs
    '''
    if(sequence == None):
        sequence = list(data.columns)

    res = l_glyph_add(widget, "serialaxes",
                        data = l_data(data),
                        sequence=sequence,
                        linewidth=linewidth,
                        scaling=scaling,
                        axesLayout=axesLayout,
                        showAxes=showAxes,
                        axesColor=axesColor,
                        showEnclosing=showEnclosing,
                        bboxColor=bboxColor,
                        label=label ,**options)
    return res 



