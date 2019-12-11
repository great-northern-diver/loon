
from .loon_class import loon_l_navigator,loon_l_context,loon_l_plot
from .loonobject import loonobject
from .helper import opts_to_list
from .l_navigator import l_navigator
from .l_data import l_data
from sys import exit
def l_context(navigator, *opt_list,**opt_dict):
    if(not isinstance(navigator, loon_l_navigator)):
        exit('navigator should be a loon_l_navigator class object.')
    options = list(opt_list) + opts_to_list(opt_dict)
    obj_eval = loonobject(navigator)
    obj_eval('context', *options)


#' @title Delete a context from a navigator
#'   
#' @description Navigators can have multiple contexts. This function removes a 
#'   context from a navigator.
#'   
#' @param navigator navigator hanlde
#' @param id context id
#'   
#' @templateVar page  learn_R_display_graph
#' @templateVar section contexts
#' @template see_l_help
#'   
#' @seealso \code{\link{l_context_ids}}, \code{\link{l_context_add_context2d}}, 
#'   \code{\link{l_context_add_geodesic2d}},
#'   \code{\link{l_context_add_slicing2d}}, \code{\link{l_context_getLabel}},
#'   \code{\link{l_context_relabel}}
#'   
#' @export
def l_context_delete(navigator, id):
    if(not isinstance(navigator, loon_l_navigator)):
        exit('navigator should be a loon_l_navigator class object.')
    if(isinstance(id,loon_l_navigator)):
        id = id.id 
    l_navigator(navigator, "delete", id)

#' @title List context ids of a navigator
#'   
#' @description Navigators can have multiple contexts. This function list the 
#'   context ids of a navigator.
#'   
#' @inheritParams l_context_delete
#'   
#' @templateVar page  learn_R_display_graph
#' @templateVar section contexts
#' @template see_l_help
#'   
#' @seealso \code{\link{l_context_delete}},
#'   \code{\link{l_context_add_context2d}}, 
#'   \code{\link{l_context_add_geodesic2d}},
#'   \code{\link{l_context_add_slicing2d}}, \code{\link{l_context_getLabel}},
#'   \code{\link{l_context_relabel}}
#'   
#'   
#' @export
def l_context_ids(navigator):
    l_navigator(navigator, "ids")

#' @title Change the label of a context
#'   
#' @description Context labels are eventually used in the context inspector.
#'   This function relabels a context.
#'   
#' @inheritParams l_context_delete
#' @param label context label shown
#'   
#' @templateVar page  learn_R_display_graph
#' @templateVar section contexts
#' @template see_l_help
#'   
#' @seealso \code{\link{l_context_getLabel}}, 
#'   \code{\link{l_context_add_context2d}}, 
#'   \code{\link{l_context_add_geodesic2d}}, \code{\link{l_context_add_slicing2d}}, 
#'   \code{\link{l_context_delete}}
#'   
#' @export
def l_context_relabel(navigator, id, label):
    l_navigator(navigator, "relabel", id, label)

#' @title Query the label of a context
#'   
#' @description Context labels are eventually used in the context inspector. 
#'   This function queries the label of a context.
#'   
#' @inheritParams l_context_delete
#'   
#' @templateVar page  learn_R_display_graph
#' @templateVar section contexts
#' @template see_l_help
#'   
#' @seealso \code{\link{l_context_getLabel}}, 
#'   \code{\link{l_context_add_context2d}}, 
#'   \code{\link{l_context_add_geodesic2d}},
#'   \code{\link{l_context_add_slicing2d}}, \code{\link{l_context_delete}}
#'   
#' @export
def l_context_getLabel(navigator, id):
    res = ' '.join(l_navigator(navigator, "getLabel", id))
    return res 

def l_context_add(navigator, type, *opt_list,**opt_dict):
    if(not isinstance(navigator, loon_l_navigator)):
        exit('navigator should be a loon_l_navigator class object.')
    
    obj_eval = loonobject(navigator)
    options = list(opt_list) + opts_to_list(opt_dict)
    res = obj_eval('context', 'add', type, *options)
    res = loon_l_context(res,navigator.widget,navigator.id)
    return res 

#' @title Create a context2d navigator context
#'   
#' @description A context2d maps every location on a 2d space graph to a list of
#'   xvars and a list of yvars such that, while moving the navigator along the 
#'   graph, as few changes as possible take place in xvars and yvars.
#'   
#' @template details_contexts
#'   
#' @param navigator navigator handle object
#' @param ... arguments passed on to modify context states
#'   
#' @return context handle
#'   
#' @seealso \code{\link{l_info_states}}, \code{\link{l_context_ids}}, 
#'   \code{\link{l_context_add_geodesic2d}}, 
#'   \code{\link{l_context_add_slicing2d}}, \code{\link{l_context_getLabel}}, 
#'   \code{\link{l_context_relabel}}
#'   
#'   
#' @export
def l_context_add_context2d(navigator, *opt_list,**opt_dict):
    l_context_add(navigator, 'context2d', *opt_list,**opt_dict)

#' @title Create a geodesic2d navigator context
#'   
#' @description Geodesic2d maps every location on the graph as an orthogonal
#'   projection of the data onto a two-dimensional subspace. The nodes then
#'   represent the sub-space spanned by a pair of variates and the edges either
#'   a 3d- or 4d-transition of one scatterplot into another, depending on how
#'   many variates the two nodes connected by the edge share (see Hurley and
#'   Oldford 2011). The geodesic2d context inherits from the context2d context. 
#'   
#' @template details_contexts
#'   
#' @inheritParams l_context_add_context2d
#'   
#' @return context handle
#'   
#' @seealso \code{\link{l_info_states}}, \code{\link{l_context_ids}}, 
#'   \code{\link{l_context_add_context2d}}, 
#'   \code{\link{l_context_add_slicing2d}}, \code{\link{l_context_getLabel}}, 
#'   \code{\link{l_context_relabel}}
#'   
#'   
#' @export
def l_context_add_geodesic2d(navigator, *opt_list, **opt_dict):

    if('data' in opt_dict.keys()):
        opt_dict['data'] = l_data(opt_dict['data'])
    #args = list(opt_list) + opts_to_list(opt_dict) 
   
    g2d = l_context_add(navigator,'geodesic2d',*opt_list,**opt_dict)

    # add plot handle
    if('command' not in opt_dict.keys()):
        g2d.add_plot_handle(g2d['command'].split(' ')[0])  
    return g2d


#' @title Create a slicind2d navigator context
#'   
#' @description The slicing2d context implements slicing using navigation graphs
#'   and a scatterplot to condition on one or two variables.
#'   
#' @template details_contexts
#'   
#' @inheritParams l_context_add_context2d
#'   
#' @return context handle
#'   
#' @export
#' 
#' @examples 
#' names(oliveAcids) <- c('p','p1','s','o','l','l1','a','e')
#' nodes <- apply(combn(names(oliveAcids),2),2,
#'               function(x)paste(x, collapse=':'))
#' G <- completegraph(nodes)
#' g <- l_graph(G)
#' nav <- l_navigator_add(g)
#' con <- l_context_add_slicing2d(nav, data=oliveAcids)
#' 
#' # symmetric range proportion around nav['proportion']
#' con['proportion'] <- 0.2
#' 
#' con['conditioning4d'] <- "union"
#' con['conditioning4d'] <- "intersection"
#' 
def l_context_add_slicing2d(navigator, *opt_list, **opt_dict):   
    if(not isinstance(navigator, loon_l_navigator)):
        exit('navigator should be a loon_l_navigator class object.')

    if('data' in opt_dict.keys()):
        opt_dict['data'] = l_data(opt_dict['data'])
    #args = list(opt_list) + opts_to_list(opt_dict) 
   
    con = l_context_add(navigator.id,'slicing2d',*opt_list,**opt_dict)

    con.add_plot_xy_handle(con['plot_xy'])
    con.add_plot_uv_handle(con['plot_uv'])
    #attr(con, 'plot_xy')  <- structure(con['plot_xy'], class='loon')
    #attr(con, 'plot_uv')  <- structure(con['plot_uv'], class='loon')
    return con 

