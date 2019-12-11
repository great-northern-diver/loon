from .tk import tk
from .l_throwErrorIfNotLoonWidget import l_throwErrorIfNotLoonWidget
from .helper import opts_to_list
def l_move(widget,*args,**kwargs):
    '''l_move 
    
    @namespace loon.l_move
    '''
    l_throwErrorIfNotLoonWidget(widget)    
    opts = opts_to_list(kwargs)
    tk.tk.call(widget, "move", *opts)


#' @title Vertically Align Points or Nodes
#'   
#' @template body_l_move
#'   
#' @export
def l_move_valign(widget, which="selected"):
    l_move(widget, "valign", which = which)


#' @title Horizontally Align Points or Nodes
#' 
#' @template body_l_move
#' 
#' @export
def l_move_halign(widget, which="selected"):
    l_move(widget, "halign", which = which)


#' @title Vertically Distribute Points or Nodes
#' 
#' @template body_l_move
#' 
#' @export
def l_move_vdist(widget, which="selected"):
    l_move(widget, "vdist", which = which )


#' @title Horizontally Distribute Points or Nodes
#' 
#' @template body_l_move
#' 
#' @export
def l_move_hdist(widget, which="selected"):
    l_move(widget, "hdist", which=which)


#' @title Arrange Points or Nodes on a Grid
#' 
#' @template body_l_move
#'  
#' @export
def l_move_grid(widget, which="selected"):
    l_move(widget, "grid", which = which)


#' @title Jitter Points Or Nodes
#' 
#' @template body_l_move
#' 
#' @inheritParams base::jitter
#' 
#' @export
def l_move_jitter(widget, which="selected", factor=1, amount=""):
    l_move(widget, "jitter", which = which, factor=factor, amount=amount)


#' @title Reset Temporary Point or Node Locations to the x and y states
#' 
#' @template body_l_move
#' 
#' @export
def l_move_reset(widget, which="selected"):
    l_move(widget, "reset", which = which)

