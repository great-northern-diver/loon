

l_move <- function(widget, ...) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, "move", ...)
    invisible()
}

#' @title Vertically Align Points or Nodes
#'   
#' @template body_l_move
#'   
#' @export
l_move_valign <- function(widget, which="selected") {
    l_move(widget, "valign", which)
}

#' @title Horizontally Align Points or Nodes
#' 
#' @template body_l_move
#' 
#' @export
l_move_halign <- function(widget, which="selected") {
    l_move(widget, "halign", which)
}

#' @title Vertically Distribute Points or Nodes
#' 
#' @template body_l_move
#' 
#' @export
l_move_vdist <- function(widget, which="selected") {
    l_move(widget, "vdist", which)
}

#' @title Horizontally Distribute Points or Nodes
#' 
#' @template body_l_move
#' 
#' @export
l_move_hdist <- function(widget, which="selected") {
    l_move(widget, "hdist", which)
}

#' @title Arrange Points or Nodes on a Grid
#' 
#' @template body_l_move
#'  
#' @export
l_move_grid <- function(widget, which="selected") {
    l_move(widget, "grid", which)
}

#' @title Jitter Points Or Nodes
#' 
#' @template body_l_move
#' 
#' @inheritParams base::jitter
#' 
#' @export
l_move_jitter <- function(widget, which="selected", factor=1, amount="") {
    l_move(widget, "jitter", which, factor, amount)
}

#' @title Reset Temporary Point or Node Locations to the x and y states
#' 
#' @template body_l_move
#' 
#' @export
l_move_reset <- function(widget, which="selected") {
    l_move(widget, "reset", which)
}
