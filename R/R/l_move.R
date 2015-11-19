
#' @export
l_move <- function(widget, ...) {
    l_throwErrorIfNotLoonWidget(widget)
    tcl(widget, "move", ...)
    invisible()
}

#' @export
l_move_valign <- function(widget, which="selected") {
    l_move(widget, "valign", which)
}

#' @export
l_move_halign <- function(widget, which="selected") {
    l_move(widget, "halign", which)
}

#' @export
l_move_vdist <- function(widget, which="selected") {
    l_move(widget, "vdist", which)
}

#' @export
l_move_hdist <- function(widget, which="selected") {
    l_move(widget, "hdist", which)
}

#' @export
l_move_grid <- function(widget, which="selected") {
    l_move(widget, "grid", which)
}

#' @export
l_move_jitter <- function(widget, which="selected", factor=1, amount="") {
    l_move(widget, "jitter", which, factor, amount)
}

#' @export
l_move_reset <- function(widget, which="selected") {
    l_move(widget, "reset", which)
}

