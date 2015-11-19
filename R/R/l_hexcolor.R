

#' @export
l_hexcolor <- function(colors) {
    as.character(tcl('::loon::listfns::toHexcolor', colors))
}
