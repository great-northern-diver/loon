
# TODO
l_colorcrosstable <- function(group="", ...) {
    p <- as.character(tcl('::loon::colorcrosstable', group=group, ...))
}
