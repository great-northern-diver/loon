#' @title Set the title font of all loon displays
#'
#' @description All of loon's displays access a set of common options.
#' This function sets the font for the title bar of the displays.
#'
#' @param size the font size.
#' @param weight the font size.
#' @param family the font family.
#'
#' @return the value of the named option.
#'
#' @seealso \code{\link{l_getOptionNames}}, \code{\link{l_userOptions}},
#'   \code{\link{l_userOptionDefault}}, \code{\link{l_setOption}}
#'
l_setTitleFont <- function(size='16', weight="bold", family="Helvetica") {
    tcl('font', 'configure',
        tcl('set', '::loon::Options(font-title)'),
        weight=weight, size=size, family=family)
    invisible()
}

