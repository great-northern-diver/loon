
# Change all title fonts of loon's displays
# @examples 
# loon:::l_setTitleFont(14)
l_setTitleFont <- function(size='16', weight="bold", family="Helvetica") {
    tcl('font', 'configure',
        tcl('set', '::loon::Options(font-title)'),
        weight=weight, size=size, family=family)
    invisible()
}

