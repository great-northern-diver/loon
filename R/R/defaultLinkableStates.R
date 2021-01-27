.defaultLinkableStates_l_hist <- c("selected", "color", "active")
.defaultLinkableStates_l_plot <- c("selected", "color", "active", "size")
.defaultLinkableStates_l_plot3D <- c("selected", "color", "active", "size")
.defaultLinkableStates_l_serialaxes <- c("selected", "color", "active")
.defaultLinkableStates_l_graph <- c("selected", "color", "active", "size")

hasDefaultLinkableStatesVar <- function(l_className) {
    tryCatch(eval(parse(text = paste0(".defaultLinkableStates_", l_className))),
             error = function(e) {
                 warning(substitute(deparse(l_className)), " does not have linked states.")
             })
}
