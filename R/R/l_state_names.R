#' @export
l_state_names <- function(target) {
    if (!is(target, "loon")) stop("target needs to be of class loon")
    
    names(l_info_states(target))
}

#' @export
names.loon <- function(x) {
    l_state_names(x)
}