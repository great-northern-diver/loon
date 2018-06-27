
#' Get State Names of Loon Object
#' 
#' States of loon objects can be accessed \code{`[`} and \code{\link{l_cget}}
#' and modified with \code{\link{l_configure}}.
#' 
#' @template param_target
#' 
#' 
#' @return state names
#' 
#' @export
#' 
l_state_names <- function(target) {
    if (!is(target, "loon")) stop("target needs to be of class loon")
    
    names(l_info_states(target))
}

#' Get State Names of Loon Object
#' 
#' States of loon objects can be accessed \code{`[`} and \code{\link{l_cget}}
#' and modified with \code{\link{l_configure}}.
#' 
#' @param x loon object
#' 
#' @return state names
#' 
#' @export
names.loon <- function(x) {
    l_state_names(x)
}