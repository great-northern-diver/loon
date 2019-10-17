
#' Get State Names of Loon Object
#'
#' States of loon objects can be accessed \code{`[`} and \code{\link{l_cget}}
#' and modified with \code{\link{l_configure}}.
#'
#' @family loon interactive states
#' @template param_target
#'
#' @details
#' In order to access values of a states use \code{\link{l_info_states}}.
#'
#'
#' @return state names
#'
#' @aliases {names.loon}
#'
#' @export
#'
#' @seealso
#' \code{\link{l_info_states}}, \code{\link{l_cget}}, \code{\link{l_configure}}
#'
l_state_names <- function(target) {
    if (!is(target, "loon")) stop("target needs to be of class loon")

    names(l_info_states(target))
}

#' Get State Names of Loon Object
#'
#' States of loon objects can be accessed \code{`[`} and \code{\link{l_cget}}
#' and modified with \code{\link{l_configure}}.
#' @family loon interactive states
#'
#' @param x loon object
#'
#' @return state names
#'
#' @aliases {l_state_names}
#'
#' @export
names.loon <- function(x) {
    l_state_names(x)
}
