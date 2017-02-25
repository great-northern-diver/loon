
# aliased in l_cget
#' @export
`[.loon` <- function(target, state) {
    l_cget(target, state)
}

# aliased in l_configure
#' @export
`[<-.loon` <- function(target, state, value) {
    args <- list(target, value)
    names(args) <- c('target', state)
    do.call("l_configure", args)
}



#' @title Print a summary of a loon layer object
#' 
#' @description Prints the layer label and layer type
#' 
#' @param x an \code{l_layer} object
#' @template param_dots_method_not_used
#' 
#' @seealso \code{\link{l_layer}}
#' 
#' @export
print.l_layer <- function(x, ...) {
    cat(paste0('loon layer "', l_layer_getLabel(attr(x, 'widget'), x),
              '" of type ', l_layer_getType(attr(x, 'widget'), x),
              ' of plot ',
              attr(x, 'widget'),
              '\n', sep='')) 
    print(as.vector(x))
}
