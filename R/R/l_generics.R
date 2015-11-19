
#' @export
"[.loon" <- function(target, state) {
    l_cget(target, state)
}

#' @export
"[<-.loon" <- function(target, state, value) {
    args <- list(target, value)
    names(args) <- c('target', state)
    do.call("l_configure", args)
}



#' experimental
#' @export
print.l_layer <- function(x) {
    cat(paste0('loon layer "', l_layer_getLabel(attr(x, 'widget'), x),
              '" of type ', l_layer_getType(attr(x, 'widget'), x),
              ' of plot ',
              attr(x, 'widget'),
              '\n', sep='')) 
    print(as.vector(x))
}
