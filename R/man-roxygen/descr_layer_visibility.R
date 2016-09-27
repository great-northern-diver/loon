#' @details Visibile layers are rendered, invisible ones are not. If any
#'   ancestor of a layer is set to be invisible then the layer is not rendered
#'   either. The layer visibility flag can be checked with
#'   \code{\link{l_layer_isVisible}} and the actual visibility (i.e. are all the
#'   ancesters visibile too) can be checked with
#'   \code{\link{l_layer_layerVisibility}}.
#' 
#'   Note that layer visibility is not a state of the layer itself,
#'   instead is information that is part of the layer collection (i.e. its
#'   parent widget).
#'   
#' 
