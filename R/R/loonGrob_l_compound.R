#' @rdname loonGrob
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' library(grid)
#' ## l_pairs (scatterplot matrix) examples
#'
#' p <- l_pairs(iris[,-5], color=iris$Species)
#'
#' lgrob <- loonGrob(p)
#' grid.newpage()
#' grid.draw(lgrob)
#'
#' ## Time series decomposition examples
#'
#' decompose <- decompose(co2)
#' # or decompose <- stl(co2, "per")
#' p <- l_plot(decompose, title = "Atmospheric carbon dioxide over Mauna Loa")
#'
#' # To print directly use either
#' plot(p)
#' # or
#' grid.loon(p)
#' # or to save structure
#' lgrob <- loonGrob(p)
#' grid.newpage()
#' grid.draw(lgrob)
#' }
#'
#' @export
loonGrob.l_compound <- function(target, name = NULL, gp = NULL, vp = NULL){
    arrangeGrob.args <- switch(loonGrob_layoutType(target),
                               locations = {
                                   plots <- l_getPlots(target)
                                   grobs <- lapply(plots, function(w){loonGrob(w)})
                                   locations <- l_getLocations(target)
                                   is_locationNames <- names(locations) %in% c("ncol", "nrow",
                                                                               "layout_matrix",
                                                                               "heights", "widths")
                                   if(!all(is_locationNames)) {
                                       warning(
                                         paste(c("l_getLocations() returned some named components not in ",
                                               "{ncol, nrow, layout_matrix, heights, widths}.", "\n",
                                               "Only {",
                                               paste(names(locations)[is_locationNames], collapse=", "),
                                               "} will be used to determine location."
                                               )
                                         ))
                                   locations <- locations[is_locationNames]
                                   }
                                   c(list(grobs = grobs), locations)
                               },
                               arrangeGrobArgs = {
                                   l_get_arrangeGrobArgs(target)
                               },
                               stop("target has unknown loonGrob_layout type")
    )


    if (!"name" %in% names(arrangeGrob.args)) {
        arrangeGrob.args <- c(name = class(target)[1],
                              arrangeGrob.args
        )
    }

    compoundGrob <- l_createCompoundGrob(target, arrangeGrob.args)

    gTree(
        children = gList(compoundGrob),
        name = if (is.null(name)) "compound" else name,
        vp = vp ,
        gp = gp
    )
}

#' A generic function used to distinguish whether only the locations
#' of plots will be used to arrange them in a grob, or whether
#' all arguments to `gridExtra::arrangeGrob()` will be used.
#'
#' @param target the (compound) loon plot to be laid out.
#' @return either the string "locations" (the default) or the string "arrangeGrobArgs".
#' If "locations", then the generic function `l_getLocations()` will be called
#' and only the location arguments of `gridExtra::arrangeGrob()` used
#' (i.e. a subset of `c("ncol", "nrow", "layout_matrix", "heights", "widths")`).
#' The grobs to be laid out are constructed using the generic function `l_getPlots()`.
#'
#' @export
loonGrob_layoutType <- function(target) {
    UseMethod("loonGrob_layoutType", target)
}

loonGrob_layoutType.default <- function(target) {
    stop("loonGrob_layoutType.default has no valid inheritance; not an l_compound plot")
}

loonGrob_layoutType.l_compound <- function(target){
    "locations"
}

#' For the target (compound) loon plot, determines all arguments
#' (i.e. including the grobs) to be passed to `gridExtra::arrangeGrob()`
#' so as to determine the layout in `grid` graphics.
#'
#' @param target the (compound) loon  plot to be laid out.
#' @return a list of the named arguments and their values to be passed to
#' `gridExtra::arrangeGrob()`.
#'
#' @export
l_get_arrangeGrobArgs <- function(target){
    UseMethod("l_get_arrangeGrobArgs", target)
}

#' @export
l_get_arrangeGrobArgs.default <- function(target){
    stop("l_get_arrangeGrobArgs.default has no valid inheritance; not an l_compound plot")
}

#' @export
l_get_arrangeGrobArgs.l_compound <- function(target){
    stop("l_get_arrangeGrobArgs.l_compound has no valid inheritance; needs to be specialized for each type of l_compound")
}


#' For the target compound loon plot, creates the final grob from
#' the class of the `target` and the `arrangeGrob.args`
#'
#' @param target the (compound) loon  plot
#' @param arrangeGrob.args arguments as described by `gridExtra::arrangeGrob()`
#'
#' @return a grob (or list of grobs) that can be handed to `gTree()` as
#' `children = gList(returnedValue)` as the final grob constructed for
#' the compound loon plot.  Default for an `l_compound` is to simply execute
#' `gridExtra::arrangeGrob(arrangeGrob.args)`.
#'
#' @export
l_createCompoundGrob <- function(target, arrangeGrob.args) {
    if (!all(names(arrangeGrob.args) %in% names(as.list(args(gridExtra::arrangeGrob))))) {
        stop("arrangeGrob.args must be named arguments of gridExtra::arrangeGrob()")
    }
    UseMethod("l_createCompoundGrob", target)
}

#' @export
l_createCompoundGrob.default <- function(target, arrangeGrob.args){
    stop("l_createCompoundGrob.default has no valid inheritance; not an l_compound plot")
}

#' @export
l_createCompoundGrob.l_compound <- function(target, arrangeGrob.args){
    do.call(gridExtra::arrangeGrob,  arrangeGrob.args)
}

