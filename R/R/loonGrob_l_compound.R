#' @rdname loonGrob
#' 
#' 
#' @examples
#'  
#' \dontrun{
#' ## Time series decomposition examples
#' 
#' decompose <- decompose(co2) 
#' # or decompose <- stl(co2, "per")
#' p <- l_plot(decompose, title = "Atmospheric carbon dioxide over Mauna Loa")
#' 
#' library(grid)
#' lgrob <- loonGrob(p)
#' grid.newpage()
#' grid.draw(lgrob)
#' }
#' 
#' @export

loonGrob.l_compound <- function(target, name = NULL, gp = NULL, vp = NULL){
    
    switch(loonGrob_layoutType(target),
           default = {
               plots <- l_getPlots(target)
               grobs <- lapply(plots, function(w){loonGrob(w)})
               locations <- l_getLocations(target)
               is_locationNames <- names(locations) %in% c("ncol", "nrow", "layout_matrix", "heights", "widths")
               if(!all(is_locationNames)) {
                   locations <- locations[is_locationNames]  
               } 
               arrangeGrob.args <- c(grobs = grobs, locations) 
           },
           arrangeGrobArgs = {
               arrangeGrob.args <- l_get_arrangeGrobArgs(target)
           },
           stop("target has unknown loonGrob_layout type")
    )
    
    
    if (!"name" %in% names(arrangeGrob.args)) {
        arrangeGrob.args <- c(
            list(name = class(target)[1]),
            arrangeGrob.args
        )
    }
    
    gTree( 
        children = gList(
            do.call(gridExtra::arrangeGrob,  arrangeGrob.args)
        ),
        name = name, 
        vp = vp , 
        gp = gp
    )
}

loonGrob_layoutType <- function(target) {
    UseMethod("loonGrob_layoutType", target)
}

loonGrob_layoutType.default <- function(target) {
    "default"
}

loonGrob_layoutType.l_pairs <- function(target) {
    "arrangeGrobArgs"
}

l_get_arrangeGrobArgs <- function(target){
    UseMethod("l_get_arrangeGrobArgs", target) 
}

l_get_arrangeGrobArgs.default <- function(target){
    NULL
}

l_get_arrangeGrobArgs.l_ts <- function(target){
    plots <- target
    nPlots <- length(plots)
    list(
        grobs = lapply(plots, 
                       function(w){loonGrob(w)}),
        nrow = nPlots,
        ncol = 1,
        heights = c(1.3, rep(1, nPlots-1))
    )
}

l_getPlots <- function(target) {
    UseMethod("l_getPlots", target)
}

l_getPlots.default <- function(target) {
    stop("l_getPlots.default no valid inheritance")
}

l_getLocations <- function(target) {
    UseMethod("l_getLocations", target)
}

l_getLocations.default <- function(target) {
    stop("l_getLocations.default no valid inheritance")
}

l_getPlots.l_ts <- function(target){
    # throw errors if not loon widget
    lapply(target, 
           function(tar){
               l_throwErrorIfNotLoonWidget(tar)
           }
    )
    target
}

l_getLocations.l_ts <- function(target){
    nPlots <- length(target)
    list(
        nrow = nPlots,
        ncol = 1,
        heights = c(1.3, rep(1, nPlots-1))
    )
}