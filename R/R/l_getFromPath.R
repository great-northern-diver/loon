#' @title Create loon objects from path name
#' @description This function can be used to create the loon objects from
#'   a valid widget path name. The main difference from \code{l_create_handle} is that
#'   \code{l_getFromPath} can take a loon compound widget path
#'   but \code{l_create_handle} cannot.
#'
#' @templateVar page learn_R_intro
#' @templateVar section re-creating-object-handles
#' @template see_l_help
#'
#' @param target loon object specification (e.g. \code{".l0.plot"})
#' @seealso \code{\link{l_create_handle}} \code{\link{l_loonWidgets}}
#' @export
#' @examples
#' \dontrun{
#'  l_pairs(iris, showHistogram = TRUE)
#'  # The path can be found at the top of tk title
#'  # Suppose it is the first loon widget, this path should be ".l0.pairs"
#'  p <- l_create_handle(".l0.pairs") # error
#'  p <- l_getFromPath(".l0.pairs")
#' }

l_getFromPath <- function(target) {

    ## first check for loon objects
    if (is(target,'loon')) {
        loon_obj <- target
    } else {
        ## strip attributes
        specifier <- vapply(target, as.vector, character(1), USE.NAMES=FALSE)

        widget <- specifier[1]

        # create a compound handle
        if (!l_isLoonWidget(widget)) {
            loon_obj <- lapply(as.character(tkwinfo("child", widget)),
                               function(x) {
                                   tryCatch(
                                       expr = {l_create_handle(x)},
                                       error = function(e) return(NULL)
                                   )
                               })
            # return a compound widget
            return(Filter(Negate(is.null), loon_obj))
        }

        loon_obj <- if (length(specifier) == 1) {

            cl <- as.character(tcl("info", "object", "class", widget))

            wcl <- switch(
                cl,
                "::loon::classes::Scatterplot_Widget" = "l_plot",
                "::loon::classes::Scatterplot3D_Widget" = c("l_plot3D", "l_plot"),
                "::loon::classes::Histogram_Widget" = "l_hist",
                "::loon::classes::Serialaxes_Widget" = "l_serialaxes",
                "::loon::classes::Graph_Widget" = "l_graph",
                character(0)
            )
            structure(widget, class = c(wcl, "loon"))
        } else if (length(specifier) == 2) {

            spec_2 <- specifier[2]
            spec_2_short <- substr(spec_2, 1, 5)
            if ( spec_2_short == "layer" || spec_2 == "model") {
                structure(as.vector(spec_2),
                          widget = widget,
                          class = c(paste0("l_layer_", l_layer_getType(widget, specifier[2])),
                                    'l_layer', 'loon'))
            } else if (spec_2 == "root") {
                structure("root", widget=as.vector(widget), class=c("l_layer_group", "l_layer", "loon"))
            } else if (spec_2_short == "glyph") {
                structure(as.vector(specifier[2]),
                          widget= widget,
                          class=c(paste0("l_glyph_", l_glyph_getType(widget, spec_2)), 'l_glyph', 'loon'))

            } else if (spec_2_short == "navig") {
                structure(as.vector(specifier[2]),
                          widget=widget,
                          class=c('l_navigator', 'loon'))
            } else {
                stop(paste0("Invalid target specifier: ", target))
            }
        } else if (length(specifier) == 3) {
            if(substr(specifier[2], 1, 5) != "navig" ||
               substr(specifier[3], 1, 5) != "conte") {
                stop(paste0("Invalid target specifier: ", target))
            } else {
                navigator <- structure(as.vector(specifier[2]),
                                       widget=widget,
                                       class=c('l_navigator', 'loon'))
                structure(as.vector(specifier[3]),
                          navigator=navigator,
                          widget=widget,
                          class=c('l_context', 'loon'))
            }
        } else {
            stop(paste0("Invalid target specifier: ", target))
        }
    }

    loon_obj
}


compound_path <- function(target, type, index) {
    if(index == 0) {
        paste0(target[1], ".", type)
    } else {
        paste0(target[1], ".", type, index)
    }
}
