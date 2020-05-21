#' @title N dimensional state names access
#' @name l_nDimStateNames
#' @description Get all n dimensional state names
#' @param loon_plot A \code{loon} widget or the class name of a loon plot
#' @export
#' @examples
#' if(interactive()){
#'
#' p <- l_plot()
#' l_nDimStateNames(p)
#' l_nDimStateNames("l_plot")
#'
#' }
l_nDimStateNames  <- function(loon_plot) {
    UseMethod("l_nDimStateNames", loon_plot)
}

#' @export
l_nDimStateNames.loon <- function(loon_plot) {
    info_states <- l_info_states(loon_plot)
    names_p <- names(info_states)

    n_dim_states <-  lapply(seq_along(names(loon_plot)),
                            FUN = function(i){
                                info_name  <- names_p[[i]]
                                info_state <- info_states[[i]]
                                if ("dimension" %in% names(info_state) && !info_name %in% c("colorStackingOrder", "sequence", "axesLabels")) {
                                    if (info_state$dimension == "n" |
                                        info_state$dimension == "-1") {
                                        info_name
                                    }
                                }
                            }
    )
    unlist(n_dim_states)
}

#' @export
l_nDimStateNames.character <- function(loon_plot) {

    result <- n_dim_states_list[[loon_plot]]
    if (is.null(result)) {
        message("No nDimStateNames found for ", loon_plot, ", returning NULL.")
    }
    result
}


n_dim_states_list <- list(
    l_plot.default = c("itemLabel", "x", "y", "color", "xTemp", "yTemp",
                       "selected", "active", "size", "tag", "linkingKey", "glyph"),
    l_plot = c("itemLabel", "x", "y", "color", "xTemp", "yTemp",
               "selected", "active", "size", "tag", "linkingKey", "glyph"),
    l_hist = c("x", "color", "selected", "active", "linkingKey"),
    l_graph = c("itemLabel","glyph" ,"linkingKey" ,"x" ,"y" , "xTemp", "yTemp",
                "color" , "selected" , "active" , "size" , "tag" ,
                "nodes", "orbitAngle"),
    l_serialaxes = c("itemLabel", "linkingKey", "linewidth",
                     "data", "active", "color", "selected", "tag"),
    l_plot3D = c("itemLabel", "x", "y", "z", "color", "xTemp", "yTemp",
                 "selected", "active", "size", "tag", "linkingKey", "glyph"),
    l_layer_lines = c("x", "y", "tag", "itemLabel", "color", "active",
                      "group", "linewidth"),
    l_layer_polygons = c("x", "y", "tag", "itemLabel", "color", "active",
                         "group", "linecolor", "linewidth"),
    l_layer_texts = c("x", "y", "tag", "active", "itemLabel", "color", "text",
                      "angle", "size", "anchor", "justify"),
    l_layer_rectangles = c("x", "y", "tag", "active", "itemLabel", "color",
                           "linecolor", "linewidth"),
    l_layer_points = c("x", "y", "tag", "active", "itemLabel", "size", "color")
)

#  While these might be useful we do not offer them because they require
#  Namespace manipulation.
#
# #' @rdname nDimStateNames
# #' @param name A function name you created
# #' @param key Corresponding n dimensional state names for your created function
# #' @export
# #' @examples
# #' name <- "l_foo"
# #' key <- c("x", "y", "foo")
# #' l_add_nDimStateNames(name, key)
# l_add_nDimStateNames <- function(name, key) {
#
#   if(missing(name)) stop("Name is missing")
#   if(missing(key)) stop("Key is missing")
#   envir <- getNamespace("loon")
#   name <- as.character(name)
#
#   n_dim_states_list <- get("n_dim_states_list", envir = envir)
#
#   c(n_dim_states_list,
#     stats::setNames(list(key), name))
# }
# #' @importFrom utils assignInNamespace
# #' \dontrun{
# #'   l_set_nDimStateNames <- function(name, key) {
# #'     if(missing(name)) stop("Name is missing")
# #'     if(missing(key)) stop("Key is missing")
# #'     envir <- getNamespace("loon")
# #'     name <- as.character(name)
# #'     n_dim_states_list <- get("n_dim_states_list", envir = envir)
# #'     n_dim_states_list <- c(n_dim_states_list,
# #'                            setNames(list(key), name))
# #'     assignInNamespace(x = "n_dim_states_list",
# #''                       value = n_dim_states_list,
# #'                       ns = envir)
# #'     invisible()
# #'  }
# #'
# #'  l_delete_nDimStateNames <- function(name) {
# #'
# #'     if(missing(name)) stop("Name is missing")
# #'
# #'     name <- as.character(name)
# #'     envir <- getNamespace("loon")
# #'
# #'     n_dim_states_list <- get("n_dim_states_list", envir = envir)
# #''     if(!name %in% c("l_plot.default", "l_hist", "l_graph", "l_serialaxes",
# #'                     "l_plot3D", "l_layer_lines", "l_layer_polygons", "l_layer_texts",
# #'                     "l_layer_rectangles", "l_layer_points")) {
# #'         n_dim_states_list[name] <- NULL
# #'         assignInNamespace(x = "n_dim_states_list",
# #'                           value = n_dim_states_list,
# #'                           ns = envir)
# #'     } else {
# #'         warning(
# #'             "N dimension state names of widget ",
# #'             name,
# #'             " are not allowed to be modified" ,
# #'             call. = FALSE)
# #'     }
# #'     invisible()
# #' }
# #'
# #' ## Suppose l_foo is the function you created
# #' ## Its n dimensional state is c("x", "y", "linkingKey")
# #' l_nDimStateNames("l_foo")
# #' l_set_nDimStateNames("l_foo", c("x", "y", "linkingKey"))
# #' l_nDimStateNames("l_foo")
# #' l_delete_nDimStateNames("l_foo")
# #' }

