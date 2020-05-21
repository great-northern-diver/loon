# Inspector Factory
loonInspectorFactory <- function(factory_tclcmd, factory_path, factory_window_title="loon widget", parent=NULL, ...) {

    new.toplevel <- FALSE
    if(is.null(parent)) {
        new.toplevel <- TRUE
        parent <- l_toplevel()
    }

    child <- l_subwin(parent, factory_path)

    ## Make try catch and delete toplevel if necessary
    widget <-  try(as.character(tcl(factory_tclcmd, child, ...)), silent=TRUE)

    if(is(widget, "try-error")) {
        if(new.toplevel) tkdestroy(parent)
        stop(paste0(factory_window_title, " could not be created."))
    }

    if (new.toplevel) {
        tktitle(parent) <- paste(factory_window_title, child)
        tkpack(widget, fill="both", expand=1)
    }

    class(widget) <- "loon"
    return(widget)
}


#' @title Create a loon inspector
#'
#' @description The loon inspector is a singleton widget that provids an
#'   overview to view and modify the active plot.
#'
#' @template param_parent
#' @template param_dots_state_args
#'
#' @templateVar page learn_R_display_inspectors
#' @templateVar section graph-utilities
#' @template see_l_help_page
#'
#' @template return_widget_handle
#'
#' @export
#'
#' @examples
#' if(interactive()){
#' i <- l_loon_inspector()
#' }
l_loon_inspector <- function(parent=NULL, ...) {
    currentInspector <- as.character(tcl('set', 'loon::loonInspector'))
    if(currentInspector == "") {
        loonInspectorFactory("::loon::loon_inspector", "looninspector",
                             "Loon inspector", parent, ...)
    } else {
        class(currentInspector) <- "loon"
        currentInspector
    }
}


#' @templateVar type Worldview
#' @template man_inspector
#'
#' @export
#'
#' @examples
#' if(interactive()){
#' i <- l_worldview()
#' }
l_worldview <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::worldview", "worldview",
                         "Worldview inspector", parent, ...)
}

#' @templateVar type Layers
#' @template man_inspector
#'
#' @export
#' @examples
#' if(interactive()){
#'
#' i <- l_layers_inspector()
#' }
l_layers_inspector <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::layers_inspector", "layersinspector",
                         "Layers inspector", parent, ...)
}

#' @templateVar type Glyphs
#' @template man_inspector
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' i <- l_glyphs_inspector()
#' }
l_glyphs_inspector <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::glyphs_inspector", "glyphinspector",
                         "Glyph inspector", parent, ...)
}

#' @templateVar type Serialaxes Glyph
#' @template man_inspector
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' i <- l_glyphs_inspector_serialaxes()
#' }
l_glyphs_inspector_serialaxes <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::serialaxes_glyph_inspector", "glyphsinspectorserialaxes",
                         "Serialaxes glyph inspector", parent, ...)
}

#' @templateVar type Pointrange Glyph
#' @template man_inspector
#'
#' @export
#' @examples
#' if(interactive()){
#'
#' i <- l_glyphs_inspector_pointrange()
#' }
l_glyphs_inspector_pointrange <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::pointrange_glyph_inspector", "glyphsinspectorpointrange",
                         "Pointrange glyph inspector", parent, ...)
}


#' @templateVar type Text Glyph
#' @template man_inspector
#'
#' @export
#' @examples
#' if(interactive()){
#'
#' i <- l_glyphs_inspector_text()
#' }
l_glyphs_inspector_text <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::text_glyph_inspector", "glyphsinspectortext",
                         "Text glyph inspector", parent, ...)
}


#' @templateVar type Image Glyph
#' @template man_inspector
#'
#' @export
#' @examples
#' if(interactive()){
#'
#' i <- l_glyphs_inspector_image()
#' }
l_glyphs_inspector_image <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::image_glyph_inspector", "glyphsinspectorimage",
                         "Image glyph inspector", parent, ...)
}


#' @templateVar type Scatterplot
#' @template man_inspector
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' i <- l_plot_inspector()
#' }
l_plot_inspector <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::plot_inspector", "plotinspector",
                         "Scatterplot inspector", parent, ...)
}


#' @templateVar type Scatterplot Analysis
#' @template man_inspector
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' i <- l_plot_inspector_analysis()
#' }
l_plot_inspector_analysis <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::plot_inspector_analysis", "plotinspectoranalysis",
                         "Scatterplot analysis inspector", parent, ...)
}


#' @templateVar type Histogram
#' @template man_inspector
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' i <- l_hist_inspector()
#' }
l_hist_inspector <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::histogram_inspector", "histinspector",
                         "Histogram inspector", parent, ...)
}


#' @templateVar type Histogram Analysis
#' @template man_inspector
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' i <- l_hist_inspector_analysis()
#' }
l_hist_inspector_analysis <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::histogram_inspector_analysis", "histinspectoranalysis",
                         "Histogram analysis inspector", parent, ...)
}


#' @templateVar type Serialaxes Analysis
#' @template man_inspector
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' i <- l_serialaxes_inspector()
#' }
l_serialaxes_inspector <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::serialaxes_inspector", "serialaxesinspector",
                         "Serialaxes inspector", parent, ...)
}


#' @templateVar type Graph
#' @template man_inspector
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' i <- l_graph_inspector()
#' }
l_graph_inspector <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::graph_inspector", "graphinspector",
                         "Graph inspector", parent, ...)
}

#' @templateVar type Graph Analysis
#' @template man_inspector
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' i <- l_graph_inspector_analysis()
#' }
l_graph_inspector_analysis <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::graph_inspector_analysis", "graphinspectoranalysis",
                         "Graph analysis inspector", parent, ...)
}

#' @templateVar type Graph Navigator
#' @template man_inspector
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' i <- l_graph_inspector_navigators()
#' }
l_graph_inspector_navigators <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::navigators_inspector", "graphinspectornavigators",
                         "Graph navigators inspector", parent, ...)
}
