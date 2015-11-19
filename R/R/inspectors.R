#' Inspector Factory
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


#' Create a loon linspector
#' 
#' @export
#' @examples 
#' i <- l_loon_inspector()
l_loon_inspector <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::loon_inspector", "looninspector",
                         "Loon inspector", parent, ...)
}


#' Worldview Inspector
#' @export
#' @examples 
#' i <- l_worldview()
l_worldview <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::worldview", "worldview",
                         "Worldview inspector", parent, ...)
}

#' Layers inspector
#' @export
#' @examples 
#' i <- l_layers_inspector()
l_layers_inspector <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::layers_inspector", "layersinspector",
                         "Layers inspector", parent, ...)
}

#' Glyphs inspector
#' @export
#' @examples 
#' i <- l_glyphs_inspector()
l_glyphs_inspector <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::glyphs_inspector", "glyphinspector",
                         "Glyph inspector", parent, ...)
}

#' Serialaxes Glyphs inspector
#' @export
#' @examples 
#' i <- l_glyphs_inspector_serialaxes()
l_glyphs_inspector_serialaxes <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::serialaxes_glyph_inspector", "glyphsinspectorserialaxes",
                         "Serialaxes glyph inspector", parent, ...)
}

#' Pointrange Glyph inspector
#' @export
#' @examples 
#' i <- l_glyphs_inspector_pointrange()
l_glyphs_inspector_pointrange <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::pointrange_glyph_inspector", "glyphsinspectorpointrange",
                         "Pointrange glyph inspector", parent, ...)
}


#' Text Glyph inspector
#' @export
#' @examples 
#' i <- l_glyphs_inspector_text()
l_glyphs_inspector_text <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::text_glyph_inspector", "glyphsinspectortext",
                         "Text glyph inspector", parent, ...)
}

#' Image Glyph inspector
#' @export
#' @examples 
#' i <- l_glyphs_inspector_image()
l_glyphs_inspector_image <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::image_glyph_inspector", "glyphsinspectorimage",
                         "Image glyph inspector", parent, ...)
}


#' @title Create a Scatterplot inspector
#'
#'
#' @description
#'
#' The loon plot inspector is a singelton and displays the correct
#' inspector for the active plot widget that has "loon" as an element
#' of -inspector.
#'
#'
#' @export
#' @examples 
#' i <- l_plot_inspector()
l_plot_inspector <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::plot_inspector", "plotinspector",
                         "Scatterplot inspector", parent, ...)
}

#' Scatterplot Analysis Inspector
#' @export
#' @examples 
#' i <- l_plot_inspector_analysis()
l_plot_inspector_analysis <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::plot_inspector_analysis", "plotinspectoranalysis",
                         "Scatterplot analysis inspector", parent, ...)
}


#' Histogram Inspector
#' @export
#' @examples 
#' i <- l_hist_inspector()
l_hist_inspector <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::histogram_inspector", "histinspector",
                         "Histogram inspector", parent, ...)
}

#' Histogram Analysis Inspector
#' @export
#' @examples 
#' i <- l_hist_inspector_analysis()
l_hist_inspector_analysis <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::histogram_inspector_analysis", "histinspectoranalysis",
                         "Histogram analysis inspector", parent, ...)
}



#' Serialaxes inspector
#' @export
#' @examples 
#' i <- l_serialaxes_inspector()
l_serialaxes_inspector <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::serialaxes_inspector", "serialaxesinspector",
                         "Serialaxes inspector", parent, ...)
}

#' Graph inspector
#' @export
#' @examples 
#' i <- l_graph_inspector()
l_graph_inspector <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::graph_inspector", "graphinspector",
                         "Graph inspector", parent, ...)
}

#' Graph Analysis Inspector
#' @export
#' @examples 
#' i <- l_graph_inspector_analysis()
l_graph_inspector_analysis <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::graph_inspector_analysis", "graphinspectoranalysis",
                         "Graph analysis inspector", parent, ...)
}

#' Graph Navigator Inspector
#' @export
#' @examples 
#' i <- l_graph_inspector_navigators()
l_graph_inspector_navigators <- function(parent=NULL, ...) {
    loonInspectorFactory("::loon::navigators_inspector", "graphinspectornavigators",
                         "Graph navigators inspector", parent, ...)
}
