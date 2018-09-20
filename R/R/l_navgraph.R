
#' @title Explore a dataset with the canonical 2d navigation graph setting
#'   
#' @description Creates a navigation graph, a graphswitch, a navigator and a 
#'   geodesic2d context added, and a scatterplot.
#' 
#' @param data a data.frame with numeric variables only
#' @template param_separator
#' @param graph optional, graph or loongraph object with navigation graph. If 
#'   the graph argument is not used then a 3d and 4d transition graph and a 
#'   complete transition graph is added.
#' @param ... arguments passed on to modify the scatterplot plot states
#' 
#' @templateVar page  learn_R_display_graph
#' @templateVar section l_navgraph
#' @template see_l_help
#' 
#' @return named list with \code{graph} handle, \code{plot} handle,
#'   \code{graphswitch} handle, \code{navigator} handle, and \code{context}
#'   handle.
#'   
#' @export
#' 
#' @examples 
#' ng <- l_navgraph(oliveAcids, color=olive$Area)
#' ng2 <- l_navgraph(oliveAcids, separator='-', color=olive$Area)



l_navgraph <- function(data, separator=":", graph=NULL,  ...) {

    if(!is.data.frame(data)) {
        data <- as.data.frame(data)
    }
    
    tt <- l_toplevel()

    if (is.null(graph)) {
        G <- completegraph(nodes=names(data))
        LG <- linegraph(G, sep=separator)
        LGnot <- loon::complement(LG)
        
        cmb <- utils::combn(names(data),2)
        CompG <- completegraph(nodes=apply(cmb[,cmb[1,]!=cmb[2,]],2,
                                   FUN=function(x)paste(x, collapse=separator)))
        
        g <- l_graph(LG, parent=tt)
    } else {
        g <- l_graph(graph, parent=tt)
    }

    tktitle(tt) <- paste0("loon navigation graph: ", g)
    
    gs <- l_graphswitch(activewidget=g, parent=tt)

    if (is.null(graph)) {
        l_graphswitch_add(gs, graph=LG, label="3d transition")
        l_graphswitch_add(gs, graph=LGnot, label="4d transition")
        l_graphswitch_add(gs, graph=CompG, label="3d and 4d transition")
    } else {
        gid <- l_graphswitch_add(gs, graph=graph, label="user specified")
    }

    tkpack(gs, side='right', fill='y')    
    tkpack(g, side='right', fill='both', expand=TRUE)


    navigator <- l_navigator_add(g, label="default navgraph")
    context <- l_context_add_geodesic2d(navigator, data=data, separator=separator)  

    plot <- strsplit(context['command'], " ", fixed = TRUE)[[1]][1]
    class(plot) <- c("l_plot", "loon")

    args <- list(...)
    if (length(args)>0) {
        l_configure(plot, ...)
    }
    
    navgraph <- list(graph=g, plot=plot, graphswitch=gs,
                     navigator=navigator, context=context)
    
    class(navgraph) <- c("l_navgraph", "l_compound", "loon")
    
    navgraph
}

#' @export
l_getPlots.l_navgraph <- function(target){
    list(graph = target$graph, plot = target$plot)
}