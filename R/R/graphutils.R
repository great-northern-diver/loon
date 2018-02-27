##
## Helper functions to create simple graphs
##

#' @title Create a graph object of class loongraph
#'   
#' @description The loongraph class provides a simple alternative to the graph 
#'   class to create common graphs that are useful for use as navigation graphs.
#'   
#'   
#' @param nodes a character vector with node names, each element defines a node
#'   hence the elements need to be unique
#' @param from a character vector with node names, each element defines an edge
#' @param to  a character vector with node names, each element defines an edge
#' @param isDirected boolean scalar, defines whether from and to define directed
#'   edges
#'   
#'   
#' @details loongraph objects can be converted to graph objects (i.e. objects of
#' class graph which is defined in the graph package) with the as.graph 
#' function.
#' 
#' @templateVar page learn_R_display_graph.html
#' @templateVar section graph-utilities
#' @template see_l_help
#'
#' @template return_loongraph 
#' 
#' @seealso \code{\link{completegraph}}, \code{\link{linegraph}}, 
#'   \code{\link{complement}}, \code{\link{as.graph}}
#'   
#' @export
#'      
#' @examples 
#' g <- loongraph(
#'   nodes = c("A", "B", "C", "D"),
#'   from = c("A", "A", "B", "B", "C"),
#'   to   = c("B", "C", "C", "D", "D")
#' )
#' 
#' \dontrun{
#' # create a loon graph plot
#' p <- l_graph(g)
#' }
#' 
#' lg <- linegraph(g)
loongraph <- function(nodes, from=character(0), to=character(0), isDirected=FALSE) {

    if (length(nodes) != length(unique(nodes)))
        warning("node names are not unique")
    
    if (length(from) != length(to))
        stop(paste0("'from' and 'to' differ in length: ",
                    length(from), ' vs. ', length(to)))
    
    if (length(from) != 0  && !all(from %in% nodes))
        stop(paste("the following nodes in 'from' do not exist:",
                   paste(from[!(from %in% nodes)], collapse=', ')))
    
    if (length(to) != 0 && !all(to %in% nodes))
        stop(paste("the following nodes in 'to' do not exist:",
                   paste(to[!(to %in% nodes)], collapse=', ')))

    graph <- list(nodes=nodes, from=from, to=to, isDirected=isDirected)
    
    class(graph) <- "loongraph"
    
    return(graph)
}


#' @title Convert a graph object to a loongraph object
#'   
#' @description Sometimes it is simpler to work with objects of class loongraph
#'   than to work with object of class graph.
#' 
#' @param graph object of class graph (defined in the graph library)
#' 
#' 
#' @details See 
#'   \url{http://www.bioconductor.org/packages/release/bioc/html/graph.html} for
#'   more information about the graph R package.
#'   
#' @templateVar page learn_R_display_graph.html
#' @templateVar section graph-utilities
#' @template see_l_help
#'   
#'   
#' @template return_loongraph 
#' 
#' 
#' @export
#'   
#' @examples 
#' if (requireNamespace("graph", quietly = TRUE)) {
#'   graph_graph  = graph::randomEGraph(LETTERS[1:15], edges=100)
#'   loon_graph <- as.loongraph(graph_graph)
#' }
as.loongraph <- function(graph) {
    if (!is(graph, "graph")) stop("graph argument is not of class graph.")
    requireNamespace("graph", quietly = TRUE) || stop("graph package is required for this function.")
    
    nodes <- graph::nodes(graph)
    ft <- graph::edgeMatrix(graph)
    
    loongraph(nodes=nodes, from=nodes[ft[1,]], to=nodes[ft[2,]],
              isDirected=graph::isDirected(graph))    
}


#' @title Convert a loongraph object to an object of class graph
#'   
#' @description Loon's native graph class is fairly basic. The graph package (on
#'   bioconductor) provides a more powerful alternative to create and work with 
#'   graphs. Also, many other graph theoretic algorithms such as the complement 
#'   function and some graph layout and visualization methods are implemented 
#'   for the graph objects in the RBGL and Rgraphviz R packages. For more 
#'   information on packages that are useful to work with graphs see the
#'   \emph{gRaphical Models in R} CRAN Task View at
#'   \url{https://CRAN.R-project.org/view=gR}.
#'   
#' @param loongraph object of class loongraph
#'   
#' @details See 
#'   \url{http://www.bioconductor.org/packages/release/bioc/html/graph.html} for
#'   more information about the graph R package.
#'   
#' @template return_loongraph
#'   
#' @export
#' 
#' @examples 
#' if (requireNamespace("graph", quietly = TRUE)) {
#'   g <- loongraph(letters[1:4], letters[1:3], letters[2:4], FALSE)
#'   g1 <- as.graph(g) 
#' }
as.graph <- function(loongraph) {
    if (!is(loongraph, "loongraph")) {
        stop("loongraph argument is not of class loongraph.")
    }
    
    if (!requireNamespace("graph", quietly = TRUE)) {
        stop("graph package needed for this function to work. Please install it from bioconductor.",
             call. = FALSE)
    }

    n <- length(loongraph$nodes)
    edL <- vector("list", length = n)
    names(edL) <- loongraph$nodes

    if(loongraph$isDirected) {
        for(i in 1:n) {
            edL[[i]] <- loongraph$to[loongraph$from==loongraph$nodes[i]]
        }
    } else {
        for(i in 1:n) {
            
            edL[[i]] <- unique(c(loongraph$to[loongraph$from==loongraph$nodes[i]],
                          loongraph$from[loongraph$to==loongraph$nodes[i]]))
        }
    }

    new("graphNEL", nodes = loongraph$nodes,
        edgeL=edL,
        edgemode= ifelse(loongraph$isDirected,"directed","undirected"))
    
}


#' @title Plot a loon graph object with base R graphics
#'   
#' @description This function converts the loongraph object to one of class 
#'   graph and the plots it with its respective plot method.
#'   
#' @param x object of class loongraph
#' @param ... arguments forwarded to method
#' 
#' @export
#' 
#' @examples 
#' if (requireNamespace("Rgraphviz", quietly = TRUE)) {
#'   g <- loongraph(letters[1:4], letters[1:3], letters[2:4], FALSE)
#'   plot(g)
#' }
plot.loongraph <- function(x, ...) {
    
    requireNamespace("Rgraphviz", quietly = TRUE) || stop("Rgraphviz library required")
    
    Rgraphviz::plot(as.graph(x), ...)
}


#' @title Create a complete graph or digraph with a set of nodes
#'   
#' @description From Wikipedia: "a complete graph is a simple undirected graph 
#'   in which every pair of distinct vertices is connected by a unique edge. A 
#'   complete digraph is a directed graph in which every pair of distinct 
#'   vertices is connected by a pair of unique edges (one in each direction
#'   
#' @inheritParams loongraph
#' @param isDirected a boolean scalar to indicate wheter the returned object is 
#'   a complete graph (undirected) or a complete digraph (directed).
#'   
#'         
#' @details Note that this function masks the completegraph function of the
#'   graph package. Hence it is a good idead to specify the package namespace
#'   with ::, i.e. loon::completegraph and graph::completegraph.
#'   
#' @templateVar page learn_R_display_graph.html
#' @templateVar section graph-utilities
#' @template see_l_help
#'   
#' @template return_loongraph
#' 
#' @export
#'   
#' @examples 
#' g <- loon::completegraph(letters[1:5])
completegraph <- function(nodes, isDirected=FALSE) {

    n <- length(nodes)
    if (n < 2) {
        stop("number of nodes must be > 2.")
    }
    if (isDirected) {
        co <- expand.grid(nodes, nodes, stringsAsFactors=FALSE)[-seq(1, n*n, by=n+1),]
        from <- as.vector(co[,1])
        to <- co[,2]
    } else {
        co <- utils::combn(nodes, 2)
        from <- co[1,]
        to <- co[2,]
    }
    return(loongraph(nodes=nodes, from=from, to=to, isDirected=isDirected))

}


#' @title Create a linegraph
#' 
#' @description The line graph of G, here denoted L(G), is the graph whose nodes 
#'   correspond to the edges of G and whose edges correspond to nodes of G such 
#'   that nodes of L(G) are joined if and only if the corresponding edges of G 
#'   are adjacent in G.
#'   
#' @param x graph of class graph or loongraph
#' @param ... arguments passed on to method
#'   
#' @return graph object
#'   
#' @export
linegraph <- function(x, ...) {
    UseMethod("linegraph")
}


#' @title Create a linegraph of a graph
#'   
#' @description Create a lingraph of a loongraph
#'   
#' @param x loongraph object
#' @param separator one character - node names in x get concatenated with this 
#'   character
#' @template param_dots_method_not_used
#'   
#' @details linegraph.loongraph needs the code part for directed graphs (i.e.
#'   isDirected=TRUE)
#'   
#' @template return_loongraph
#'   
#' @export
#' 
#' @examples 
#' g <- loongraph(letters[1:4], letters[1:3], letters[2:4], FALSE)
#' 
#' linegraph(g)
linegraph.loongraph <- function(x, separator=":", ...) {
    nodes <- x$nodes
    from <- x$from
    to <- x$to
    
    n <- length(nodes) 
    p <- length(from)
    
    if (x$isDirected) {

        stop("not implemented for directed graphs yet.")
       
#        foreach nfrom1 $from nto1 $to {
#            foreach nfrom2 $from nto2 $to {
#                if {!($nfrom1 eq $nfrom2 && $nto1 eq $nto2)} {
#                    ## Do they share a node?
#                    if {$nfrom1 eq $nfrom2 || $nfrom1 eq $nto2 ||\
#                        $nto1 eq $nfrom2 || $nto1 eq $nto2} {
#			    lappend newfrom [format "%s%s%s"\
#                                             $nfrom1 $separator $nto1]
#			    lappend newto [format "%s%s%s"\
#                                           $nfrom2 $separator $nto2]
#			}
#                }
#            }
#        }

        
    } else {

        newfrom <- character(0)
        newto <- character(0)
        
        for (i in 1:p) {
            for (j in i:p) {
                if (i!=j && !(from[i] == from[j] && to[i] == to[j])) {
                    ## Do they share a node?
                    if (from[i]==from[j] || from[i]==to[j] || to[i]==from[j] || to[i]==to[j]) {
                       # cat(paste0('linegraph --   i: ', i,', j: ',j, '\n'))
                        newfrom <- c(newfrom, paste(from[i], separator, to[i], sep=''))
                        newto <- c(newto, paste(from[j], separator, to[j], sep=''))
                    }
                }
            }
        }
    }
    
    newnodes <- unique(c(newfrom, newto))

    G <- loongraph(nodes=newnodes, from=newfrom,
                   to=newto, isDirected=x$isDirected)
    
    attr(G, "separator") <- separator
    
    return(G)
}

#' @title Create the Complement Graph of a Graph
#' 
#' @description Creates a complement graph of a graph
#' 
#' @param x graph or loongraph object
#' 
#' @return graph object
#' 
#' @export
complement <- function(x) {
    UseMethod("complement", x)
}


#' @title Create the Complement Graph of a loon Graph
#'   
#' @description Creates a complement graph of a graph
#'   
#' @details This method is currently only implemented for undirected graphs.
#'   
#' @param x loongraph object
#'   
#' @template return_loongraph
#'   
#' @export
complement.loongraph <- function(x) {
    nodes <- x$nodes
    from <- x$from
    to <- x$to
    n <- length(nodes)

    newfrom <- character(0)
    newto <- character(0)
    
    if (x$isDirected) {
        
        stop("not implemented for directed graphs yet.")

    } else {
        if(n > 0) {
            for (i in 1:n) {
                for (j in i:n) {
                    if(i!=j && !(any(from==nodes[i] & to==nodes[j]) ||
                                     any(from==nodes[j] & to==nodes[i]))) {
                       # cat(paste0('complement --   i:', i,', j:',j, '\n'))
                        newfrom <- c(newfrom, nodes[i])
                        newto <- c(newto, nodes[j])
                    }
                }
            }
        }
    }

    
    
    G <- loongraph(nodes=nodes, from=newfrom, to=newto,
                   isDirected=x$isDirected)
    
    attributes(G) <- attributes(x)
    return(G)
}


## Theoretically if graph would be a dependency
# setOldClass("loongraph")
# setGeneric("complement", function(x) {standardGeneric("complement")})
# setMethod("complement", "loongraph", loon:::complement.loongraph)


# Create a graph product
# 
# TODO: graphproduct is incomplete
graphproduct <- function(U,V, type=c("product", "tensor", "strong"), separator=':') {

    stop("not implemented yet.")
    
    type <- match.arg(type)

    switch(type,
           product = {
               NULL
           },
           tensor = {
               NULL
           },
           strong = {
               NULL
           })
}



#' @title Make each space in a node apprear only once
#' 
#' @description Reduce a graph to have unique node names
#' 
#' @details Note this is a string based operation. Node names must not contain
#'   the separator character!
#'   
#' @param graph graph of class loongraph
#' @template graph_separator
#' 
#' @template return_loongraph
#'       
#' @export
#' 
#' @examples
#' G <- completegraph(nodes=LETTERS[1:4])
#' LG <- linegraph(G)
#' 
#' LLG <- linegraph(LG)
#' 
#' graphreduce(LLG)
#' 
#' if (requireNamespace("Rgraphviz", quietly = TRUE)) {
#'   plot(graphreduce(LLG))
#' }
#' 
graphreduce <- function(graph, separator) {

    if(!is(graph, 'loongraph'))
        stop('graphreduce is only implemented for objects of class loongraph')
    
    if (graph$isDirected == TRUE)
        stop("graphreduce is not implemented for directed graphs")
    
    
    if (missing(separator)) {
        if (!is.null(attr(graph, "separator"))) {
            separator <- attr(graph, "separator")
        } else {
            stop('separator not known')
        }
    }
    
    
    nodes_reduce <- function(nodes) {
        nodes_vars <- lapply(strsplit(nodes, separator, fixed = TRUE), unique)
        unlist(lapply(nodes_vars, function(vars)paste(sort(vars), collapse = separator)))
    }
    
    ## quick way to get to result of removing duplicate edges
    
    from <- nodes_reduce(graph$from)
    to <- nodes_reduce(graph$to)
    
    sep_nodes <- paste0(separator, separator)
    if(any(grepl(sep_nodes, from)) || any(grepl(sep_nodes, to)))
       stop(paste0('nodes can not contain the string (twice the separator):', sep_nodes ))
    
    edges <- unique(apply(cbind(from,to), 1, function(nodes)paste(sort(nodes), collapse = sep_nodes)))
    
    edges_split <- strsplit(edges, sep_nodes, fixed = TRUE)
    
    new_from <- vapply(edges_split, function(x)x[1], FUN.VALUE = character(1))
    new_to <- vapply(edges_split, function(x)x[2], FUN.VALUE = character(1))
    
    
    loongraph(nodes = unique(nodes_reduce(graph$nodes)),
              from = new_from,
              to = new_to,
              isDirected = FALSE)
}


#' @title Create a n-d transition graph
#' 
#' @description A n-d transition graph has k-d nodes and all edges that connect
#'   two nodes that from a n-d subspace
#' 
#' @param nodes node names of graph
#' @param n integer, dimension an edge should represent
#' @param separator character that separates spaces in node names
#' 
#' @templateVar page learn_R_display_graph.html
#' @templateVar section graph-utilities
#' @template see_l_help
#' @details 
#'   
#' 
#' @template return_loongraph
#' 
#' @export
#' 
#' @examples 
#' g <- ndtransitiongraph(nodes=c('A:B', 'A:F', 'B:C', 'B:F'), n=3, separator=':')
ndtransitiongraph <- function(nodes, n, separator=":") {

    nnodes <- length(nodes)

    nodes_vars <- strsplit(nodes, separator, fixed = TRUE)

    from <- character(0)
    to <- character(0)

    if (nnodes > 0) {
        for (i in 1:nnodes) {
            for (j in i:nnodes) {
                if (i!=j && length(unique(unlist(nodes_vars[c(i,j)]))) %in% n) {
                    from <- c(from, nodes[[i]])
                    to <- c(to, nodes[[j]])
                }
            }
        }
    }

    loongraph(nodes, from, to, isDirected=FALSE)
}
