##
## Helper functions to create simple graphs
##

#' @title Create a graph object of class loongraph
#'   
#' @description The loongraph class provides a simple alternative to the graph 
#'   class to create common graphs that are useful for use as navigation graphs.
#'   
#'   
#' @details loongraph objects can be converted to graph objects (i.e. objects of
#'   class graph which is defined in the graph package) with the as.graph 
#'   function.
#'   
#' @export
#' 
#' @param nodes a character vector with node names, each element defines a node
#'   hence the elements need to be unique
#' @param from a character vector with node names, each element defines an edge
#' @param to  a character vector with node names, each element defines an edge
#' @param isDirected boolean scalar, defines whether from and to define directed
#'   edges
#'   
#'   
#' @return An object of class loongraph (S3)
#'   
#' @seealso \code{\link{completegraph}}, \code{\link{linegraph}}, 
#'   \code{\link{complement}}, \code{\link{as.graph}}
#'   
#'   
#' @examples 
#' 
#' g <- loongraph(nodes = c("A", "B", "C", "D"),
#'                from = c("A", "A", "B", "B", "C"),
#'                to   = c("B", "C", "C", "D", "D"))
#' \dontrun {
#' p <- l_graph(g)
#' }
#' 
#' lg <- linegraph(g)
#' 
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
#' @details See 
#'   \url{http://www.bioconductor.org/packages/release/bioc/html/graph.html} for
#'   more information about the graph R package.
#'   
#' @export
#'   
#' @param graph object of class graph (defined in the graph library)
#' 
#' @examples 
#' library(graph)
#' graph_graph  = randomEGraph(LETTERS[1:15], edges=100)
#' 
#' loon_graph <- as.loongraph(graph_graph)
as.loongraph <- function(graph) {
    if (!is(graph, "graph")) {
        stop("graph argument is not of class graph.")
    }
    
    nodes <- nodes(graph)
    ft <- edgeMatrix(graph)
    
    loongraph(nodes=nodes, from=nodes[ft[1,]], to=nodes[ft[2,]],
              isDirected=isDirected(graph))    
}


#' @title Convert a loongraph object to an object of class graph
#'   
#' @description Loon's native graph class is fairly basic. The graph package (on
#'   bioconductor) provides a more powerful alternative to create and work with 
#'   graphs. Also, many other graph theoretic algorithms such as the complement 
#'   function and some graph layout and visualization methods are implemented 
#'   for the graph objects in the RBGL and Rgraphviz R packages.
#'   
#' @details See 
#'   \url{http://www.bioconductor.org/packages/release/bioc/html/graph.html} for
#'   more information about the graph R package.
#'   
#' @export
#' 
#' @param loongraph object of class loongraph
#'   
#' @return object of class graph
#' 
#' @examples 
#' 
#' library(graph)
#' g <- loongraph(letters[1:4], letters[1:3], letters[2:4], FALSE)
#' g1 <- as.graph(g) 
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
#'   
#' @export
#' 
#' @examples 
#' g <- loongraph(letters[1:4], letters[1:3], letters[2:4], FALSE)
#' plot(g)
plot.loongraph <- function(x, ...) {
    plot(as.graph(x), ...)
}


#' @title Create a complete graph or digraph with a set of nodes
#'   
#' @description From Wikipedia: "a complete graph is a simple undirected graph 
#'   in which every pair of distinct vertices is connected by a unique edge. A 
#'   complete digraph is a directed graph in which every pair of distinct 
#'   vertices is connected by a pair of unique edges (one in each direction
#'   
#'   
#' @details Note that this function masks the completegraph function of the
#'   graph package. Hence it is a good idead to specify the package namespace
#'   with ::, i.e. loon::completegraph and graph::completegraph.
#'   
#' @export
#' 
#' @param nodes character vector with unique node names
#' @param isDirected a boolean scalar to indicate wheter the returned object is 
#'   a complete graph (undirected) or a complete digraph (directed).
#'   
#' @return an loongraph object
#'   
#' @examples 
#' 
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
        co <- combn(nodes, 2)
        from <- co[1,]
        to <- co[2,]
    }
    return(loongraph(nodes=nodes, from=from, to=to, isDirected=isDirected))

}


#' @title 
#' 
#' @description 
#' 
#' @export
#' 
#' 
#' @param x 
#' 
#' @return 
#' 
#' @examples 
linegraph <- function(x, ...) {
    UseMethod("linegraph")
}


#' @title Create a linegraph of a graph
#'   
#'   
#'   
#' @details TODO: linegraph.loongraph needs the code part for directed graphs
#' (i.e. isDirected=TRUE)
#' 
#' @export
#' 
linegraph.loongraph <- function(x, separator=":") {
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


#' @export
complement <- function(x, ...) {
    UseMethod("complement")
}

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


#' Create a graph product
#' 
#' 
#' TODO: graphproduct is incomplete
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



#' Make each space in a node apprear only once
#' 
#' @details Note this is a string based operation. Node names must not contain
#'   the separator character!
#'   
#' @export
#' 
#' @examples 
#' 
#' G <- completegraph(nodes=LETTERS[1:4])
#' LG <- linegraph(G)
#' 
#' LLG <- linegraph(LG)
#' 
#' graphreduce(LLG)
#' 
#' library(Rgraphviz)  
#' plot(graphreduce(LLG))
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


#' @export
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
