
#' Create an plot with a map layered
#' @export l_plot.map
#' @examples 
#' p <- l_plot(map('world', fill=TRUE, plot=FALSE))
l_plot.map <-  function(x, ...) {

    p <- l_plot.default()
    l_layer.map(p, map=x, label="Map",  ...)
    l_scaleto_world(p)
    p
}    

#' @title Add a Map of class map as Drawings to Loon plot
#'   
#'   
#' @description The maps library provides some map data in polygon which can be 
#'   added as drawings to Loon plots. This function adds map objects with class
#'   map from the maps library as background drawings.
#'   
#' @export l_layer.map
#'   
#' @examples 
#' 
#' canada <- map("world",  "Canada", fill=TRUE, plot=FALSE)
#' p <- l_plot()
#' l_map <- l_layer(p, canada, asSingleLayer=TRUE)
#' l_map['color'] <- ifelse(grepl("lake", canada$names, TRUE), "lightblue", "")
#' l_scaleto_layer(p, l_map)
#' l_map['active'] <- FALSE
#' l_map['active'] <- TRUE
#' l_map['tag']
l_layer.map <- function(widget, map,
                        color="", linecolor="black", linewidth=1,
                        label, parent="root", index=0, asSingleLayer=TRUE) {

    l_throwErrorIfNotLoonWidget(widget)

    if(!is(map,"map")) {
        stop("map is not an map object from the maps library.")
    }
    
    if (all(is.na(map$x) != is.na(map$y))) {
        stop("NA's not at same indices in x and y")
    }

    if (missing(label)) {
        label <- deparse(substitute(map))   
    }
    
    is.color <- function(x) {
        sapply(x, function(X) {
            tryCatch(is.matrix(col2rgb(X)), 
                     error = function(e) FALSE)
        })
    }
    
    if(!all(color=="" || is.color(color))) {
        stop("color needs to be a color or \"\"")
    }
    if(!all(linecolor=="" || is.color(linecolor))) {
        stop("linecolor needs to be a color or \"\"")
    }
    if(!all(linewidth>=0)) {
        stop("linewidth needs to be numeric and >=0")
    }

    pos <-  which(is.na(map$x))
    npolygons <- length(pos) + 1
    
    id <- NA
    
    if (npolygons == 1) {
        id <- l_layer_polygon(widget, x=map$x, y=map$y,
                               color=color[1],
                               linecolor=linecolor[1],
                               linewidth=linewidth[1],
                               label=label)
    } else {
        if (length(color) == 1) {
            color <- rep(color, npolygons)
        } else if (length(color) != 1 && length(color) != npolygons) {
            warning("only first color element used as length not equal to number of polygons")
            color <- rep(color[1], npolygons)
        }
        if (length(linecolor) == 1) {
            linecolor <- rep(linecolor, npolygons)
        } else if (length(linecolor) != 1 && length(linecolor) != npolygons) {
            warning("only first linecolor element used as length not equal to number of polygons")
            linecolor <- rep(linecolor[1], npolygons)
        }
        if (length(linewidth) == 1) {
            linewidth <- rep(linewidth, npolygons)
        } else if (length(linewidth) != 1 && length(linewidth) != npolygons) {
            warning("only first linewidth element used as length not equal to number of polygons")
            linewidth <- rep(linewidth[1], npolygons)
        }

        
        newPos <- c(0, pos, length(map$x)+1)
        ids <- rep(NA, npolygons)
        
        x <- map$x
        y <- map$y

        if(asSingleLayer) {
            ## use a polygons layer
            xlist <- vector(mode="list", length = npolygons)
            ylist <- vector(mode="list", length = npolygons)
             
            for(i in 1:npolygons) {
                sel <- seq(newPos[i]+1, newPos[i+1]-1, by=1)
                xlist[[i]] <- x[sel]
                ylist[[i]] <- y[sel]
            }
            
            id <- l_layer_polygons(widget, xlist, ylist,
                            color=color,
                            linecolor=linecolor,
                            linewidth=linewidth,
                            tag=map$names,
                            label=label,
                            parent=parent,
                            index='end')
        } else {
            ## use a group with polygon layers
            id <- l_layer_group(widget, label=label, parent=parent, index=index)
            for (i in 1:npolygons) {
                sel <- seq(newPos[i]+1, newPos[i+1]-1, by=1)
                
                l_layer_polygon(widget, x[sel], y[sel],
                                color=color[i],
                                linecolor=linecolor[i],
                                linewidth=linewidth[i],
                                label=map$names[i],
                                parent=id,
                                index='end')
            }
        }
    }
    return(id)    
}
