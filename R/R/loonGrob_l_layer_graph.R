
#' @rdname loonGrob
#'
#' @examples
#'
#' \dontrun{
#' ## graph examples
#'
#' G <- completegraph(names(iris[,-5]))
#' LG <- linegraph(G)
#' g <- l_graph(LG)
#'
#' nav0 <- l_navigator_add(g)
#' l_configure(nav0, label = 0)
#' con0 <- l_context_add_geodesic2d(navigator=nav0, data=iris[,-5])
#'
#' nav1 <- l_navigator_add(g, from = "Sepal.Length:Petal.Width",
#'   to = "Petal.Length:Petal.Width", proportion = 0.6)
#' l_configure(nav1, label = 1)
#' con1 <- l_context_add_geodesic2d(navigator=nav1, data=iris[,-5])
#'
#' nav2 <- l_navigator_add(g, from = "Sepal.Length:Petal.Length",
#'   to = "Sepal.Width:Petal.Length", proportion = 0.5)
#' l_configure(nav2, label = 2)
#' con2 <- l_context_add_geodesic2d(navigator=nav2, data=iris[,-5])
#'
#' # To print directly use either
#' plot(g)
#' # or
#' grid.loon(g)
#' # or to save structure
#' library(grid)
#' lgrob <- loonGrob(g)
#' grid.newpage(); grid.draw(lgrob)
#' }
#'
#' @export

loonGrob.l_layer_graph  <- function(target, name = NULL, gp = NULL, vp = NULL) {

    widget <- l_create_handle(attr(target, "widget"))
    states <- get_layer_states(widget)

    active <- states$active
    if (!any(active)) {
        grob(name = if (is.null(name)) "graph" else name, gp = gp, vp = vp)
    } else {

        edgesGrob <- edgesGrob(states)
        nodeGlyphGrob <- nodeGlyphGrob(states)
        labelGrob <- labelGrob(states)
        # add navigators
        nav_ids <- l_navigator_ids(widget)

        if(length(nav_ids) == 0){
            # No navigator, just return the graph
            gTree(children =
                      gList(
                          edgesGrob,
                          nodeGlyphGrob,
                          labelGrob),
                  name = if (is.null(name)) "graph" else name,
                  gp = gp, vp = vp
            )
        } else {
            # have navigator, need path and navigator as well
            activeNavigator <- widget["activeNavigator"]
            gTree(
                children = gList(
                    edgesGrob,
                    do.call(gList,
                            lapply(nav_ids,
                                   function(nav_id){
                                       navPathGrob(states,
                                                   navigator = l_create_handle(c(widget, nav_id)),
                                                   name = paste0("navigation path edges", nav_id))
                                   })
                    ),
                    nodeGlyphGrob,
                    labelGrob,
                    do.call(gList,
                            lapply(nav_ids,
                                   function(nav_id){
                                       navPointsGrob(activeNavigator,
                                                     states,
                                                     navigator = l_create_handle(c(widget, nav_id)),
                                                     name = paste0("navigation points edges", nav_id))
                                   })
                    )
                ),
                name = if (is.null(name)) "graph" else name, gp = gp, vp = vp
            )
        }
    }
}

edgesGrob <- function(states = NULL, name = NULL){
    active <- states$active
    activeNode <- states$nodes[active]
    activeX <- states$x[active]
    activeY <- states$y[active]
    isActiveEdge <- states$activeEdge
    gTree(children =
              do.call(
                  gList,
                  lapply(seq_len(length(activeNode)),
                         function(i) {
                             nodeFrom <- activeNode[i]
                             nodeFrom_EdgeId <- which(states$from[isActiveEdge] == nodeFrom)

                             if (length(nodeFrom_EdgeId) != 0){
                                 nodeTo <- states$to[isActiveEdge][nodeFrom_EdgeId]
                                 nodeTo_CoordId <- which (activeNode %in% nodeTo == TRUE)
                                 numNodesTo <- length(nodeTo_CoordId)
                                 cols <- states$colorEdge[isActiveEdge][nodeFrom_EdgeId]
                                 x <- unit(c(rep(activeX[i], numNodesTo),
                                             activeX[nodeTo_CoordId]),
                                           "native")
                                 y <- unit(c(rep(activeY[i], numNodesTo),
                                             activeY[nodeTo_CoordId]),
                                           "native")
                                 polylineGrob(x, y,
                                              id = rep(nodeTo_CoordId, 2),
                                              gp = gpar(col= cols, lwd=1),
                                              name = paste("edge", i))
                             } else {
                                 condGrob(test = FALSE,
                                          grobFun = polylineGrob,
                                          name = paste("edge", i, "missing")
                                 )

                             }
                         }
                  )

              ),
          name = if (is.null(name)) "graph edges" else name
    )
}

labelGrob <- function(states = NULL, name = NULL){

    active <- states$active
    activeNode <- states$nodes[active]
    activeX <- states$x[active]
    activeY <- states$y[active]
    activeAngle <- states$orbitAngle[active]
    orbitDistance <- states$orbitDistance

    gTree(children = do.call(
        gList,
        lapply(seq_len(length(activeNode)),
               function(i) {
                   condGrob(test = states$showOrbit,
                            grobFun = textGrob,
                            name = paste("label", i),
                            label = activeNode[i],
                            x = unit(activeX[i], "native") +
                                unit(orbitDistance * cos(activeAngle[i]),
                                     "mm" ),
                            y = unit(activeY[i], "native") +
                                unit(orbitDistance * sin(activeAngle[i]),
                                     "mm" ),
                            gp=gpar(fontsize= 8, # TODO find this somewhere
                                    col= l_getOption("foreground")))
               }
        )
    ),
    name = if (is.null(name)) "graph labels" else name
    )
}

nodeGlyphGrob <- function(states = NULL, name = NULL){

    active <- states$active

    cex <- as_r_point_size(states$size[active])
    selected <- states$selected[active]
    col <-  get_display_color(states$color[active], selected)
    pch <- glyph_to_pch(states$glyph[active])


    # is there a fill colour?
    filled <- pch %in% 21:24

    activeX <- states$x[active]
    activeY <- states$y[active]

    gTree(
        children = do.call(gList,
                           lapply(seq_len(length(filled)),
                                  function(i){
                                      gp <- if (filled[i]) {
                                          gpar(fill = col[i],
                                               col = l_getOption("foreground"),
                                               cex = cex[i])
                                      } else {
                                          gpar(col = col[i], cex = cex[i])
                                      }
                                      pointsGrob(x = activeX[i],
                                                 y = activeY[i],
                                                 pch = pch[i],
                                                 gp = gp,
                                                 name = paste("node", i)
                                      )
                                  }

                           )),
        name = if (is.null(name)) "graph nodes" else name
    )
}

navPathGrob <- function(states, navigator, name = NULL){
    x <- as.numeric(states$x)
    y <- as.numeric(states$y)
    node <- states$nodes

    color <- as_hex6color(navigator['color'])
    from <- navigator['from']
    to <- navigator['to']
    prop <- navigator['proportion']

    fromId <- sapply(1:length(from), function(i){which(node %in% from[i] == T)})
    toId <- sapply(1:length(to), function(i){which(node %in% to[i] == T)})

    if(length(from) == 0 || length(to) == 0) {
        grob(name = name)
    } else {

        visitedLinesGrob <-
            if(length(from) < 2) {
                grob(name = name)
            } else {
                do.call(gList,
                        lapply(1:(length(from) - 1),
                               function(i){
                                   linesGrob(unit(c(x[fromId[i]], x[fromId[i+1]]),
                                                  "native"),
                                             unit( c(y[fromId[i]], y[fromId[i+1]]),
                                                   "native"),
                                             gp = gpar(col = color,
                                                       lwd = 9), #TODO find the line widths
                                             name = paste("line", i, "(visited)")
                                             )
                               }
                        )
                )
            }

        unvisitedLinesGrob <-
            if(length(to) < 2){
                grob(name = name)
            } else {
                do.call(gList,
                        lapply(1:(length(to) - 1),
                               function(i){
                                   linesGrob(unit( c(x[toId[i]], x[toId[i+1]]), "native"),
                                             unit( c(y[toId[i]], y[toId[i+1]]), "native"),
                                             gp = gpar(col = color,
                                                       lwd = 3), #TODO find the line widths
                                             name = paste("line", i, "(unvisited)")
                                   )
                               }
                        )
                )
            }

        xn <- (1 - prop) * x[fromId[length(fromId)]] + prop * x[toId[1]]
        yn <- (1 - prop) * y[fromId[length(fromId)]] + prop * y[toId[1]]

        betweenLinesGrob <- gList(linesGrob(unit(c(x[fromId[length(fromId)]], xn), "native"),
                                           unit(c(y[fromId[length(fromId)]], yn), "native"),
                                           gp = gpar(col = color, lwd = 9)),  #TODO find the line widths
                                 linesGrob(unit(c(xn, x[toId[1]]), "native"),
                                           unit(c(yn, y[toId[1]]), "native"),
                                           gp = gpar(col = color, lwd = 3)) #TODO find the line widths
        )

        gTree(children =  gList(unvisitedLinesGrob,
                                visitedLinesGrob,
                                betweenLinesGrob),
              name = if (is.null(name)) "navigation path" else name
             )
    }
}

# size of navigator is arbitrary, just as close as loon object.
navPointsGrob <- function(activeNavigator,
                          states,
                          navigator,
                          name){

    x <- as.numeric(states$x)
    y <- as.numeric(states$y)
    node <- states$nodes

    color <- as_hex6color(navigator['color'])
    from <- navigator['from']
    to <- navigator['to']
    prop <- navigator['proportion']
    label <- navigator['label']

    fromId <- sapply(1:length(from), function(i){which(node %in% from[i] == TRUE)})
    toId <- sapply(1:length(to), function(i){which(node %in% to[i] == TRUE)})

    sel_color <- as.character(l_getOption("select-color"))
    if (grepl("^#", sel_color) && nchar(sel_color) == 13) {
        sel_color <- hex12tohex6(sel_color)
    }

    circleGp <- if(length(activeNavigator) != 0) {
        if(activeNavigator == navigator) {
            gpar(fill = color, lwd = 4, col = sel_color) # TODO line width?
        } else {
            gpar(fill = color)
        }
    } else {gpar(fill = color)}

    fromRadius <- unit(5.5, "mm")

    if(length(from) == 0){

        xx <- unit(0.1, "npc")
        yy <- unit(0.9, "npc")

        gTree(children = gList(circleGrob(xx, yy, r = fromRadius,
                                          gp = circleGp,
                                          name = "navigator circle"),
                               if(length(label) != 0) {
                                   textGrob(paste(label, collapse = " "),
                                            xx, yy,
                                            gp = gpar(fill = "black", fontsize = 9),
                                            name = "navigator label") # font size?
                               }
        ),
        name = if (is.null(name)) "navigator" else name
        )

    } else if(length(from) == 1 & length(to) == 0) {

        xx <- unit(x[fromId], "native")
        yy <- unit(y[fromId], "native")

        gTree(children = gList(
            circleGrob(x = xx,
                       y = yy,
                       r = fromRadius,
                       gp = circleGp,
                       name = "navigator circle"
            ),
            if(length(label) != 0) {
                textGrob(paste(label, collapse = " "), xx, yy,
                         gp = gpar(fill = "black", fontsize = 9),
                         name = "navigator label")
            }

        ),
        name = if (is.null(name)) "navigator" else name
        )

    } else {

        xx <- unit( (1 - prop) * x[fromId[length(fromId)]] + prop * x[toId[1]], "native")
        yy <- unit( (1 - prop) * y[fromId[length(fromId)]] + prop * y[toId[1]], "native")

        toRadius <- unit(1, "mm")

        gTree(children = gList(
            # 'to' dot
            circleGrob(unit(x[toId[length(toId)]], "native"),
                       unit(y[toId[length(toId)]], "native"),
                       r = toRadius,
                       gp = gpar(fill = color),
                       name = "to dot"
            ),
            # 'from' navigator
            circleGrob(xx, yy,
                       r = fromRadius,
                       gp = circleGp,
                       name = "navigator circle"
            ),
            # 'text' on the navigator
            condGrob(
                test = length(label) != 0,
                grobFun = textGrob,
                name = "navigator label",
                label = paste(label, collapse = " "),
                x = xx, y = yy,
                gp = gpar(fill = "black", fontsize = 9)
                )
        ),
        name = if (is.null(name)) "navigator" else name
        )
    }
}
