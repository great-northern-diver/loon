#' @title Creates a loon plot for each facet from an existing loon plot.
#'
#' @description
#' A generic function used by \code{l_facet} when facetting an existing loon plot.
#'
#' @param widget the loon widget
#' @param ...  other arguments to the function used to create
#' the loon plot for each facet.  Depending on the plot being facetted, these
#' include the arguments \code{by},  \code{on}, \code{parent},  \code{layout},
#' \code{inheritLayers}, \code{separate}, and \code{bySubstitute}.
#' See \code{\link{l_facet}} for details on these parameters.
#' Other loon plot constructor parameters can be passed as well this way.
#'
#' @return A list containing the named components \code{plots}, \code{subtitles},
#' \code{child = child}, and \code{new.toplevel} containing the facets as plots
#' and other relevant information to construct the facetted plot.
#'
#' @seealso \code{\link{l_facet}}
#'
#' @export
#' @keywords internal
get_facets <- function(widget, ...) {
    UseMethod("get_facets")
}

#' @export
#' @keywords internal
get_facets.loon <- function(widget, by, on,
                            parent = NULL,
                            layout = "grid",
                            linkingGroup, inheritLayers = TRUE, separate = FALSE,
                            bySubstitute, ...) {

    nDimStates <- l_nDimStateNames(widget)
    states <- names(l_info_states(widget))

    data <- setNames(
        object = as.data.frame(
            lapply(nDimStates,
                   function(state) {
                       s <- widget[state]
                       if(length(s) == 0) s <- NA
                       s
                   }),
            stringsAsFactors = FALSE
        ),  nm = nDimStates
    )
    N <- dim(data)[1]
    data <- cbind(index = 1:N, data)

    # remove columns which involves NAs
    column_names <- names(data)
    lapply(nDimStates,
           function(state) {
               if(any(is.na(data[[state]]))) {
                   data[[state]] <<- NULL
                   column_names <<- column_names[-which(column_names == state)]
               } else {
                   if(length(unique(data[[state]])) == 1)
                       column_names <<- column_names[-which(column_names == state)]
               }
           })

    splited <- splitFun(widget = widget,
                        data = data,
                        layout = layout,
                        by = by,
                        on = on,
                        column_names = column_names,
                        bySubstitute = bySubstitute)
    split_data <- splited$split_data
    subtitles <- splited$subtitles

    if(length(split_data) == 1) return(widget)

    # linkingGroup
    if(missing(linkingGroup)) {
        linkingGroup <- widget['linkingGroup']
        if(linkingGroup == "none")
            linkingGroup <- paste0("facet", valid_path())
        message(paste("linkingGroup:", linkingGroup))
    }

    # define functions
    loonFun <- function(widget) {
        if(inherits(widget, "l_plot3D"))
            return(l_plot3D)
        else if(inherits(widget, "l_hist"))
            return(l_hist)
        else if(inherits(widget, "l_serialaxes"))
            return(l_serialaxes)
        else
            return(l_plot)
    }

    # All children will inherit states from parent widget
    inheritStates <- setdiff(states, c(nDimStates, "zoomX", "zoomY", "panX", "panY",
                                       "deltaX", "deltaY", "parent", "linkingGroup"))

    # set parent
    new.toplevel <- FALSE
    if(separate) {
        child <- parent
    } else {
        if(is.null(parent)) {
            new.toplevel <- TRUE
            # create parent
            parent <- l_toplevel()
        }

        subwin <- l_subwin(parent, 'facet')
        by_names <- names(subtitles)
        tktitle(parent) <- if(!is.null(by_names))
            paste("loon layouts on",
                  deparse(substitute(by_names)), "--path:", subwin)
        else
            paste("loon layouts on",
                  deparse(bySubstitute), "--path:", subwin)

        # create child
        child <- as.character(tcl('frame', subwin))
    }

    inheritArgs <- setNames(
        lapply(inheritStates,
               function(s) {
                   if(is.list(widget[s])) return(NULL)
                   if(separate) widget[s]
                   else {
                       if(s %in% c("xlabel", "ylabel", "zlabel", "title"))
                           ""
                       else if(s == "minimumMargins")
                           rep(5, 4)
                       else
                           widget[s]
                   }
               }),
        inheritStates
    )

    inheritArgs <- Filter(Negate(is.null), inheritArgs)

    # build loon plot
    if(inheritLayers)
        l_children_layers <- lapply(rev(setdiff(l_layer_getChildren(widget), "model")),
                                    function(layerid) {
                                        if(length(layerid) > 0)
                                            l_create_handle(c(widget, layerid))
                                    })

    displayed_glyph_index <- function(widget, glyph) {
        if(inherits(widget, "l_plot")) {
            valid_glyph <- c("ccircle", "ctriangle", "csquare",
                             "ocircle", "otriangle", "osquare",
                             "circle", "triangle", "square")
            which(!glyph %in% valid_glyph)
        } else numeric(0)
    }

    plots <- setNames(
        lapply(split_data,
               function(d) {
                   if(dim(d)[1] == 0) {

                       args <- c(
                           list(...),
                           inheritArgs ,
                           parent = child
                       )
                       # remove duplicated names
                       args <- args[which(!duplicated(names(args)))]
                       return(
                           do.call(
                               loonFun(widget),
                               args
                           )
                       )
                   }
                   args <- as.list(d)
                   glyph <- args$glyph
                   index <- args$index
                   args$index <- NULL
                   # l_plot or l_plot3D
                   glyph_index <- displayed_glyph_index(widget, glyph)
                   if(length(glyph_index) > 0) {
                       # default glyph
                       args$glyph[glyph_index] <- "ccircle"
                   }

                   args$linkingGroup <- linkingGroup
                   args$parent <- child

                   args <- c(
                       list(...),
                       args,
                       inheritArgs
                   )
                   # remove duplicated names
                   args <- args[which(!duplicated(names(args)))]
                   p <- do.call(
                       loonFun(widget),
                       args
                   )
                   if(inheritLayers) {
                       if(length(l_children_layers) > 0) {
                           lapply(l_children_layers,
                                  function(layer) {
                                      l_copyLayers(p, widget, layer)
                                  }
                           )
                       }
                   }

                   draw_displayed_glyph(p, widget, glyph, glyph_index, index, N)
                   draw_hidden_glyph(p, widget, glyph, index, N)

                   p
               }),
        names(split_data)
    )

    list(
        plots = plots,
        subtitles = subtitles,
        child = child,
        new.toplevel = new.toplevel
    )
}

#' @export
#' @keywords internal
get_facets.l_serialaxes <- function(widget, by, parent = NULL,
                                    layout = "grid", linkingGroup,
                                    inheritLayers = TRUE, separate = FALSE,
                                    bySubstitute, ...) {

    nDimStates <- setdiff(l_nDimStateNames(widget), "data")
    states <- names(l_info_states(widget))

    data <- setNames(
        object = as.data.frame(
            lapply(nDimStates,
                   function(state) {
                       s <- widget[state]
                       if(length(s) == 0) s <- NA
                       s
                   }),
            stringsAsFactors = FALSE
        ),  nm = nDimStates
    )
    N <- dim(data)[1]
    data <- cbind(index = 1:N, data)

    serialaxesData <- char2num.data.frame(widget["data"])

    # remove columns which involves NAs
    column_names <- names(data)
    lapply(nDimStates,
           function(state) {
               if(any(is.na(data[[state]]))) {
                   data[[state]] <<- NULL
                   column_names <<- column_names[-which(column_names == state)]
               } else {
                   if(length(unique(data[[state]])) == 1)
                       column_names <<- column_names[-which(column_names == state)]
               }
           })

    # TODO by is a formula
    splited <- splitFun(widget = widget,
                        data = data,
                        layout = layout,
                        by = by,
                        column_names = column_names,
                        bySubstitute = bySubstitute)
    split_data <- splited$split_data
    subtitles <- splited$subtitles

    if(length(split_data) == 1) return(widget)

    new.toplevel <- FALSE
    if(separate) {
        child <- parent
    } else {
        # set parent
        if(is.null(parent)) {
            new.toplevel <- TRUE
            # create parent
            parent <- l_toplevel()
        }

        subwin <- l_subwin(parent, 'facet')
        by_names <- names(subtitles)
        tktitle(parent) <- if(!is.null(by_names))
            paste("loon layouts on",
                  deparse(substitute(by_names)), "--path:", subwin)
        else
            paste("loon layouts on",
                  deparse(bySubstitute), "--path:", subwin)

        # create child
        child <- as.character(tcl('frame', subwin))
    }

    # linkingGroup
    if(missing(linkingGroup)) {
        linkingGroup <- widget['linkingGroup']
        if(linkingGroup == "none")
            linkingGroup <- paste0("facet", valid_path())
        message(paste("linkingGroup:", linkingGroup))
    }

    # All children will inherit states from parent widget
    inheritStates <- setdiff(states, c(nDimStates, "data", "parent", "linkingGroup"))
    inheritArgs <- setNames(
        lapply(inheritStates,
               function(s) {
                   widget[s]
               }),
        inheritStates
    )

    # build loon plot
    plots <- setNames(
        lapply(split_data,
               function(d) {
                   if(dim(d)[1] == 0)
                       return(
                           l_plot(parent = child,
                                  showScales = FALSE,
                                  showLabels = FALSE,
                                  guidelines = l_getOption('guidesBackground'))
                       ) #fill the place
                   args <- as.list(d)
                   index <- args$index
                   args$index <- NULL
                   args$data <- serialaxesData[index, ]
                   args$linkingGroup <- linkingGroup
                   args$parent <- child
                   do.call(
                       l_serialaxes,
                       c(
                           args,
                           inheritArgs
                       )
                   )
               }),
        names(split_data)
    )

    list(
        plots = plots,
        subtitles = subtitles,
        child = child,
        new.toplevel = new.toplevel
    )
}

splitFun <- function(widget, data, layout = "grid", by, on,
                     column_names = NULL,
                     bySubstitute, sep = "*",
                     N = nrow(data)) {

    byDataFrame <- by2Data(by, on,
                           bySubstitute = bySubstitute,
                           n = N, args = data,
                           l_className = class(widget)[1L])

    if(length(byDataFrame) == 0)
        return(
            list(
                subtitles = NULL,
                split_data = list(data)
            )
        )

    byNames <- colnames(byDataFrame)

    subtitles <- setNames(lapply(byDataFrame,
                                 function(b)
                                     as.character(levels(factor(b)))),
                          byNames)

    if(layout == "grid") lex.order <- FALSE else lex.order <- TRUE
    split_data <- split(data,
                        f = as.list(byDataFrame),
                        drop = FALSE,
                        lex.order = lex.order,
                        sep = sep)

    list(
        subtitles = subtitles,
        split_data = split_data
    )
}

glyph4child <- function(widget, glyph, index, N) {

    glyphStateNames <- names(glyph)

    states <- setNames(
        lapply(glyphStateNames,
               function(g) {
                   if(g == "data")
                       char2num.data.frame(glyph[g][index, ])
                   else {
                       if(length(glyph[g]) == N)
                           glyph[g][index]
                       else
                           glyph[g]
                   }

               }),
        glyphStateNames
    )

    args <- c(widget = widget, states)
    switch(class(glyph)[1],
           "l_glyph_serialaxes" = do.call(l_glyph_add_serialaxes, args),
           "l_glyph_image" = do.call(l_glyph_add_image, args),
           "l_glyph_polygon" = do.call(l_glyph_add_polygon, args),
           "l_glyph_pointrange" = do.call(l_glyph_add_pointrange, args),
           "l_glyph_text" = do.call(l_glyph_add_text, args)
    )
}

draw_displayed_glyph <- function(p, widget, glyph, glyph_index, index, N) {

    if(length(glyph_index) > 0) {

        unique_glyph <- unique(glyph[glyph_index])
        lapply(unique_glyph,
               function(g) {
                   gh_child <- glyph4child(widget = p,
                                           glyph = l_create_handle(c(widget, g)),
                                           index = index,
                                           N = N)
                   p['glyph'][which(glyph == unique_glyph)] <- gh_child
               })
    }
}

draw_hidden_glyph <- function(p, widget, glyph, index, N) {

    if(inherits(p, "l_plot")) {

        all_glyph <- l_glyph_ids(widget)

        if(length(all_glyph) > 0) {

            unique_glyph <- unique(glyph)
            hidden_glyph <- all_glyph[!all_glyph %in% unique_glyph]

            if(length(hidden_glyph) > 0) {
                lapply(hidden_glyph,
                       function(g) {
                           glyph4child(widget = p,
                                       glyph = l_create_handle(c(widget, g)),
                                       index = index,
                                       N = N)
                       })
            }
        }
    }
}


# copy layer from loon 'widget' to loon 'p'
l_copyLayers <- function(p, widget, layer, parent = "root") {

    # input is a char
    if(!inherits(layer, "loon")) layer <- l_create_handle(c(widget, layer))

    layerType <- class(layer)[1]

    isVisible <- l_layer_isVisible(widget, layer)


    if(layerType == "l_layer_group") {

        group <- l_layer_group(p, parent = parent)
        if(!isVisible) l_layer_hide(p, group)

        children <- l_layer_getChildren(widget, layer)

        if(length(children) == 0) return(invisible())

        lapply(children,
               function(child) {
                   l_copyLayers(p, widget, child,
                                parent = group)
               })

    } else {
        layer_name <- names(l_info_states(layer))
        l <- do.call(
            what = eval(parse(text = layerType)),
            args = c(list(widget = p,
                          parent = parent),
                     setNames(
                         lapply(layer_name,
                                function(l) layer[l]),
                         layer_name
                     )
            )
        )
        if(!isVisible) l_layer_hide(p, l)
    }
}
