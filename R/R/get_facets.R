get_facets <- function(widget, ...) {
    UseMethod("get_facets")
}

get_facets.loon <- function(widget, by, parent = NULL,
                            linkingGroup, inheritLayers = TRUE, separate = FALSE,
                            byDeparse = "by", ...) {

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

    # TODO by is a formula
    splited <- splitedFun(data = data,
                          by = by,
                          column_names = column_names,
                          byDeparse = byDeparse)
    splitted_data <- splited$splitted_data
    subtitles <- splited$subtitles
    by <- splited$by

    if(length(splitted_data) == 1) return(widget)

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
    if(separate) {
        child <- parent
    } else {
        if(is.null(parent)) {
            # create parent
            parent <- l_toplevel()
            subwin <- l_subwin(parent, 'facet')

            by_names <- names(subtitles)
            tktitle <- if(!is.null(by_names))
                paste("loon layouts on",
                      deparse(substitute(by_names)), "--path:", subwin)
            else
                paste("loon layouts on",
                      byDeparse, "--path:", subwin)
            tktitle(parent) <- tktitle

            # create child
            child <- as.character(tcl('frame', subwin))
        } else {
            child <- parent
        }
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

    glyphIndex <- function(widget, glyph) {
        if(inherits(widget, "l_plot")) {
            valid_glyph <- c("ccircle", "ctriangle", "csquare",
                             "ocircle", "otriangle", "osquare",
                             "circle", "triangle", "square")
            which(!glyph %in% valid_glyph)
        } else numeric(0)
    }

    plots <- setNames(
        lapply(splitted_data,
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
                   glyph_index <- glyphIndex(widget, glyph)
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
                                      isVisible <- l_layer_isVisible(widget, layer)
                                      layer_major_class <- class(layer)[1]
                                      layer_name <- names(l_info_states(layer))
                                      do.call(
                                          what = eval(parse(text = layer_major_class)),
                                          args = c(
                                              list(widget = p),
                                              setNames(
                                                  lapply(layer_name,
                                                         function(l) layer[l]),
                                                  layer_name
                                              )
                                          )
                                      )
                                      if(!isVisible) l_layer_hide(p, layer)
                                  }
                           )
                       }
                   }

                   if(length(glyph_index) > 0) {

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
                   p
               }),
        names(splitted_data)
    )

    list(
        plots = plots,
        subtitles = subtitles,
        child = child
    )
}

get_facets.l_serialaxes <- function(widget, by, parent = NULL, linkingGroup,
                                    inheritLayers = TRUE, separate = FALSE,
                                    byDeparse = "by") {

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
    splited <- splitedFun(data = data,
                          by = by,
                          column_names = column_names,
                          byDeparse = byDeparse)
    splitted_data <- splited$splitted_data
    subtitles <- splited$subtitles
    by <- splited$by

    if(length(splitted_data) == 1) return(widget)

    if(separate) {
        child <- parent
    } else {
        # set parent
        if(is.null(parent)) {
            # create parent
            parent <- l_toplevel()
            subwin <- l_subwin(parent, 'facet')

            by_names <- names(subtitles)
            tktitle <- if(!is.null(by_names))
                paste("loon layouts on",
                      deparse(substitute(by_names)), "--path:", subwin)
            else
                paste("loon layouts on",
                      byDeparse, "--path:", subwin)
            tktitle(parent) <- tktitle

            # create child
            child <- as.character(tcl('frame', subwin))
        } else {
            child <- parent
        }
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
        lapply(splitted_data,
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
        names(splitted_data)
    )

    list(
        plots = plots,
        subtitles = subtitles,
        child = child
    )
}

splitedFun <- function(data, by, column_names = NULL,
                       byDeparse = "by", sep = "*",
                       N = nrow(data)) {

    if(is.atomic(by)) {
        if(length(by) == N) {
            # a vector
            subtitles <- setNames(
                list(levels(factor(by))),
                byDeparse
            )
            splitted_data <- split(data,
                                   f = by,
                                   drop = FALSE,
                                   sep = sep)
        } else {

            # some aesthetics (e.g. color, glyph, size, etc) char
            byOriginal <- by
            by <- intersect(by, column_names)
            if(length(by) == 0) {
                stop(byOriginal, " is not a valid setting")
            }
            subtitles <- setNames(lapply(by, function(b) as.character(levels(factor(data[[b]])))), by)
            # split data by "by"
            splitted_data <- split(data,
                                   f = lapply(by, function(b) data[[b]]),
                                   drop = FALSE,
                                   sep = sep)
        }
    } else {
        # by is a data.frame or a list
        ## as.data.frame

        if(is.null(names(by))) {

            by <- as.data.frame(by, stringsAsFactors = FALSE)
            names(by) <- NULL

            subtitles <- lapply(seq(ncol(by)),
                                function(i)
                                    as.character(levels(factor(by[[i]]))))
        } else {
            by <- as.data.frame(by, stringsAsFactors = FALSE)
            by_column_names <- colnames(by)

            subtitles <- setNames(
                lapply(by_column_names,
                       function(b)
                           as.character(levels(factor(by[[b]])))),
                by_column_names
            )
        }

        # split data by "by"
        splitted_data <- split(data,
                               f = by,
                               drop = FALSE,
                               sep = sep)
    }

    list(
        subtitles = subtitles,
        splitted_data = splitted_data,
        by = by
    )
}
