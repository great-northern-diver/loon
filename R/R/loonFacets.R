loonFacets <- function(type, by, args, on, bySubstitute, layout = "grid",
                       connectedScales = "both", byArgs, linkingGroup, sync, parent,
                       factory_tclcmd, factory_path, factory_window_title,
                       xlabel = "", ylabel = "", title = "",
                       modifiedLinkedStates = character(0L), ...) {
    class(type) <- type
    UseMethod("loonFacets", type)
}

loonFacets.default <- function(type,
                               by,
                               args,
                               on,
                               bySubstitute,
                               layout = "grid",
                               connectedScales = "both",
                               byArgs,
                               linkingGroup, sync, parent,
                               factory_tclcmd, factory_path,
                               factory_window_title,
                               xlabel = "", ylabel = "", title = "",
                               modifiedLinkedStates = character(0L), ...) {

    ## get N dimensional data frame
    # what is the n?
    x <- args[["x"]]
    N <- length(x)

    # in case: `by` is a formula
    byDataFrame <- by2Data(by, on, bySubstitute = bySubstitute,
                           n = N, args = args,
                           l_className = type[1L])

    # byDataFrame is a data frame
    byNames <- colnames(byDataFrame)

    args$byDataFrame <- byDataFrame
    args <- l_na_omit(type[1L], args, n_dim_states = c(l_nDimStateNames(type[1L]), "byDataFrame"))
    # remove NAs
    byDataFrame <- setNames(as.data.frame(args$byDataFrame, stringsAsFactors = FALSE),
                            byNames)
    # remove 'byDataFrame' from args
    args$byDataFrame <- NULL

    # separate windows or not
    separate <- layout == "separate"

    # `by` includes NAs so that the length may vary
    N <- nrow(byDataFrame)

    # N dim args
    nDimArgs <- as.data.frame(args[which(lengths(args) == N)],
                              stringsAsFactors = FALSE)
    # 1 dim args
    oneDimArgs <- args[which(lengths(args) != N)]

    subtitles <- setNames(lapply(byDataFrame,
                                 function(b)
                                     as.character(levels(factor(b)))),
                          byNames)

    # split nDimArgs by "byDataFrame"
    splitNDimArgs <- split(nDimArgs,
                           f = as.list(byDataFrame),
                           drop = FALSE,
                           sep = "*")
    len <- length(splitNDimArgs)
    if(len == 1) {

        plot <- do.call(
            loonPlotFactory,
            c(
                args,
                list(factory_tclcmd = factory_tclcmd,
                     factory_path = factory_path,
                     factory_window_title = factory_window_title,
                     parent = parent),
                list(...)
            )
        )

        if(!is.null(linkingGroup)) {

            syncTemp <- ifelse(length(modifiedLinkedStates) == 0,  sync, "pull")
            if(syncTemp == "push")
                message("The modification of linked states is not detected",
                        " so that the default settings will be pushed to all plots")
            # configure plot (linking)
            l_configure(plot,
                        linkingGroup = linkingGroup,
                        sync = syncTemp)

            if(sync == "push" && length(modifiedLinkedStates) > 0) {

                do.call(l_configure,
                        c(
                            list(
                                target = plot,
                                linkingGroup = linkingGroup,
                                sync = sync
                            ),
                            nDimArgs[modifiedLinkedStates]
                        )
                )
            } else {
                l_linkingWarning(plot, sync, args = nDimArgs,
                                 modifiedLinkedStates = modifiedLinkedStates,
                                 l_className = type[1L])
            }
        }

        class(plot) <- c(type, class(plot))
        return(plot)
    }

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
        tktitle(parent) <- if(!is.null(byNames))
            paste("loon layouts on",
                  deparse(substitute(byNames)), "--path:", subwin)
        else
            paste("loon layouts on",
                  deparse(bySubstitute), "--path:", subwin)

        # create child
        child <- as.character(tcl('frame', subwin))
    }

    # linkingGroup
    new.linkingGroup <- FALSE
    if(is.null(linkingGroup)) {
        linkingGroup <- paste0("layout", valid_path())
        message(paste("linkingGroup:", linkingGroup))
        new.linkingGroup <- TRUE
    }

    plots <- lapply(splitNDimArgs,
                    function(d) {

                        if(dim(d)[1] == 0) {

                            p <- if(separate) {

                                loonPlotFactory(
                                    factory_tclcmd = factory_tclcmd,
                                    factory_path = factory_path,
                                    factory_window_title = factory_window_title,
                                    parent = child,
                                    ...
                                )

                            } else {
                                loonPlotFactory(
                                    factory_tclcmd = factory_tclcmd,
                                    factory_path = factory_path,
                                    factory_window_title = factory_window_title,
                                    parent = child,
                                    xlabel = "",
                                    ylabel = "",
                                    title = "",
                                    minimumMargins = rep(5, 4),
                                    ...
                                )
                            }

                        } else {

                            if(!separate) {
                                oneDimArgs$minimumMargins <- rep(5, 4)
                                oneDimArgs$xlabel <- ""
                                oneDimArgs$ylabel <- ""
                                oneDimArgs$title <- ""
                            }

                            p <- do.call(
                                loonPlotFactory,
                                c(
                                    as.list(d),
                                    oneDimArgs,
                                    list(factory_tclcmd = factory_tclcmd,
                                         factory_path = factory_path,
                                         factory_window_title = factory_window_title,
                                         parent = child),
                                    list(...)
                                )
                            )
                        }

                        class(p) <- c(type, class(p))
                        p
                    })

    # set linkingGroup for each plot

    lapply(seq(len),
           function(i) {

               plot <- plots[[i]]

               if(!new.linkingGroup) {

                   syncTemp <- ifelse(length(modifiedLinkedStates) == 0,  sync, "pull")
                   # give message once
                   if(i == 1L && syncTemp == "push") {
                       message("The modification of linked states is not detected",
                               " so that the default settings will be pushed to all plots")
                   }
                   l_configure(plot,
                               linkingGroup = linkingGroup,
                               sync = syncTemp)

                   if(sync == "push" && length(modifiedLinkedStates) > 0) {

                       do.call(l_configure,
                               c(
                                   list(
                                       target = plot,
                                       linkingGroup = linkingGroup,
                                       sync = sync
                                   ),
                                   splitNDimArgs[[i]][modifiedLinkedStates]
                               )
                       )
                   } else {

                       l_linkingWarning(plot, sync,
                                        args = splitNDimArgs[[i]],
                                        modifiedLinkedStates = modifiedLinkedStates)
                   }

               } else {

                   l_configure(plot,
                               linkingGroup = linkingGroup,
                               sync = sync)
               }
           })

    xrange <- c()
    yrange <- c()
    lapply(plots,
           function(p) {
               xrange <<- c(xrange, p['panX'], p['panX'] + p['deltaX']/p['zoomX'])
               yrange <<- c(yrange, p['panY'], p['panY'] + p['deltaY']/p['zoomY'])
           })
    xrange <- extendrange(xrange)
    yrange <- extendrange(yrange)

    swapAxes <- ifelse(is.null(oneDimArgs$swapAxes), FALSE, oneDimArgs$swapAxes)

    if(swapAxes) {
        connectedScales <- switch(connectedScales,
                                  "x" = "y",
                                  "y" = "x",
                                  {
                                      connectedScales
                                  })
    }

    if(separate) {

        # force scales
        forceScales(plots = plots,
                    xrange = xrange,
                    yrange = yrange,
                    connectedScales = connectedScales)

        facet_separate_layout(plots = plots,
                              subtitles = subtitles,
                              title = title,
                              xlabel = xlabel,
                              ylabel = ylabel,
                              labelMargins = args$labelMargins)

        p <- structure(
            plots,
            class = c("l_facet", "l_compound", "loon")
        )
        return(p)
    }

    if(!is.null(oneDimArgs$title)) title <- oneDimArgs$title

    # forbidden swapAxes
    swap_forbiddenSetting(plots,
                          child = child,
                          swapAxes = swapAxes)

    # synchronize scales
    linkOneDimensionalStates(plots,
                             oneDimensionalStates = c("showScales", "showLabels", "showGuides"))

    if(layout == "grid") {

        ## Two major objectives here
        # 1. pack plots and labels
        # 2. rename and reorder plots
        plots <- do.call(
            facet_grid_layout,
            c(
                byArgs,
                list(plots = plots,
                     subtitles = subtitles,
                     by = by,
                     parent = child,
                     xlabel = xlabel,
                     ylabel = ylabel,
                     title = title,
                     swapAxes = swapAxes,
                     new.toplevel = new.toplevel
                )
            )
        )

        layout_synchronizeSetting(plots,
                                  connectedScales = connectedScales,
                                  xrange = xrange,
                                  yrange = yrange,
                                  child = child)

        structure(
            plots,
            class = c("l_facet_grid", "l_facet", "l_compound", "loon")
        )

    } else if(layout == "wrap") {

        plots <- do.call(
            facet_wrap_layout,
            c(
                byArgs,
                list(plots = plots,
                     subtitles = subtitles,
                     parent = child,
                     xlabel = xlabel,
                     ylabel = ylabel,
                     title =  title,
                     swapAxes = swapAxes,
                     new.toplevel = new.toplevel
                )
            )
        )

        layout_synchronizeSetting(plots,
                                  connectedScales = connectedScales,
                                  xrange = xrange,
                                  yrange = yrange,
                                  child = child)

        structure(
            plots,
            class = c("l_facet_wrap", "l_facet", "l_compound", "loon")
        )
    } else
        stop("Unknown layouts")
}

loonFacets.l_serialaxes <- function(type,
                                    by,
                                    args,
                                    on,
                                    bySubstitute,
                                    layout = "grid",
                                    connectedScales = "both",
                                    byArgs, linkingGroup, sync, parent,
                                    factory_tclcmd, factory_path,
                                    factory_window_title,
                                    xlabel = "", ylabel = "", title = "",
                                    modifiedLinkedStates = character(0L), ...) {

    ## get N dimensional data frame
    # what is the n?
    x <- args[["data"]]
    N <- dim(x)[1]

    # in case: `by` is a formula
    byDataFrame <- by2Data(by, on,
                           bySubstitute = bySubstitute,
                           n = N, args = args,
                           l_className = type[1L])

    byNames <- colnames(byDataFrame)

    args$byDataFrame <- byDataFrame
    args <- l_na_omit(type[1], args,
                      n_dim_states = c(l_nDimStateNames(type[1]),
                                       "byDataFrame"))
    # remove 'byDataFrame' from args
    byDataFrame <- setNames(as.data.frame(args$byDataFrame,
                                          stringsAsFactors = FALSE),
                            byNames)
    args$byDataFrame <- NULL

    # separate windows or not
    separate <- layout == "separate"

    # `by` includes NAs so that the length may vary
    N <- nrow(byDataFrame)

    # N dim args
    nDimArgs <- cbind(index = seq(N),
                      as.data.frame(args[which(lengths(args) == N)],
                                    stringsAsFactors = FALSE))

    serialaxesData <- args$data
    # 1 dim args
    args$data <- NULL
    oneDimArgs <- args[which(lengths(args) != N)]

    subtitles <- setNames(lapply(byDataFrame, function(b) as.character(levels(factor(b)))), byNames)

    # split data by "byDataFrame"
    splitNDimArgs <- split(nDimArgs,
                           f = as.list(byDataFrame),
                           drop = FALSE, sep = "*")
    len <- length(splitNDimArgs)

    if(len == 1) {

        plot <- do.call(
            loonPlotFactory,
            c(
                args,
                list(factory_tclcmd = factory_tclcmd,
                     factory_path = factory_path,
                     factory_window_title = factory_window_title,
                     parent = parent,
                     data = l_data(serialaxesData)),
                list(...)
            )
        )

        if(!is.null(linkingGroup)) {

            syncTemp <- ifelse(length(modifiedLinkedStates) == 0,  sync, "pull")
            if(syncTemp == "push")
                message("The modification of linked states is not detected",
                        " so that the default settings will be pushed to all plots")
            # configure plot (linking)
            l_configure(plot,
                        linkingGroup = linkingGroup,
                        sync = syncTemp)

            if(sync == "push" && length(modifiedLinkedStates) > 0) {

                do.call(l_configure,
                        c(
                            list(
                                target = plot,
                                linkingGroup = linkingGroup,
                                sync = sync
                            ),
                            nDimArgs[modifiedLinkedStates]
                        )
                )
            } else {
                l_linkingWarning(plot, sync, args = nDimArgs,
                                 modifiedLinkedStates = modifiedLinkedStates,
                                 l_className = type[1L])
            }
        }

        class(plot) <- c(type, class(plot))
        return(plot)
    }

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
        tktitle(parent) <- if(!is.null(byNames))
            paste("loon layouts on",
                  deparse(substitute(byNames)), "--path:", subwin)
        else
            paste("loon layouts on",
                  deparse(bySubstitute), "--path:", subwin)

        # create child
        child <- as.character(tcl('frame', subwin))
    }

    # linkingGroup
    new.linkingGroup <- FALSE
    if(is.null(linkingGroup)) {
        linkingGroup <- paste0("layout", valid_path())
        message(paste("linkingGroup:", linkingGroup))
        new.linkingGroup <- TRUE
    }

    plots <- lapply(splitNDimArgs,
                    function(d) {

                        if(dim(d)[1] == 0) {

                            p <- loonPlotFactory(
                                factory_tclcmd = factory_tclcmd,
                                factory_path = factory_path,
                                factory_window_title = factory_window_title,
                                parent = child,
                                title = "",
                                ...
                            )

                        } else {

                            oneDimArgs$xlabel <- NULL
                            oneDimArgs$ylabel <- NULL
                            oneDimArgs$title <- NULL

                            index <- d$index
                            d$index <- NULL

                            p <- do.call(
                                loonPlotFactory,
                                c(
                                    as.list(d),
                                    oneDimArgs,
                                    list(factory_tclcmd = factory_tclcmd,
                                         factory_path = factory_path,
                                         factory_window_title = factory_window_title,
                                         parent = child,
                                         title = "",
                                         data = l_data(serialaxesData[index, ])),
                                    list(...)
                                )
                            )
                        }

                        class(p) <- c(type, class(p))
                        p
                    })


    lapply(seq(len),
           function(i) {

               plot <- plots[[i]]

               if(!new.linkingGroup) {

                   syncTemp <- ifelse(length(modifiedLinkedStates) == 0,  sync, "pull")
                   # give message once
                   if(i == 1L && syncTemp == "push") {
                       message("The modification of linked states is not detected",
                               " so that the default settings will be pushed to all plots")
                   }
                   l_configure(plot,
                               linkingGroup = linkingGroup,
                               sync = syncTemp)

                   if(sync == "push" && length(modifiedLinkedStates) > 0) {

                       do.call(l_configure,
                               c(
                                   list(
                                       target = plot,
                                       linkingGroup = linkingGroup,
                                       sync = sync
                                   ),
                                   splitNDimArgs[[i]][modifiedLinkedStates]
                               )
                       )
                   } else {
                       l_linkingWarning(plot, sync,
                                        args = splitNDimArgs[[i]],
                                        modifiedLinkedStates = modifiedLinkedStates)
                   }

               } else {

                   l_configure(plot,
                               linkingGroup = linkingGroup,
                               sync = sync)
               }
           })

    if(!is.null(oneDimArgs$title)) title <- oneDimArgs$title

    if(separate) {

        facet_separate_layout(plots = plots,
                              subtitles = subtitles,
                              title = title)

        # scale to plot
        return(
            structure(
                plots,
                class = c("l_facet", "l_compound", "loon")
            )
        )
    }

    if(layout == "grid") {

        ## Two major objectives here
        # 1. pack plots and labels
        # 2. rename and reorder plots
        plots <- do.call(
            facet_grid_layout,
            c(
                byArgs,
                list(plots = plots,
                     subtitles = subtitles,
                     by = by,
                     parent = child,
                     xlabel = xlabel,
                     ylabel = ylabel,
                     title = title,
                     new.toplevel = new.toplevel
                )
            )
        )

        structure(
            plots,
            class = c("l_facet_grid", "l_facet", "l_compound", "loon")
        )

    } else if(layout == "wrap") {

        plots <- do.call(
            facet_wrap_layout,
            c(
                byArgs,
                list(plots = plots,
                     subtitles = subtitles,
                     parent = child,
                     xlabel = xlabel,
                     ylabel = ylabel,
                     title =  title,
                     new.toplevel = new.toplevel
                )
            )
        )

        structure(
            plots,
            class = c("l_facet_wrap", "l_facet", "l_compound", "loon")
        )
    } else
        stop("Unknown layouts")
}

# convert all types of 'by' to a data frame
by2Data <- function(by, on, bySubstitute,
                    n, args, l_className) {

    if(inherits(by, "formula")) {

        by <- if(missing(on)) {

            tryCatch(
                model.frame(by),
                error = function(e) {
                    model.frame(by, data = args)
                }
            )


        } else {

            tryCatch(
                expr = {
                    model.frame(by, data = on)
                },
                error = function(e) {
                    on[, all.vars(by)]
                }
            )

        }

    } else {

        standardizedBy <- function(by, bySubstitute, args, n) {

            if(is.atomic(by)) {
                names <- by
            } else {
                names <- names(by)
                if(is.null(names)) {
                    names <- vapply(2:length(bySubstitute),
                                    function(i) {
                                        deparse(bySubstitute[[i]])
                                    }, character(1L))
                }
            }

            i <- 0
            by <- lapply(by,
                         function(b) {

                             i <<- i + 1

                             if(length(b) == 1) {

                                 state <- args[[b]]

                                 if(!b %in% l_nDimStateNames(l_className)) {
                                     warning(deparse(bySubstitute[[i + 1]]),
                                             " is not recognized and removed", call. = FALSE)
                                 }

                                 return(state)
                             }

                             if(length(b) == n) return(b)

                             warning("The ", deparse(bySubstitute[[i + 1]]),
                                     " is neither a valid state nor a valid vector", call. = FALSE)

                             NULL

                         })

            isNotNULL <- unlist(Map(Negate(is.null), by))
            by <- by[isNotNULL]
            names <- names[isNotNULL]
            setNames(as.data.frame(by, stringsAsFactors = FALSE),
                     names)
        }

        if(is.atomic(by)) {
            if(length(by) == n) {
                by <- tryCatch(
                    expr = {setNames(data.frame(by, stringsAsFactors = FALSE), deparse(bySubstitute))},
                    error = function(e) {setNames(data.frame(by, stringsAsFactors = FALSE), "by")}
                )
            } else {
                # by is a char
                # aesthetics states, e.g. "color", "size", etc
                by <- standardizedBy(by, bySubstitute, args, n)
            }
        } else {

            by <- standardizedBy(by, bySubstitute, args, n)
        }
    }

    if(nrow(by) != n)
        stop("'by' must be an n-dimensional data")

    return(by)
}
