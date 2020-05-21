loonFacets <- function(type, by, args, layout = "grid", byDeparse = "",
                       connectedScales = "both", by_args, linkingGroup, sync, parent,
                       factory_tclcmd, factory_path, factory_window_title,
                       xlabel = "", ylabel = "", title = "", ...) {
    class(type) <- type[1]
    UseMethod("loonFacets", type)
}

loonFacets.default <- function(type,
                               by,
                               args,
                               byDeparse = "",
                               layout = "grid",
                               connectedScales = "both",
                               by_args,
                               linkingGroup, sync, parent,
                               factory_tclcmd, factory_path,
                               factory_window_title,
                               xlabel = "", ylabel = "", title = "", ...) {

    by_names <- colnames(by)

    args$by <- by
    args <- l_na_omit(type[1], args, n_dim_states = c(l_nDimStateNames(type[1]), "by"))
    # remove 'by' from args
    by <- setNames(as.data.frame(args$by, stringsAsFactors = FALSE), by_names)
    args$by <- NULL

    # separate windows or not
    separate <- layout == "separate"

    ## get N dimensional data frame
    # what is the n?
    x <- args[["x"]]
    N <- if(is.null(x)) {
        # serialaxes?
        x <- args[["data"]]
        if(is.null(x)) integer(0) else dim(x)[1]
    } else {
        length(x)
    }
    # N dim args
    data <- as.data.frame(args[which(lengths(args) == N)], stringsAsFactors = FALSE)
    # 1 dim args
    oneDimArgs <- args[which(lengths(args) != N)]

    subtitles <- setNames(lapply(by,
                                 function(b)
                                     as.character(levels(factor(b)))),
                          by_names)

    # split data by "by"
    splitted_data <- split(data, f = as.list(by), drop = FALSE, sep = "*")

    if(length(splitted_data) == 1) {

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
            l_configure(plot,
                        linkingGroup = linkingGroup,
                        sync = sync)
        }

        class(plot) <- c(type, class(plot))
        return(plot)
    }

    if(separate) {
        child <- parent
    } else {
        # set parent
        if(is.null(parent)) {
            # create parent
            parent <- l_toplevel()
            subwin <- l_subwin(parent, 'layout')
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
    if(is.null(linkingGroup)) {
        linkingGroup <- paste0("layout", valid_path())
        message(paste("linkingGroup:", linkingGroup))
    }

    plots <- lapply(splitted_data,
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
    plots <- lapply(plots,
                    function(plot) {
                        l_configure(plot,
                                    linkingGroup = linkingGroup,
                                    sync = sync)
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
                by_args,
                list(plots = plots,
                     subtitles = subtitles,
                     parent = child,
                     xlabel = xlabel,
                     ylabel = ylabel,
                     title = title,
                     swapAxes = swapAxes
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
                by_args,
                list(plots = plots,
                     subtitles = subtitles,
                     parent = child,
                     xlabel = xlabel,
                     ylabel = ylabel,
                     title =  title,
                     swapAxes = swapAxes
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
                                    byDeparse = "",
                                    layout = "grid",
                                    connectedScales = "both",
                                    by_args, linkingGroup, sync, parent,
                                    factory_tclcmd, factory_path,
                                    factory_window_title,
                                    xlabel = "", ylabel = "", title = "", ...) {

    by_names <- colnames(by)

    args$by <- by
    args <- l_na_omit(type[1], args,
                      n_dim_states = c(l_nDimStateNames(type[1]),
                                       "by"))
    # remove 'by' from args
    by <- setNames(as.data.frame(args$by, stringsAsFactors = FALSE), by_names)
    args$by <- NULL

    # separate windows or not
    separate <- layout == "separate"

    ## get N dimensional data frame
    # what is the n?
    x <- args[["x"]]
    N <- if(is.null(x)) {
        # serialaxes?
        x <- args[["data"]]
        if(is.null(x)) integer(0) else dim(x)[1]
    } else {
        length(x)
    }
    # N dim args
    data <- cbind(index = 1:N, as.data.frame(args[which(lengths(args) == N)], stringsAsFactors = FALSE))

    serialaxesData <- args$data
    # 1 dim args
    args$data <- NULL
    oneDimArgs <- args[which(lengths(args) != N)]

    subtitles <- setNames(lapply(by, function(b) as.character(levels(factor(b)))), by_names)

    # split data by "by"
    splitted_data <- split(data, f = as.list(by), drop = FALSE, sep = "*")

    if(length(splitted_data) == 1) {

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
            l_configure(plot,
                        linkingGroup = linkingGroup,
                        sync = sync)
        }

        class(plot) <- c(type, class(plot))
        return(plot)
    }

    if(separate) {
        child <- parent
    } else {
        # set parent
        if(is.null(parent)) {
            # create parent
            parent <- l_toplevel()
            subwin <- l_subwin(parent, 'layout')

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
    if(is.null(linkingGroup)) {
        linkingGroup <- paste0("layout", valid_path())
        message(paste("linkingGroup:", linkingGroup))
    }

    plots <- lapply(splitted_data,
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
                by_args,
                list(plots = plots,
                     subtitles = subtitles,
                     parent = child,
                     xlabel = xlabel,
                     ylabel = ylabel,
                     title = title
                )
            )
        )

        # set class and linkingGroup for each plot
        plots <- lapply(plots,
                        function(plot) {
                            l_configure(plot,
                                        linkingGroup = linkingGroup,
                                        sync = sync)
                        })

        structure(
            plots,
            class = c("l_facet_grid", "l_facet", "l_compound", "loon")
        )

    } else if(layout == "wrap") {

        plots <- do.call(
            facet_wrap_layout,
            c(
                by_args,
                list(plots = plots,
                     subtitles = subtitles,
                     parent = child,
                     xlabel = xlabel,
                     ylabel = ylabel,
                     title =  title
                )
            )
        )

        # set class and linkingGroup for each plot
        plots <- lapply(plots,
                        function(plot) {
                            l_configure(plot,
                                        linkingGroup = linkingGroup,
                                        sync = sync)
                        })

        structure(
            plots,
            class = c("l_facet_wrap", "l_facet", "l_compound", "loon")
        )
    } else
        stop("Unknown layouts")
}
