#' @title A helper function that produces facetted plots at the time of constructing a loon plot.
#'
#' @description
#' Facets across multiple panels can be created from an existing loonplot via \code{l_facet} or
#' directly at the time of the original loon plot call (without constructing the unfacetted loon plot itself).
#' \code{loonFacets} is that helper function called by the various loon plot creation function
#' (e.g., \code{l_plot}, \code{l_hist}, etc.).  at the time of their creation to
#' produce the facets. It should rarely be called directly by the user.
#'
#' The function makes use of the general \code{loonPlotFactory} interface to tcl.
#'
#' For detailed information on its common arguments, see the arguments of \code{\link{l_facet}} or those of the
#' loon plot constructor (e.g., \code{\link{l_plot}}, etc.)
#'
#' @param type the class name of the loon plot
#' @param by loon plot can be separated by some variables into mutiple panels.
#' This argument can take a \code{vector}, a \code{list} of same lengths or a \code{data.frame} as input.
#' @param args named list of N-dimensional arguments (e.g., x, y, selected, etc.)
#' @param on if the \code{by} is a formula, an optional data frame containing the variables in the \code{by}.
#' If variables in \code{by} is not found in data, the variables are taken from environment(formula),
#' typically the environment from which the function is called.
#' @param bySubstitute effectively a call of \code{substitute(by)} on the \code{by} arguments
#' used to generate warnings or errors.
#' @param layout layout facets as \code{'grid'}, \code{'wrap'} or \code{'separate'}
#' @param connectedScales Determines how the scales of the facets are to be connected depending
#' on which \code{layout} is used.  For each value of \code{layout}, the scales are connected
#' as follows:
#' \itemize{
#' \item{\code{layout = "wrap":}  Across all facets, when \code{connectedScales} is
#'    \itemize{
#'    \item{\code{"x"}, then  only the "x"  scales are connected}
#'    \item{\code{"y"}, then only the "y" scales are connected}
#'    \item{\code{"both"},  both "x" and "y" scales are connected}
#'    \item{\code{"none"},  neither "x" nor "y" scales are connected.}
#'    For any other value, only the "y" scale is connected.
#'    }
#'    }
#' \item{\code{layout = "grid":}  Across all facets, when \code{connectedScales} is
#'    \itemize{
#'    \item{\code{"cross"}, then only the scales in the same row and the same column are connected}
#'    \item{\code{"row"}, then both "x" and "y" scales of facets in the same row are connected}
#'    \item{\code{"column"}, then both "x" and "y" scales of facets in the same column are connected}
#'    \item{\code{"x"}, then all of the "x"  scales are connected (regardless of column)}
#'    \item{\code{"y"}, then all of the "y" scales are connected (regardless of row)}
#'    \item{\code{"both"},  both "x" and "y" scales are connected in all facets}
#'    \item{\code{"none"},  neither "x" nor "y" scales are connected in any facets.}
#'    }
#'    }
#'  }
#' @param byArgs further arguments to be used in determining how \code{by} will be used.
#' @param linkingGroup the string naming the group of plots to be linked to the facets.
#' @param sync string identifying how to synchronize the aesthetics with
#' linked plots (i.e., "push" or "pull", "pull" by default).
#' @param parent the parent loon widget
#' @param factory_tclcmd the tcl command to be given to loonPlotFactory.  For example, the
#' loon histogram tcl command is the string '::loon::histogram'
#' @param factory_path the tcl path given to loonPlotFactory to identify the widgets; for example,
#' the string 'hist' to prefix histograms.
#' @param factory_window_title the window title to be given to loonPlotFactory to better identify
#' the window for the user; for example, the string 'loon histogram'.
#' @param xlabel string to label the x direction of the facets
#' @param ylabel string to label the y direction of the facets
#' @param title string providing a title for the collection of facets
#' @param modifiedLinkedStates states of the plot to be synchronized with plots
#' in the same linking group.  Used with \code{sync = "push"}.
#' @param ... named arguments to modify the `loon` widget states

#'
#' @seealso \code{\link{l_facet}} and, for example, \code{\link{l_plot}}, \code{\link{l_hist}}, \code{\link{l_serialaxes}}
#'
#' @export
#' @keywords internal
loonFacets <- function(type, by, args, on, bySubstitute, layout = "grid",
                       connectedScales = "both", byArgs, linkingGroup, sync, parent,
                       factory_tclcmd, factory_path, factory_window_title,
                       xlabel = "", ylabel = "", title = "",
                       modifiedLinkedStates = character(0L), ...) {
    class(type) <- type
    UseMethod("loonFacets", type)
}

#' @export
#' @keywords internal
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
    if(layout == "grid") lex.order <- FALSE else lex.order <- TRUE
    splitNDimArgs <- split(nDimArgs,
                           f = as.list(byDataFrame),
                           drop = FALSE,
                           lex.order = lex.order,
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

#' @export
#' @keywords internal
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
    if(layout == "grid") lex.order <- FALSE else lex.order <- TRUE
    splitNDimArgs <- split(nDimArgs,
                           f = as.list(byDataFrame),
                           drop = FALSE,
                           lex.order = lex.order,
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
                    on <- as.data.frame(args)
                    vars <- all.vars(by)
                    colnames <- colnames(on)
                    on[vars[vars %in% colnames]]
                }
            )


        } else {

            tryCatch(
                expr = {
                    vars <- all.vars(by)
                    colnames <- colnames(on)
                    on[vars[vars %in% colnames]]
                },
                error = function(e) {
                    model.frame(by, data = on)
                }
            )

        }

    } else {

        standardizedBy <- function(by, bySubstitute, args, n) {

            if(is.atomic(by)) {
                names <- by
            } else {

                names <- names(by)
                whichIsEmpty <- names == ""

                if(is.null(names) || any(whichIsEmpty)) {
                    possibleNames <- vapply(2:length(bySubstitute),
                                    function(i) {
                                        deparse(bySubstitute[[i]])
                                    }, character(1L))
                    if(any(whichIsEmpty)) {
                        names[whichIsEmpty] <- possibleNames[whichIsEmpty]
                    } else {
                        names <- possibleNames
                    }
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
