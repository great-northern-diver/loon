#' @title Layout as a grid
#' @param plots A list of \code{loon} plots
#' @param subtitles The subtitles of the layout. It is a list and the length is equal to
#' the number of \code{by} variables. Each element in a list is the unique values of such \code{by} variable.
#' @param by an object of class "formula" (or one that can be coerced to that class):
#' a symbolic description of the plots separated by
#' @param prop The proportion of the label height and widget height
#' @param parent a valid Tk parent widget path. When the parent widget is
#'   specified (i.e. not \code{NULL}) then the plot widget needs to be placed using
#'   some geometry manager like \code{\link{tkpack}} or \code{\link{tkplace}} in
#'   order to be displayed. See the examples below.
#' @param title The title of the widget
#' @param xlabel The xlabel of the widget
#' @param ylabel The ylabel of the widget
#' @param labelLocation Labels location.
#' \itemize{
#' \item {
#' Length two vector for layout \code{grid}.
#' The first one is used to determine the position of column labels ('top' or 'bottom').
#' The second one is used to determine the position of row labels ('right' or 'left').
#' }
#' \item {
#' Length one vector for layout \code{wrap}, 'top' or 'bottom'.
#' }
#' }
#' @param byrow Place widget by row or by column
#' @param swapAxes swap axes, \code{TRUE} or \code{FALSE}
#' @param labelBackground Label background color
#' @param labelForeground Label foreground color
#' @param labelBorderwidth Label border width
#' @param labelRelief Label relief
#' @param plotWidth default plot width (in pixel)
#' @param plotHeight default plot height (in pixel)
#' @param sep The character string to separate or combine a vector
#' @param maxCharInOneRow Max char in one row. If it exceeds the max, then a char will be displayed into two rows
#' @param new.toplevel determine whether the parent is a new top level.
#' If it is not a new window, the widgets will not be packed
#' @param ... named arguments to modify plot states.
#' See \code{\link{l_info_states}} of any instantiated l_plot for examples of names and values.
#'
facet_grid_layout <- function(plots,
                              subtitles,
                              by = NULL,
                              prop = 10,
                              parent = NULL,
                              title = "",
                              xlabel = "",
                              ylabel = "",
                              labelLocation = c("top", "right"),
                              byrow = FALSE,
                              swapAxes = FALSE,
                              labelBackground = l_getOption("facetLabelBackground"),
                              labelForeground = l_getOption("foreground"),
                              labelBorderwidth = 2,
                              labelRelief = "ridge",
                              plotWidth = 200,
                              plotHeight = 200,
                              sep = "*",
                              maxCharInOneRow = 15,
                              new.toplevel = TRUE,
                              ...) {

    len <- length(subtitles)
    span <- prop

    # tk configure canvas size
    sapply(plots,
           function(p) {
               tkconfigure(paste(p,'.canvas',sep=''),
                           width = plotWidth,
                           height = plotHeight)
           }
    )
    plot_names <- names(plots)

    if(swapAxes) {
        label <- xlabel
        xlabel <- ylabel
        ylabel <- label
    }

    rowGoesFirst <- FALSE

    if(inherits(by, "formula")) {

        if(length(by) == 2) {

            colsLabel <- subtitles[all.vars(by[[2]])]
            rowsLabel <- list()

        } else {
            # vars before tilde
            rowsLabel <- subtitles[all.vars(by[[2]])]
            # vars after tilde
            colsLabel <- subtitles[all.vars(by[[3]])]
        }

    } else {
        if(byrow) {

            if(len == 1) {
                colsLabel <- list()
                rowsLabel <- subtitles
            } else {
                rowsLabel <- subtitles[seq(len) <= (len/2)]
                colsLabel <- subtitles[seq(len) > (len/2)]
            }
        } else {

            if(len == 1) {
                colsLabel <- subtitles
                rowsLabel <- list()
            } else {
                rowsLabel <- subtitles[seq(len) > (len/2)]
                colsLabel <- subtitles[seq(len) <= (len/2)]
            }
            rowGoesFirst <- TRUE
        }
    }

    if(swapAxes) {
        label <- colsLabel
        colsLabel <- rowsLabel
        rowsLabel <- label
        rowGoesFirst <- !rowGoesFirst
    }

    nrowsLabel <- prod(lengths(rowsLabel))
    ncolsLabel <- prod(lengths(colsLabel))
    len_rowsLabel <- length(rowsLabel)
    len_colsLabel <- length(colsLabel)

    if(length(labelLocation) != 2) {
        warning("Layout 'grid' accept two positions.\nThe first one is used to determine the postion of column labels.\nThe second one is used to determine the postion of row labels.")
        labelLocation <- rep_len(labelLocation, 2)
    }

    columnLabelLocation <- labelLocation[1]
    rowLabelLocation <- labelLocation[2]

    if(!columnLabelLocation %in% c("top", "bottom")) {
        warning("The first one is the column label location, one of 'top' or 'bottom'")
        columnLabelLocation <- "top"
    }

    if(!rowLabelLocation %in% c("right", "left")) {
        warning("The second one is the row label location, one of 'right' or 'left'")
        rowLabelLocation <- "right"
    }

    row_start_pos <- ifelse(columnLabelLocation == "top", len_colsLabel, 0)
    col_start_pos <- ifelse(rowLabelLocation == "right", 0, len_rowsLabel)

    # To simplify the settings of labels, we clarify that
    ## 1. title will be always on top
    ## 2. ylabel is always on left
    ## 3. xlabel is always on bottom
    title_span <- 2
    title_pos <- ifelse(title == "", 0, title_span)
    ylabel_pos <- ifelse(ylabel == "", 0, 1)

    new_names <- c()

    if(rowGoesFirst) {

        for(j in seq(nrowsLabel)) {
            for(i in seq(ncolsLabel)) {
                id <- (j - 1) * ncolsLabel + i
                # plotid <- get_plot_id(layout_orders[id],
                #                       plot_names,
                #                       sep = sep)
                # plot_id <- c(plot_id, plotid)
                new_names <- c(new_names, paste0("x", j, "y", i))
                tkgrid(plots[[id]],
                       row = (j - 1) * span + row_start_pos + title_pos, # leave space for labels
                       column = (i - 1) * span + col_start_pos + ylabel_pos,
                       rowspan = span,
                       columnspan = span,
                       sticky="nesw")

            }
        }


    } else {

        for(i in seq(ncolsLabel)) {
            for(j in seq(nrowsLabel)) {
                id <- (i - 1) * nrowsLabel + j
                # plotid <- get_plot_id(layout_orders[id],
                #                       plot_names,
                #                       sep = sep)
                # plot_id <- c(plot_id, plotid)
                new_names <- c(new_names, paste0("x", j, "y", i))
                tkgrid(plots[[id]],
                       row = (j - 1) * span + row_start_pos + title_pos, # leave space for labels
                       column = (i - 1) * span + col_start_pos + ylabel_pos,
                       rowspan = span,
                       columnspan = span,
                       sticky="nesw")

            }
        }
    }

    # pack column labels
    if(len_colsLabel > 0) {
        colsLabel <- rev(colsLabel)
        fluid_colsLabel <- colsLabel

        # the names can be NULL
        column_names <- names(fluid_colsLabel)
        fluid_colsLabel_names <- column_names

        if(is.null(fluid_colsLabel_names)) {
            fluid_colsLabel_names <- as.character(seq(length(fluid_colsLabel)))
            names(fluid_colsLabel) <- fluid_colsLabel_names
        }

        lapply(seq(length(fluid_colsLabel_names)),
               function(j) {

                   fluid_colsLabel_name <-  fluid_colsLabel_names[j]

                   col <- if(j == 1) {
                       fluid_colsLabel[[fluid_colsLabel_name]]
                   } else {
                       rep(fluid_colsLabel[[fluid_colsLabel_name]], prod(lengths(colsLabel[1:(j - 1)])))
                   }
                   fluid_colsLabel[fluid_colsLabel_name] <<- NULL
                   extent <- prod(lengths(fluid_colsLabel))
                   columnspan <- extent * span

                   name <- column_names[j]
                   label <- if(is.null(name)) {
                       col
                   } else {
                       if(grepl("color", name)) {
                           paste(name, color.id(col), sep = ":")
                       } else {
                           tt <- paste(name, col, sep = ":")
                           if(any(nchar(tt) >= maxCharInOneRow))
                               paste(name, col, sep = ":\n")
                           else
                               tt
                       }
                   }

                   for(i in seq(length(label))) {

                       text <- label[i]
                       tkcolname <- as.character(tcltk::tcl('label',
                                                            as.character(l_subwin(parent,
                                                                                  paste0('columnlabel-', columnLabelLocation, '-',
                                                                                         'x', j, 'y', i, 'extent', extent))),
                                                            text = text,
                                                            bg = labelBackground,
                                                            fg = labelForeground,
                                                            borderwidth = labelBorderwidth,
                                                            relief = labelRelief))
                       if(columnLabelLocation == "top")
                           tkgrid(tkcolname,
                                  row = (j - 1) + title_pos,
                                  column = (i - 1) * columnspan + col_start_pos + ylabel_pos,
                                  rowspan = 1,
                                  columnspan = columnspan,
                                  sticky="nesw")
                       else
                           tkgrid(tkcolname,
                                  row = -(j - 1) + nrowsLabel * span + len_rowsLabel + title_pos,
                                  column= (i - 1) * columnspan + col_start_pos + ylabel_pos,
                                  rowspan = 1,
                                  columnspan = columnspan,
                                  sticky="nesw")
                   }
               })
    }

    # pack row labels
    if(len_rowsLabel > 0) {
        rowsLabel <- rev(rowsLabel)
        fluid_rowsLabel <- rowsLabel

        # the names can be NULL
        row_names <- names(fluid_rowsLabel)
        fluid_rowsLabel_names <- row_names

        if(is.null(fluid_rowsLabel_names)) {
            fluid_rowsLabel_names <- as.character(seq(length(fluid_rowsLabel)))
            names(fluid_rowsLabel) <- fluid_rowsLabel_names
        }

        lapply(seq(length(fluid_rowsLabel_names)),
               function(i) {

                   fluid_rowsLabel_name <-  fluid_rowsLabel_names[i]

                   row <- if(i == 1) {
                       fluid_rowsLabel[[fluid_rowsLabel_name]]
                   } else {
                       rep(fluid_rowsLabel[[fluid_rowsLabel_name]], prod(lengths(rowsLabel[1:(i - 1)])))
                   }
                   fluid_rowsLabel[fluid_rowsLabel_name] <<- NULL

                   name <- row_names[i]

                   extent <- prod(lengths(fluid_rowsLabel))
                   rowspan <- extent * span
                   for(j in seq(length(row))) {
                       # row index
                       label <- row[j]
                       text <- if(is.null(name)) {
                           label
                       } else {
                           ifelse(grepl("color", name),
                                  paste(name, color.id(label), sep = ":"),
                                  paste(name, label, sep = ":"))
                       }
                       tkrowname <- as.character(tcltk::tcl('label',
                                                            as.character(l_subwin(parent,
                                                                                  paste0('rowlabel-', rowLabelLocation, '-',
                                                                                         'x', j, 'y', i, 'extent', extent))),
                                                            text = paste(paste0(" ", strsplit(text, "")[[1]], " "), collapse = "\n"),
                                                            bg = labelBackground,
                                                            fg = labelForeground,
                                                            borderwidth = labelBorderwidth,
                                                            relief = labelRelief))
                       if(rowLabelLocation == "right")
                           tkgrid(tkrowname,
                                  row = (j - 1) * rowspan + row_start_pos + title_pos,
                                  column= -(i - 1) + ncolsLabel * span + len_colsLabel + ylabel_pos,
                                  rowspan = rowspan,
                                  columnspan = 1,
                                  sticky="nesw")
                       else
                           tkgrid(tkrowname,
                                  row = (j - 1) * rowspan + row_start_pos + title_pos,
                                  column= (i - 1)+ ylabel_pos,
                                  rowspan = rowspan,
                                  columnspan = 1,
                                  sticky="nesw")
                   }
               })
    }

    # pack title
    if(title != "") {

        title <- as.character(tcltk::tcl('label',
                                         as.character(l_subwin(parent,'title')),
                                         text = title,
                                         bg = l_getOption("canvas_bg_guides"),
                                         fg = labelForeground,
                                         borderwidth = labelBorderwidth,
                                         relief = "flat"))

        tkgrid(title,
               row = 0,
               column= col_start_pos + ylabel_pos,
               rowspan = title_span,
               columnspan = ncolsLabel * span,
               sticky="nesw")
    }

    # pack xlabel
    if(xlabel != "") {

        tkXlabel <- as.character(tcltk::tcl('label',
                                            as.character(l_subwin(parent,'xlabel')),
                                            text = xlabel,
                                            bg = l_getOption("canvas_bg_guides"),
                                            fg = labelForeground,
                                            borderwidth = labelBorderwidth,
                                            relief = "flat"))

        tkgrid(tkXlabel,
               row = len_rowsLabel + title_pos + nrowsLabel * span + len_colsLabel,
               column= col_start_pos + ylabel_pos,
               rowspan = 1,
               columnspan = ncolsLabel * span,
               sticky="nesw")

    }

    # pack ylabel
    if(ylabel != "") {

        tkYlabel <- as.character(tcltk::tcl('label',
                                            as.character(l_subwin(parent,'ylabel')),
                                            text = paste(paste0(" ", strsplit(ylabel, "")[[1]], " "), collapse = "\n"),
                                            bg = l_getOption("canvas_bg_guides"),
                                            fg = labelForeground,
                                            borderwidth = labelBorderwidth,
                                            relief = "flat"))

        tkgrid(tkYlabel,
               row = title_pos + row_start_pos,
               column= 0,
               rowspan = nrowsLabel * span,
               columnspan = 1,
               sticky="nesw")

    }

    # update ylabel (xlabel if swapAxes) when histograms change y shows
    if(inherits(plots[[1]], "l_hist")) {

        linkOneDimensionalStates(plots, oneDimensionalStates = "yshows")
        updateYshows(plots, swapAxes = swapAxes,
                     tkXlabel = tkXlabel,
                     tkYlabel = tkYlabel,
                     xlabel = xlabel,
                     ylabel = ylabel)
    }

    for (i in seq(ncolsLabel*span) - 1 + col_start_pos + ylabel_pos) {
        tkgrid.columnconfigure(parent, i, weight=1)
    }
    for (i in seq(nrowsLabel*span) - 1 + row_start_pos + title_pos) {
        tkgrid.rowconfigure(parent, i, weight=1)
    }

    if(new.toplevel)
        tkpack(parent, fill="both", expand=TRUE)

    # plots <- plots[plot_id]
    names(plots) <- new_names
    return(plots)
}

#' @title Layout as a wrap
#' @param plots A list of \code{loon} plots
#' @param subtitles The subtitles of the layout. It is a list and the length is equal to
#' the number of \code{by} variables. Each element in a list is the unique values of such \code{by} variable.
#' @param prop The proportion of the label height and widget height
#' @param parent a valid Tk parent widget path. When the parent widget is
#'   specified (i.e. not \code{NULL}) then the plot widget needs to be placed using
#'   some geometry manager like \code{\link{tkpack}} or \code{\link{tkplace}} in
#'   order to be displayed. See the examples below.
#' @param title The title of the widget
#' @param xlabel The xlabel of the widget
#' @param ylabel The ylabel of the widget
#' @param ncol The number of layout columns
#' @param nrow The number of layout rows
#' @param labelLocation Labels location.
#' \itemize{
#' \item {
#' Length two vector for layout \code{grid}.
#' The first one is used to determine the position of column labels ('top' or 'bottom').
#' The second one is used to determine the position of row labels ('right' or 'left').
#' }
#' \item {
#' Length one vector for layout \code{wrap}, 'top' or 'bottom'.
#' }
#' }
#' @param byrow Place widget by row or by column
#' @param swapAxes swap axes, \code{TRUE} or \code{FALSE}
#' @param labelBackground Label background color
#' @param labelForeground Label foreground color
#' @param labelBorderwidth Label border width
#' @param labelRelief Label relief
#' @param plotWidth default plot width (in pixel)
#' @param plotHeight default plot height (in pixel)
#' @param sep The character string to separate or combine a vector
#' @param maxCharInOneRow Max char in one row. If it exceeds the max, then a char will be displayed into two rows
#' @param new.toplevel determine whether the parent is a new top level.
#' If it is not a new window, the widgets will not be packed
#' @param ... named arguments to modify plot states.
#' See \code{\link{l_info_states}} of any instantiated l_plot for examples of names and values.
#'
facet_wrap_layout <- function(plots,
                              subtitles,
                              prop = 10,
                              parent = NULL,
                              title = "",
                              xlabel = "",
                              ylabel = "",
                              nrow = NULL,
                              ncol = NULL,
                              labelLocation = "top",
                              byrow = FALSE,
                              swapAxes = FALSE,
                              labelBackground = l_getOption("facetLabelBackground"),
                              labelForeground = l_getOption("foreground"),
                              labelBorderwidth = 2,
                              labelRelief = "ridge",
                              plotWidth = 200,
                              plotHeight = 200,
                              sep = "*",
                              maxCharInOneRow = 15,
                              new.toplevel = TRUE,
                              ...) {

    N <- length(plots)
    span <- prop
    if (is.null(nrow) && !is.null(ncol)) {
        nrow <- ceiling(N/ncol)
    }
    if (is.null(ncol) && !is.null(nrow)) {
        ncol <- ceiling(N/nrow)
    }
    stopifnot(nrow * ncol >= N)

    if (is.null(nrow) && is.null(ncol)) {
        nm <- grDevices::n2mfrow(N)
        nrow <- nm[2]
        ncol <- nm[1]
    }

    # ** respect to the number of columns **
    ## No matter rows, if the number of columns is larger than the number of plots,
    ## give a warning to modify the ncol
    if(ncol > N) {
        ncol <- N
        warning(paste("Too many spots to accommodate the plots.\nThe argument `ncol` will be modified to", ncol),
                call. = FALSE)
    }

    ## if the nrow is too large, then, to modify it to the minimum requirement.
    if(nrow > ceiling(N/ncol)) {

        nrow <- ceiling(N/ncol)
        warning(paste("Too many spots to accommodate the plots.\nThe argument `nrows` will be modified to", nrow),
                call. = FALSE)

    }

    if(swapAxes) {
        label <- xlabel
        xlabel <- ylabel
        ylabel <- label
    }

    by <- names(subtitles)
    len <- length(subtitles)

    # To simplify the settings of labels, we clarify that
    ## 1. title will be always on top
    ## 2. ylabel is always on left
    ## 3. xlabel is always on bottom
    title_span <- 2
    title_pos <- ifelse(title == "", 0, title_span)
    ylabel_pos <- ifelse(ylabel == "", 0, 1)


    # tk configure
    sapply(plots,
           function(p) {
               tkconfigure(paste(p,'.canvas',sep=''),
                           width = plotWidth,
                           height = plotHeight)
           }
    )
    plot_names <- names(plots)
    label_span <- len
    plots_span <- span - label_span
    if(plots_span <= 0) {
        span <- label_span + 10
        plots_span <- span - label_span
        warning("Span is too small. Automatically set span = ", span)
    }

    if(length(labelLocation) > 1) {
        warning("Layout 'wrap' only accepts one position")
        labelLocation <- labelLocation[1]
    }
    if(!labelLocation %in% c("top", "bottom")) {
        warning("labels can only be located at the 'top' or 'bottom' when layout is 'wrap'")
        labelLocation <- "top"
    }

    new_names <- c()
    split <- paste0("[", sep, "]")
    if(byrow) {

        for(j in seq(ncol)) {
            for(i in seq(nrow)) {

                plotid <- (j - 1) * nrow + i
                if(plotid > N) break()

                new_names <- c(new_names, paste0("x", i, "y", j))

                label <- strsplit(plot_names[plotid], split = split)[[1]]
                stopifnot(
                    length(label) == label_span
                )
                if(labelLocation == "top") {
                    # pack plots
                    tkgrid(plots[[plotid]],
                           row = (i - 1) * span + label_span + title_pos,
                           column = (j - 1) * plots_span + ylabel_pos,
                           rowspan = plots_span,
                           columnspan = plots_span,
                           sticky="nesw")
                    # pack labels
                    lapply(seq(length(label)),
                           function(k) {
                               l <- label[k]
                               name <- by[k]

                               text <- if(is.null(name)) {
                                   l
                               } else {
                                   ifelse(grepl("color", name),
                                          paste(name, color.id(l), sep = ":"),
                                          paste(name, l, sep = ":"))
                               }

                               tklabel <- as.character(tcltk::tcl('label',
                                                                  as.character(l_subwin(parent,
                                                                                        paste0('label-top-', 'x', i, 'y', j, 'p', k))),
                                                                  text = text,
                                                                  bg = labelBackground,
                                                                  fg = labelForeground,
                                                                  borderwidth = labelBorderwidth,
                                                                  relief = labelRelief))
                               tkgrid(tklabel,
                                      row = (i - 1) * span + (k - 1) + title_pos,
                                      column = (j - 1) * plots_span + ylabel_pos,
                                      rowspan = 1,
                                      columnspan = plots_span,
                                      sticky="nesw")

                           })
                } else if(labelLocation == "bottom") {

                    # pack plots
                    tkgrid(plots[[plotid]],
                           row = (i - 1) * span + title_pos,
                           column = (j - 1) * plots_span + ylabel_pos,
                           rowspan = plots_span,
                           columnspan = plots_span,
                           sticky="nesw")
                    # pack labels
                    lapply(seq(length(label)),
                           function(k) {
                               l <- label[k]
                               name <- by[k]

                               text <- if(is.null(name)) {
                                   l
                               } else {
                                   ifelse(grepl("color", name),
                                          paste(name, color.id(l), sep = ":"),
                                          paste(name, l, sep = ":"))
                               }

                               tklabel <- as.character(tcltk::tcl('label',
                                                                  as.character(l_subwin(parent,
                                                                                        paste0('label-bottom-', 'x', i, 'y', j, 'p', k))),
                                                                  text = text,
                                                                  bg = labelBackground,
                                                                  fg = labelForeground,
                                                                  borderwidth = labelBorderwidth,
                                                                  relief = labelRelief))
                               tkgrid(tklabel,
                                      row = (i - 1) * span + plots_span + k - 1 + title_pos,
                                      column = (j - 1) * plots_span + ylabel_pos,
                                      rowspan = 1,
                                      columnspan = plots_span,
                                      sticky="nesw")

                           })

                } else stop("Unknown 'labelLocation'. It can only be one of 'top' or 'bottom'.")
            }
        }

    } else {

        for(i in seq(nrow)) {
            for(j in seq(ncol)) {

                plotid <- (i - 1) * ncol + j
                if(plotid > N) break()

                new_names <- c(new_names, paste0("x", i, "y", j))

                label <- strsplit(plot_names[plotid], split = split)[[1]]
                stopifnot(
                    length(label) == label_span
                )

                if(labelLocation == "top") {
                    # pack plots
                    tkgrid(plots[[plotid]],
                           row = (i - 1) * span + label_span + title_pos,
                           column = (j - 1) * plots_span + ylabel_pos,
                           rowspan = plots_span,
                           columnspan = plots_span,
                           sticky="nesw")

                    # pack labels
                    lapply(seq(length(label)),
                           function(k) {
                               l <- label[k]
                               name <- by[k]

                               text <- if(is.null(name)) {
                                   l
                               } else {
                                   ifelse(grepl("color", name),
                                          paste(name, color.id(l), sep = ":"),
                                          paste(name, l, sep = ":"))
                               }

                               tklabel <- as.character(tcltk::tcl('label',
                                                                  as.character(l_subwin(parent,
                                                                                        paste0('label-top-', 'x', i, 'y', j, 'p', k))),
                                                                  text = text,
                                                                  bg = labelBackground,
                                                                  fg = labelForeground,
                                                                  borderwidth = labelBorderwidth,
                                                                  relief = labelRelief))
                               tkgrid(tklabel,
                                      row = (i - 1) * span + (k - 1) + title_pos,
                                      column = (j - 1) * plots_span+ ylabel_pos,
                                      rowspan = 1,
                                      columnspan = plots_span,
                                      sticky="nesw")

                           })
                } else if(labelLocation == "bottom") {

                    # pack plots
                    tkgrid(plots[[plotid]],
                           row = (i - 1) * span + title_pos,
                           column = (j - 1) * plots_span + ylabel_pos,
                           rowspan = plots_span,
                           columnspan = plots_span,
                           sticky="nesw")
                    # pack labels
                    lapply(seq(length(label)),
                           function(k) {
                               l <- label[k]
                               name <- by[k]

                               text <- if(is.null(name)) {
                                   l
                               } else {
                                   ifelse(grepl("color", name),
                                          paste(name, color.id(l), sep = ":"),
                                          paste(name, l, sep = ":"))
                               }

                               tklabel <- as.character(tcltk::tcl('label',
                                                                  as.character(l_subwin(parent,
                                                                                        paste0('label-bottom-', 'x', i, 'y', j, 'p', k))),
                                                                  text = text,
                                                                  bg = labelBackground,
                                                                  fg = labelForeground,
                                                                  borderwidth = labelBorderwidth,
                                                                  relief = labelRelief))
                               tkgrid(tklabel,
                                      row = (i - 1) * span + plots_span + k - 1 + title_pos,
                                      column = (j - 1) * plots_span + ylabel_pos,
                                      rowspan = 1,
                                      columnspan = plots_span,
                                      sticky="nesw")

                           })

                } else stop("Unknown 'labelLocation'. It can only be one of 'top' or 'bottom'.")
            }
        }

    }

    # pack title
    if(title != "") {

        title <- as.character(tcltk::tcl('label',
                                         as.character(l_subwin(parent,'title')),
                                         text = title,
                                         bg = l_getOption("canvas_bg_guides"),
                                         fg = labelForeground,
                                         borderwidth = labelBorderwidth,
                                         relief = "flat"))

        tkgrid(title,
               row = 0,
               column = ylabel_pos,
               rowspan = title_span,
               columnspan = ncol * span,
               sticky="nesw")
    }

    # pack xlabel
    if(xlabel != "") {

        tkXlabel <- as.character(tcltk::tcl('label',
                                            as.character(l_subwin(parent,'xlabel')),
                                            text = xlabel,
                                            bg = l_getOption("canvas_bg_guides"),
                                            fg = labelForeground,
                                            borderwidth = labelBorderwidth,
                                            relief = "flat"))

        tkgrid(tkXlabel,
               row = title_pos + nrow * span,
               column = ylabel_pos,
               rowspan = 1,
               columnspan = ncol * span,
               sticky="nesw")

    }

    # pack ylabel
    if(ylabel != "") {

        tkYlabel <- as.character(tcltk::tcl('label',
                                            as.character(l_subwin(parent,'ylabel')),
                                            text = paste(paste0(" ", strsplit(ylabel, "")[[1]], " "), collapse = "\n"),
                                            bg = l_getOption("canvas_bg_guides"),
                                            fg = labelForeground,
                                            borderwidth = labelBorderwidth,
                                            relief = "flat"))

        tkgrid(tkYlabel,
               row = title_pos,
               column= 0,
               rowspan = nrow * span,
               columnspan = 1,
               sticky="nesw")

    }

    # update ylabel (xlabel if swapAxes) when histograms change y shows
    if(inherits(plots[[1]], "l_hist")) {

        linkOneDimensionalStates(plots, oneDimensionalStates = "yshows")
        updateYshows(plots, swapAxes = swapAxes,
                     tkXlabel = tkXlabel,
                     tkYlabel = tkYlabel,
                     xlabel = xlabel,
                     ylabel = ylabel)
    }


    label_pos <- if(labelLocation == "top") {
        unlist(lapply(span * (seq(nrow) - 1), function(s) s + seq(label_span) - 1))
    } else {
        unlist(lapply(span * (seq(nrow) - 1), function(s) s + plots_span + seq(label_span) - 1))
    }

    for (i in setdiff(seq(nrow * span) - 1, label_pos) + title_pos) {
        tkgrid.rowconfigure(parent, i, weight=1)
    }
    for (i in seq(ncol * plots_span) - 1 + ylabel_pos) {
        tkgrid.columnconfigure(parent, i, weight=1)
    }

    if(new.toplevel)
        tkpack(parent, fill="both", expand=TRUE)

    names(plots) <- new_names
    return(plots)
}

#' @title layout separately
#' @param plots A list of \code{loon} plots
#' @param subtitles The subtitles of the layout. It is a list and the length is equal to
#' the number of \code{by} variables. Each element in a list is the unique values of such \code{by} variable.
#' @param title The title of the widget
#' @param xlabel The xlabel of the widget
#' @param ylabel The ylabel of the widget
#' @param sep The character string to separate or combine a vector
#' @param maxCharInOneRow Max char in one row. If it exceeds the max, then a char will be displayed into two rows
#' @param ... named arguments to modify plot states.
#' See \code{\link{l_info_states}} of any instantiated l_plot for examples of names and values.
#'
facet_separate_layout <- function(plots,
                                  subtitles,
                                  title = "",
                                  xlabel = "",
                                  ylabel = "",
                                  sep = "*",
                                  maxCharInOneRow = 15, # to be consistent
                                  ...) {

    # pack title
    by_names <- names(subtitles)
    split <- paste0("[", sep, "]")
    if(title == "") title <- NULL

    if(is.null(by_names)) {
        titles <- vapply(names(plots),
                         function(name) {
                             paste(c(title, strsplit(name, split = split)[[1]]), collapse = "\n")
                         }, character(1))
    } else {
        titles <- vapply(names(plots),
                         function(name) {

                             subtitle <- strsplit(name, split = split)[[1]]
                             paste(c(title, paste(by_names, subtitle, sep = ":")), collapse = "\n")
                         }, character(1))
    }

    # set labelMargins
    labelMargins <- list(...)$labelMargins

    if(is.null(labelMargins)) {
        t <- 1
        if(is.null(title)) t <- 0
        t <- t + length(subtitles)
        # 3 is the top label margin
        # default is 60
        labelMargins3 <- ifelse(t < 3, 60, t * 30)
    }


    # pack xlabel and ylabel
    lapply(seq(length(plots)),
           function(i) {
               p <- plots[[i]]

               if(inherits(p, "l_serialaxes")) {
                   p['title'] <- titles[i]
               } else {
                   p['xlabel'] <- xlabel
                   p['ylabel'] <- ylabel
                   p['title'] <- titles[i]
                   if(is.null(labelMargins))
                       p['labelMargins'][3] <- labelMargins3
               }

           })
}


color.id <- function(col) {

    vapply(col,
           function(color) {

               # real color
               if(!grepl("#", color)) return(color)

               # hex code color
               # hex12to6 will give warnings if the hex code is not 12
               # as_hex6color can accommodate 6 digits and 12 digits code
               tryCatch(
                   expr = {
                       color <- as_hex6color(color)
                       c2 <- grDevices::col2rgb(color)
                       coltab <- grDevices::col2rgb(colors())
                       cdist <- apply(coltab, 2, function(z) sum((z - c2)^2))
                       colors()[which(cdist == min(cdist))][1]
                   },
                   error = function(e) {
                       color
                   }
               )

           }, character(1))
}
