facet_grid_layout <- function(plots,
                              subtitles,
                              span = 10,
                              parent = NULL,
                              title = "",
                              xlabel = "",
                              ylabel = "",
                              column_labels_loc = "top",
                              row_labels_loc = "right",
                              byrow = TRUE,
                              label_background = "gray80", label_foreground = "black",
                              label_borderwidth = 2,
                              label_relief = "groove", ...) {

    by <- names(subtitles)
    len <- length(by)

    # tk configure
    sapply(plots,
           function(p) {
               tkconfigure(paste(p,'.canvas',sep=''),
                           width=100,
                           height=100)
           }
    )
    plot_names <- names(plots)

    # tk layout
    if(byrow) {
        rowsLabel <- subtitles[seq(len) %% 2 == 0]
        colsLabel <- subtitles[seq(len) %% 2 == 1]
    } else {
        rowsLabel <- subtitles[seq(len) %% 2 == 1]
        colsLabel <- subtitles[seq(len) %% 2 == 0]
    }

    nrowsLabel <- prod(lengths(rowsLabel))
    ncolsLabel <- prod(lengths(colsLabel))
    len_rowsLabel <- length(rowsLabel)
    len_colsLabel <- length(colsLabel)

    row_start_pos <- ifelse(column_labels_loc == "top", len_colsLabel, 0)
    col_start_pos <- ifelse(row_labels_loc == "right", 0, len_rowsLabel)

    # To simplify the settings of labels, we clarify that
    ## 1. title will be always on top
    ## 2. ylabel is always on left
    ## 3. xlabel is always on bottom
    title_span <- 2
    title_pos <- ifelse(title == "", 0, title_span)
    ylabel_pos <- ifelse(ylabel == "", 0, 1)

    layout_orders <- apply(do.call(expand.grid, lapply(c(rowsLabel, colsLabel), function(x) x)), 1, paste, collapse="*")


    get_plot_id <- function(string, matched_string) {
        plot_id <- which(
            sapply(strsplit(matched_string, split = "[*]"),
                   function(str) {

                       all(
                           vapply(string,
                                  function(s) {
                                      isIn <- s %in% str
                                      str[which(str %in% s)[1]] <- NA
                                      str <<- str
                                      isIn
                                  },
                                  logical(1))
                       )
                   })
        )
        if(length(plot_id) != 1) stop("The length of plot id is not equal to 1")
        plot_id
    }

    plot_id <- c()
    new_names <- c()
    for(i in seq(ncolsLabel)) {
        for(j in seq(nrowsLabel)) {
            id <- (i - 1) * nrowsLabel + j
            plotid <- get_plot_id(strsplit(layout_orders[id], split = "[*]")[[1]],
                                  plot_names)
            plot_id <- c(plot_id, plotid)
            new_names <- c(new_names, paste0("x", j, "y", i))
            tkgrid(plots[[plotid]],
                   row = (j - 1) * span + row_start_pos + title_pos, # leave space for labels
                   column = (i - 1) * span + col_start_pos + ylabel_pos,
                   rowspan = span,
                   columnspan = span,
                   sticky="nesw")

        }
    }

    # pack column labels
    if(len_colsLabel > 0) {
        colsLabel <- rev(colsLabel)
        fluid_colsLabel <- colsLabel
        column_names <- names(fluid_colsLabel)
        lapply(seq(length(column_names)),
               function(j) {
                   name <- column_names[j]
                   col <- if(j == 1) {
                       fluid_colsLabel[[name]]
                   } else {
                       rep(fluid_colsLabel[[name]], prod(lengths(colsLabel[1:(j - 1)])))
                   }
                   fluid_colsLabel[name] <<- NULL
                   columnspan <- prod(lengths(fluid_colsLabel)) * span
                   for(i in seq(length(col))) {
                       # row index
                       label <- col[i]
                       text <- ifelse(name == "color",
                                      paste(name, color.id(hex12tohex6(label))[1], sep = ":"),
                                      paste(name, label, sep = ":"))
                       tkcolname <- as.character(tcltk::tcl('label',
                                                            as.character(l_subwin(parent, 'label')),
                                                            text = text,
                                                            bg = label_background,
                                                            fg = label_foreground,
                                                            borderwidth = label_borderwidth,
                                                            relief = label_relief))
                       if(column_labels_loc == "top")
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
        row_names <- names(fluid_rowsLabel)
        lapply(seq(length(row_names)),
               function(i) {
                   name <- row_names[i]
                   row <- if(i == 1) {
                       fluid_rowsLabel[[name]]
                   } else {
                       rep(fluid_rowsLabel[[name]], prod(lengths(rowsLabel[1:(i - 1)])))
                   }
                   fluid_rowsLabel[name] <<- NULL
                   rowspan <- prod(lengths(fluid_rowsLabel)) * span
                   for(j in seq(length(row))) {
                       # row index
                       label <- row[j]
                       text <- ifelse(name == "color",
                                      paste(name, color.id(hex12tohex6(label))[1], sep = ":"),
                                      paste(name, label, sep = ":"))

                       tkrowname <- as.character(tcltk::tcl('label',
                                                            as.character(l_subwin(parent,'label')),
                                                            text = paste(paste0(" ", strsplit(text, "")[[1]], " "), collapse = "\n"),
                                                            bg = label_background,
                                                            fg = label_foreground,
                                                            borderwidth = label_borderwidth,
                                                            relief = label_relief))
                       if(row_labels_loc == "right")
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

    labelBG <- "grey70"
    # pack title
    if(title != "") {

        title <- as.character(tcltk::tcl('label',
                                         as.character(l_subwin(parent,'label')),
                                         text = title,
                                         bg = labelBG,
                                         fg = label_foreground,
                                         borderwidth = label_borderwidth,
                                         relief = "raised"))

        tkgrid(title,
               row = 0,
               column= col_start_pos + ylabel_pos,
               rowspan = title_span,
               columnspan = ncolsLabel * span,
               sticky="nesw")
    }

    # pack xlabel
    if(xlabel != "") {

        xlabel <- as.character(tcltk::tcl('label',
                                         as.character(l_subwin(parent,'label')),
                                         text = xlabel,
                                         bg = labelBG,
                                         fg = label_foreground,
                                         borderwidth = label_borderwidth,
                                         relief = "raised"))

        tkgrid(xlabel,
               row = len_rowsLabel + title_pos + nrowsLabel * span + len_colsLabel,
               column= col_start_pos + ylabel_pos,
               rowspan = 1,
               columnspan = ncolsLabel * span,
               sticky="nesw")

    }

    # pack ylabel
    if(ylabel != "") {

        ylabel <- as.character(tcltk::tcl('label',
                                          as.character(l_subwin(parent,'label')),
                                          text = paste(paste0(" ", strsplit(ylabel, "")[[1]], " "), collapse = "\n"),
                                          bg = labelBG,
                                          fg = label_foreground,
                                          borderwidth = label_borderwidth,
                                          relief = "raised"))

        tkgrid(ylabel,
               row = title_pos + row_start_pos,
               column= 0,
               rowspan = nrowsLabel * span,
               columnspan = 1,
               sticky="nesw")

    }
    for (i in seq(ncolsLabel*span) - 1 + col_start_pos + ylabel_pos) {
        tkgrid.columnconfigure(parent, i, weight=1)
    }
    for (i in seq(nrowsLabel*span) - 1 + row_start_pos + title_pos) {
        tkgrid.rowconfigure(parent, i, weight=1)
    }
    tkpack(parent, fill="both", expand=TRUE)

    plots <- plots[plot_id]
    names(plots) <- new_names
    return(plots)
}


facet_wrap_layout <- function(plots,
                              subtitles,
                              span = 10,
                              title = "",
                              xlabel = "",
                              ylabel = "",
                              parent = NULL,
                              nrow = NULL,
                              ncol = NULL,
                              labels_loc = "top",
                              byrow = FALSE,
                              label_background = "gray80", label_foreground = "black",
                              label_borderwidth = 2,
                              label_relief = "groove", ...) {

    N <- length(plots)

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

    by <- names(subtitles)
    len <- length(by)

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
                           width=100,
                           height=100)
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

    new_names <- c()
    if(byrow) {

        for(i in seq(nrow)) {
            for(j in seq(ncol)) {

                plotid <- (i - 1) * ncol + j
                if(plotid > N) break()

                new_names <- c(new_names, paste0("x", i, "y", j))

                label <- strsplit(plot_names[plotid], "[*]")[[1]]
                stopifnot(
                    length(label) == label_span
                )
                if(labels_loc == "top") {
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
                               text <- ifelse(name == "color",
                                              paste(name, color.id(hex12tohex6(l))[1], sep = ":"),
                                              paste(name, l, sep = ":"))

                               tklabel <- as.character(tcltk::tcl('label',
                                                                  as.character(l_subwin(parent, 'label')),
                                                                  text = text,
                                                                  bg = label_background,
                                                                  fg = label_foreground,
                                                                  borderwidth = label_borderwidth,
                                                                  relief = label_relief))
                               tkgrid(tklabel,
                                      row = (i - 1) * span + (k - 1) + title_pos,
                                      column = (j - 1) * plots_span+ ylabel_pos,
                                      rowspan = 1,
                                      columnspan = plots_span,
                                      sticky="nesw")

                           })
                } else if(labels_loc == "bottom") {

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
                               text <- ifelse(name == "color",
                                              paste(name, color.id(hex12tohex6(l))[1], sep = ":"),
                                              paste(name, l, sep = ":"))

                               tklabel <- as.character(tcltk::tcl('label',
                                                                  as.character(l_subwin(parent, 'label')),
                                                                  text = text,
                                                                  bg = label_background,
                                                                  fg = label_foreground,
                                                                  borderwidth = label_borderwidth,
                                                                  relief = label_relief))
                               tkgrid(tklabel,
                                      row = (i - 1) * span + plots_span + k - 2 + title_pos,
                                      column = (j - 1) * plots_span + ylabel_pos,
                                      rowspan = 1,
                                      columnspan = plots_span,
                                      sticky="nesw")

                           })

                } else stop("Unknown labels_loc. It can only be one of 'top' or 'bottom'.")
            }
        }

    } else {

        for(j in seq(ncol)) {
            for(i in seq(nrow)) {

                plotid <- (j - 1) * nrow + i
                if(plotid > N) break()

                new_names <- c(new_names, paste0("x", i, "y", j))

                label <- strsplit(plot_names[plotid], "[*]")[[1]]
                stopifnot(
                    length(label) == label_span
                )
                if(labels_loc == "top") {
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
                               text <- ifelse(name == "color",
                                              paste(name, color.id(hex12tohex6(l))[1], sep = ":"),
                                              paste(name, l, sep = ":"))

                               tklabel <- as.character(tcltk::tcl('label',
                                                                  as.character(l_subwin(parent, 'label')),
                                                                  text = text,
                                                                  bg = label_background,
                                                                  fg = label_foreground,
                                                                  borderwidth = label_borderwidth,
                                                                  relief = label_relief))
                               tkgrid(tklabel,
                                      row = (i - 1) * span + (k - 1) + title_pos,
                                      column = (j - 1) * plots_span + ylabel_pos,
                                      rowspan = 1,
                                      columnspan = plots_span,
                                      sticky="nesw")

                           })
                } else if(labels_loc == "bottom") {

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
                               text <- ifelse(name == "color",
                                              paste(name, color.id(hex12tohex6(l))[1], sep = ":"),
                                              paste(name, l, sep = ":"))

                               tklabel <- as.character(tcltk::tcl('label',
                                                                  as.character(l_subwin(parent, 'label')),
                                                                  text = text,
                                                                  bg = label_background,
                                                                  fg = label_foreground,
                                                                  borderwidth = label_borderwidth,
                                                                  relief = label_relief))
                               tkgrid(tklabel,
                                      row = (i - 1) * span + plots_span + k - 1 + title_pos,
                                      column = (j - 1) * plots_span + ylabel_pos,
                                      rowspan = 1,
                                      columnspan = plots_span,
                                      sticky="nesw")

                           })

                } else stop("Unknown labels_loc. It can only be one of 'top' or 'bottom'.")
            }
        }

    }

    # pack title
    if(title != "") {

        title <- as.character(tcltk::tcl('label',
                                         as.character(l_subwin(parent,'label')),
                                         text = title,
                                         bg = label_background,
                                         fg = label_foreground,
                                         borderwidth = label_borderwidth,
                                         relief = "raised"))

        tkgrid(title,
               row = 0,
               column = ylabel_pos,
               rowspan = title_span,
               columnspan = ncol * span,
               sticky="nesw")
    }

    # pack xlabel
    if(xlabel != "") {

        xlabel <- as.character(tcltk::tcl('label',
                                          as.character(l_subwin(parent,'label')),
                                          text = xlabel,
                                          bg = label_background,
                                          fg = label_foreground,
                                          borderwidth = label_borderwidth,
                                          relief = "raised"))

        tkgrid(xlabel,
               row = title_pos + nrow * span,
               column = ylabel_pos,
               rowspan = 1,
               columnspan = ncol * span,
               sticky="nesw")

    }

    # pack ylabel
    if(ylabel != "") {

        ylabel <- as.character(tcltk::tcl('label',
                                          as.character(l_subwin(parent,'label')),
                                          text = paste(paste0(" ", strsplit(ylabel, "")[[1]], " "), collapse = "\n"),
                                          bg = label_background,
                                          fg = label_foreground,
                                          borderwidth = label_borderwidth,
                                          relief = "raised"))

        tkgrid(ylabel,
               row = title_pos,
               column= 0,
               rowspan = nrow * span,
               columnspan = 1,
               sticky="nesw")

    }

    label_pos <- if(labels_loc == "top") {
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
    tkpack(parent, fill="both", expand=TRUE)

    names(plots) <- new_names
    return(plots)
}


color.id <- function(col) {
    c2 <- col2rgb(col)
    coltab <- col2rgb(colors())
    cdist <- apply(coltab, 2, function(z) sum((z - c2)^2))
    colors()[which(cdist == min(cdist))]
}
