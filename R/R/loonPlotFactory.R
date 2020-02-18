loonPlotFactory <- function(factory_tclcmd, factory_path, factory_window_title="loon plot", parent=NULL, ...) {

    new.toplevel <- FALSE
    if(is.null(parent)) {
        new.toplevel <- TRUE
        parent <- l_toplevel()
    }

    child <- l_subwin(parent, factory_path)
    plot <- try(tcl(factory_tclcmd, child, ...))

    if(is(plot, 'try-error')) {
        if(new.toplevel) tkdestroy(parent)
        stop(paste0(factory_window_title, " could not be created."))
    }

    if(new.toplevel) {
        tkpack(plot,  fill="both", expand=TRUE)
        tktitle(parent) <- paste(factory_window_title, "--path:", plot)
        ## Bind the toplevel <FocusIn> event to update the loon inspector
        tcl("bind", parent, "<FocusIn>",
            paste0("+::loon::setLoonInspectorActiveWidget ", plot))

        ## Bind Ctrl-P to export image
        tcl("bind", parent, "<Control-KeyPress-p>",
            function()exportImageDialog(plot))

        tcl("bind", parent, "<Control-KeyPress-P>",
            function()exportImageDialog(plot))

    }

    plot <- as.character(plot)

    class(plot) <- "loon"

    return(plot)
}
