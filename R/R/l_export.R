

#' @export
l_export <- function(widget, filename, width, height) {

    if (missing(width))
        width <- l_size(widget)[1]
    if (missing(height))
        height <- l_size(widget)[2]

    valid_extensions <- l_export_valid_formats() 
    
    file_extension <- tools::file_ext(filename)
    if (file_extension %in% valid_extensions) {
        fname <- as.character(tcl("::loon::export",
                                  widget, filename, width, height))
    } else {
        stop(paste0("The file fomat '",
                   file_extension,
                   "' is not supported on your system.\nSupported formats are: ",
                   paste(valid_extensions, collapse=', '), '.'))
    }

    fname
}

#' @export
l_export_valid_formats <- function() {
    valid_extensions <- c("ps", "eps")
    
    if (Sys.which('epstopdf') != "") {
        valid_extensions <- c(valid_extensions, "pdf")
    }

    if (.withTclImg) {
        valid_extensions <- c(valid_extensions,
                              'jpg','jpeg','png','bmp','tiff','gif')
    }
    valid_extensions
}



filetypes <- list(
    png="Portable Network Graphics",
    jpg="JPEG",
    ps="Postscript",
    eps="Encapsulated Postscript",
    pdf="Portable Document Graphics",
    tiff="Tagged Image File Format",
    bmp="Bitmap",
    gif="Graphics Interchange Format"
    )                  

exportImageDialog <- function(widget) {

    fm <- l_export_valid_formats()
    formats <- fm[-which(fm=='jpeg')]
    fnames <- vapply(formats, function(x)filetypes[[x]], character(1))

    types <- ""
    for (i in seq_along(formats)) {
        types <- paste(types, '{',
                       paste0('{', fnames[i], '}'),
                       paste0('{','.', formats[i], '}'),
                       '}')        
    }
    
    fileName <- as.character(
        tcl('tk_getSaveFile',
            initialdir=getwd(),
            initialfile='loon_plot',
            title="Export Plot As Image",
            parent=tkwinfo('toplevel', widget),
            filetypes=types))
    
    if (length(fileName) == 1) {
        l_export(widget, fileName)
    }
    return(fileName)
}
