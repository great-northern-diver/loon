

#' @title Export a loon plot as an image
#'   
#' @description The supported image formats are dependent on the system 
#'   environment. Plots can always be exported to the Postscript format. 
#'   Exporting displays as .pdfs is only possible when the command line tool 
#'   epstopdf is installed. Finally, exporting to either png, jpg, bmp, tiff or 
#'   gif requires the Img Tcl extension. When choosing one of the formats that 
#'   depend on the Img extension, it is possible to export any Tk widget as an 
#'   image including inspectors.
#'   
#' @template param_widget
#' @param filename path of output file
#' @param width image width in pixels
#' @param height image height in pixels
#'   
#'   
#' @details Note that the \code{CTRL-T} key combination opens a dialog to export
#'   he graphic.
#'   
#'   The native export format is to \code{ps} as this is what the Tk canvas 
#'   offers. If the the \code{l_export} fails with other formats then please
#'   resort to a screen capture method for the moment.
#'   
#' @return path to the exported file
#'   
#' @seealso \code{\link{l_export_valid_formats}}
#'   
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

#' @title Return a list of the available image formats when exporting a loon 
#'   plot
#'   
#'   
#' @description The supported image formats are dependent on the system 
#'   environment. Plots can always be exported to the Postscript format. 
#'   Exporting displays as .pdfs is only possible when the command line tool 
#'   epstopdf is installed. Finally, exporting to either png, jpg, bmp, tiff or 
#'   gif requires the Img Tcl extension. When choosing one of the formats that 
#'   depend on the Img extension, it is possible to export any Tk widget as an 
#'   image including inspectors.
#'   
#' @return a vector with the image formats available for exporting a loon plot.
#'   
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
