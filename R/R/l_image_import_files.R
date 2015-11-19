## TODO: write R functions to import jpgs and pngs if Img tcl extension could not be loaded
#' @export
l_image_import_files <- function(paths) {

    if (!.withTclImg) {
        stop("The l_image_import_files function requires the Img tcl extension to be loaded")
    }
    
    unique_paths <- unique(paths)
    unique_ids <- sapply(unique_paths, function(path) {
        tclvalue(tkimage.create('photo', file = path))	
    })
    ii <- match(paths,unique_paths)
    
    return(unique_ids[ii])
}
