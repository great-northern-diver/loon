#' @title Display Tcl Images in a Simple Image Viewer
#'
#' @description
#'
#' Loon provides a simple image viewer to browse through the specified
#' tcl image objects.
#'
#' @param tclimages Vector of tcl image object names.
#'
#' @description
#'
#' The simple GUI supports either the use of the mouse or left and
#' right arrow keys to switch the images to the previous or next image
#' in the specified image vector.
#'
#' The images are resized to fill the viewer window.
#'
#' @return the \code{tclimages} vector is returned
#' 
#' @export
#'
#' @examples
#'
#' img2 <- tkimage.create('photo', width=200, height=150)
#' tcl(img2, 'put', 'yellow', '-to', 0, 0, 199, 149)
#' tcl(img2, 'put', 'green', '-to', 40, 20, 130, 40)
#' img3 <- tkimage.create('photo', width=500, height=100)
#' tcl(img3, 'put', 'orange', '-to', 0, 0, 499, 99)
#' tcl(img3, 'put', 'green', '-to', 40, 80, 350, 95)
#' 
#' l_imageviewer(c(tclvalue(img2), tclvalue(img3)))

l_imageviewer <- function(tclimages) {
    tcl("::loon::imageviewer", as.character(tclimages))
    return(tclimages)
}
