#' @title Import Greyscale Images as Tcl images from an Array
#'   
#' @description Import image grayscale data (0-255) with each image saved as a
#'   row or column of an array.
#'   
#' @param array of 0-255 grayscale value data.
#' @param width of images in pixels.
#' @param height of images in pixels.
#' @param img_in_row logical, TRUE if every row of the array represents an image
#' @param invert logical, for 'invert=FALSE' 0=withe, for 'invert=TRUE' 0=black
#' @param rotate the image: one of 0, 90, 180, or 270 degrees.
#'   
#' @details Images in tcl are managed by the tcl interpreter and made accessible
#'   to the user via a handle, i.e. a function name of the form image1, image2, 
#'   etc.
#'   
#' @templateVar page learn_R_display_plot
#' @templateVar section images
#' @template see_l_help
#'   
#' @return vector of image object names
#'   
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' # see
#' demo("l_ng_images_frey_LLE")
#' }
l_image_import_array <- function(array, width, height,
                                  img_in_row = TRUE, invert = FALSE,
                                  rotate = 0) {
    
    npix <- width*height
    if(dim(array)[ifelse(img_in_row,2,1)] != npix) {
        stop('Data dimension does not match with the width and height argument!')
    }
    
    if (rotate == 0) {
        ii <- 1:npix
        byrow = TRUE
        img_w <- width
        img_h <- height
    } else if (rotate == 90) {
        ii <- 1:npix
        byrow = FALSE
        img_h <- width
        img_w <- height
    } else if (rotate == 180) {
        ii <- npix:1
        byrow = TRUE
        img_w <- width
        img_h <- height
    } else if (rotate == 270) {
        ii <- npix:1
        byrow = FALSE
        img_h <- width
        img_w <- height
    } else {
        stop('Argument rotat must be either 0, 90, 180 or 270!')
    }
    
    ## invert
    inv <- ifelse(invert,1,0)
    sign <- ifelse(invert,-1,1)
    
    images <- apply(array,
                    ifelse(img_in_row,1,2),
                    function(img_data) {
                        im <- tkimage.create('photo',
                                             width = img_w, height = img_h,
                                             palette = '256/256/+256')
                        tcl(im,'put',
                            paste(rbind('{',
                                        matrix(grDevices::grey((inv + sign * img_data/255)[ii]),
                                               ncol = img_h, byrow = byrow),'}'),
                                  collapse = ' ')
                            )
                        return(im)
                    })
    
    ids <- sapply(images,function(img){tclvalue(img)})
    
    return(structure(ids, class = 'tkimage'))
}
