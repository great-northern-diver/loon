#' @examples 
#' # binding for when plot is resized 
#' p <- l_plot(iris[,1:2], color=iris$Species)
#' 
#' printSize <- function(p) {
#'     size <- l_size(p)
#'     cat(paste('Size of widget ', p, ' is: ',
#'               size[1], 'x', size[2], ' pixels\n', sep=''))    
#' }
#' 
#' l_bind_canvas(p, event='<Configure>', function(W) {printSize(W)})
#' 
#' id <- l_bind_canvas_ids(p)
#' id
#' 
#' l_bind_canvas_get(p, id)
