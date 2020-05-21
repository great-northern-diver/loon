
#' @title Get tags of the item below the mouse cursor
#'
#' @description Retrieves the tags of the visual item that at the time of the
#'   function evaluation is below the mouse cursor.
#'
#' @template param_widget
#'
#' @details
#' For more details see \code{l_help("learn_R_bind.html#item-bindings")}
#'
#' @return vector with item tags of visual
#'
#' @seealso \code{\link{l_bind_item}}, \code{\link{l_currentindex}}
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' printTags <- function(W) {
#'     print(l_currenttags(W))
#' }
#'
#' p <- l_plot(x=1:3, y=1:3, title='Query Visual Item Tags')
#'
#' l_bind_item(p, 'all', '<ButtonPress>', function(W)printTags(W))
#' }
l_currenttags <- function(widget) {
    l_throwErrorIfNotLoonWidget(widget)

    tags <- tcl(widget, 'currenttags')
    n <- as.numeric(tcl('llength', tags))
    vapply(0:(n-1), function(i) {
        paste(as.character(tcl('lindex', tags, i)), collapse = '')
       }, FUN.VALUE = character(1))
}


#' @title Get layer-relative index of the item below the mouse cursor
#'
#' @description Checks if there is a visual item below the mouse cursor and if
#'   there is, it returns the index of the visual item's position in the
#'   corresponding variable dimension of its layer.
#'
#' @template param_widget
#' @details
#' For more details see \code{l_help("learn_R_bind.html#item-bindings")}
#'
#' @return index of the visual item's position in the corresponding variable
#'   dimension of its layer
#'
#' @seealso \code{\link{l_bind_item}}, \code{\link{l_currenttags}}
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' p <- l_plot(iris[,1:2], color=iris$Species)
#'
#' printEntered <- function(W) {
#'     cat(paste('Entered point ', l_currentindex(W), '\n'))
#' }
#'
#' printLeave <- function(W) {
#'     cat(paste('Left point ', l_currentindex(W), '\n'))
#' }
#'
#' l_bind_item(p, tags='model&&point', event='<Enter>',
#'             callback=function(W) {printEntered(W)})
#'
#' l_bind_item(p, tags='model&&point', event='<Leave>',
#'             callback=function(W) {printLeave(W)})
#'
#' }
l_currentindex <- function(widget) {
    tags <- l_currenttags(widget)

    if (tags[1] == "layer") {
        index <- as.numeric(substring(tags[4],'5')) + 1
    } else {
        index <- -1
    }
    return(index)
}
