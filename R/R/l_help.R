

#' @title Open a browser with loon's combined (TCL and R) documentation website
#'
#' @description \code{l_help} opens a browser with the relevant page on the
#'   official combined loon documentation website at
#'   \url{https://great-northern-diver.github.io/loon/}.
#'
#' @param page relative path to a page, the .html part may be omitted
#' @param ... arguments forwarded to browseURL, e.g. to specify a browser
#'
#' @importFrom utils browseURL
#' @export
#'
#' @seealso \code{\link{help}}, \code{\link{l_web}} for \code{R} manual or
#' web \code{R} manual
#' @examples
#'
#' \dontrun{
#' l_help()
#' l_help("learn_R_intro")
#' l_help("learn_R_display_hist")
#' l_help("learn_R_bind")
#' # jump to a section
#' l_help("learn_R_bind.html#list-reorder-delete-bindings")
#' }
l_help <- function(page="index", ...) {

    if(!grepl(".html#", page, fixed = TRUE)) {
        if(tools::file_ext(page) == "")
            page <- paste0(page, ".html")
    }

    browseURL(paste0("https://great-northern-diver.github.io/loon/l_help/",
                     page), ...)

}
