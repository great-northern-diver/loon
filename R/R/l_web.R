

#' @title Open a browser with loon's R documentation webpage
#'
#' @description \code{l_web} opens a browser with the relevant page on the
#'   official loon documentation website at
#'   \url{https://great-northern-diver.github.io/loon/}.
#'
#' @param page relative path to a page, the .html part may be omitted
#' @param directory if \code{"home"} then \code{page} is ignored and the
#' browser will open at the home page of the official loon documentation website at
#'   \url{https://great-northern-diver.github.io/loon/}.  If \code{page} refers to a \code{loon}
#'   manual reference, then directory must be \code{"reference"}; if \code{page} refers to the name
#'   of a vignette file, then directory should be \code{"articles"}
#' @param ... arguments forwarded to browseURL, e.g. to specify a browser
#'
#' @importFrom utils browseURL
#' @export
#'
#' @seealso \code{\link{l_help}},\code{\link{help}}, \code{\link{vignette}}
#' @examples
#'
#' \dontrun{
#' l_web()
#' #
#' vignette("introduction", package = "loon")
#' # or
#' l_web(page = "introduction", directory = "articles")
#' #
#' help(l_hist)
#' l_web(page = "l_hist", directory = "reference")
#' }
l_web <- function(page="index", directory = c("home", "reference", "articles"),  ...) {
    dir <- match.arg(directory)
    loonSite <- "https://great-northern-diver.github.io/loon/"
    if (dir == "home") {
        webpage <- paste0(loonSite,"index.html")
    } else {
        if(!grepl(".html#", page, fixed = TRUE)) {
            if(tools::file_ext(page) == "")
                page <- paste0(page, ".html")
        }
        loonSite <- paste0("https://great-northern-diver.github.io/loon/",
                           dir, "/", page)
    }


    browseURL(loonSite, ...)

}
