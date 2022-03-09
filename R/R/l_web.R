

#' @title Open a browser with loon's R documentation webpage
#'
#' @description \code{l_web} opens a browser with the relevant page on the
#'   official loon documentation website.  This is constructed by joining together
#'   the information provided by the arguments \code{site}/\code{package}/\code{directory}/\code{page}.
#'
#'   Default would be the documentation found at
#'   \url{https://great-northern-diver.github.io/loon/}.
#'
#' @param page relative path to a page (the ".html" part may be omitted)
#' @param directory if \code{"home"} (the default) then \code{page} is ignored and the
#' browser will open at the home page of the official documentation website.  If \code{page} refers to a \code{package}
#'   manual reference, then directory must be \code{"reference"}; if \code{page} refers to the name
#'   of a vignette file, then directory should be \code{"articles"}
#' @param package a string identifying the package name having an online documentation
#'        (default \code{"loon"}).
#' @param site the URL of the site (default \code{"https://great-northern-diver.github.io"}) prefixing
#'  the path to the requested documentation.
#' @param ... arguments forwarded to \code{browseURL()}, e.g. to specify a browser
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
#' # or
#' l_web(package = "loon.data", directory = "reference")
#' #
#' help(l_hist)
#' l_web(page = "l_hist", directory = "reference")
#' }
l_web <- function(page = "index",
                  directory = c("home", "reference", "articles"),
                  package = c("loon", "loon.data", "loon.ggplot",
                              "loon.tourr", "ggmulti", "zenplots",
                              "loon.shiny", "diveR"),
                  site = "https://great-northern-diver.github.io",
                  ...) {
    pkg = match.arg(package)
    dir <- match.arg(directory)
    loonSite <- file.path(site, pkg, fsep = "/")
    if (dir == "home") {
        webpage <- file.path(loonSite, "index.html", fsep = "/")
    } else {
        if(!grepl(".html#", page, fixed = TRUE)) {
            if(tools::file_ext(page) == "")
                page <- paste0(page, ".html")}
        loonSite <- file.path(loonSite, dir, page, fsep = "/")
    }


    browseURL(loonSite, ...)

}
