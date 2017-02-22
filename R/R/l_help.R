

#' @title Open a browser with loon's local documentation webpage
#'   
#' @description The loon R package comes with a copy of loon's official webpage 
#'   at \url{http://waddella.github.io/loon/}. \code{l_help} opens a browser 
#'   with this page.
#'   
#' @param page relative path to a page, the .html part may be omitted
#' @param viewer blooean, open help page in RStuidio viewer pane if possible
#' @param ... arguments forwarded to browseURL, e.g. to specify a browser
#' 
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' l_help()
#' l_help("learn_R_display_hist")
#' l_help("learn_R_bind")
#' # jump to a section
#' l_help("learn_R_bind.html#list-reorder-delete-bindings")
#' }
l_help <- function(page="index", viewer=TRUE, ...) {
    
    if(!grepl(".html#", page, fixed = TRUE)) {
        if(tools::file_ext(page) == "")
            page <- paste0(page, ".html")
    }
    site <- file.path(find.package(package = 'loon'), "website", "html", page)
    
    fviewer <- getOption("viewer")
    if (viewer && !is.null(fviewer)) {
        tdir <- tempdir()
        ltdir <- file.path(tdir, 'loon_help')
        if(!dir.exists(ltdir)) {
            dir.create(ltdir)
            file.copy(dirname(site), ltdir, recursive = TRUE)
        }
        fviewer(file.path(ltdir, 'html', page))
    } else {
        browseURL(site, ...)
    }
}