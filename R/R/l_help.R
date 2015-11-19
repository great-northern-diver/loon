
#' @export

l_help <- function(browser= getOption("browser")) {
    browseURL(file.path(find.package(package = 'loon'), "website", "html", "index.html"),
              browser=browser)
}
