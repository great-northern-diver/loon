#' @rdname loonGrob
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Time series decomposition examples
#'
#' decompose <- decompose(co2)
#' # or decompose <- stl(co2, "per")
#' p <- l_plot(decompose, title = "Atmospheric carbon dioxide over Mauna Loa")
#'
#' # To print directly use either
#' plot(p)
#' # or
#' grid.loon(p)
#' # or to save structure
#' lgrob <- loonGrob(p)
#' grid.newpage()
#' grid.draw(lgrob)
#' }
#'
#' @export
loonGrob.l_ts <- function(target, name = NULL, gp = NULL, vp = NULL){
    loonGrob.l_compound(target,
                        name = if (!is.null(name)) name else "time series decomposition",
                        gp = gp, vp = vp)
}


