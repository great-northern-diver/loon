
#' @rdname loonGrob
#' 
#' 
#' @examples
#'  
#' \dontrun{
#' ## Time series decomposition examples
#' 
#' decompose <- decompose(co2) 
#' # or decompose <- stl(co2, "per")
#' p <- l_plot(decompose, title = "Atmospheric carbon dioxide over Mauna Loa")
#' 
#' library(grid)
#' lgrob <- loonGrob(p)
#' grid.newpage()
#' grid.draw(lgrob)
#' }
#' 
#' @export


loonGrob.l_ts <- function(target, name = NULL, gp = NULL, vp = NULL){

    widget <- target
    lgrobObject <- lapply(seq_len(length(widget)), 
                          function(i){loonGrob(widget[[i]])
                          })
    gTree(children = gList(gridExtra::arrangeGrob(grobs = lgrobObject, 
                                                  nrow=4, 
                                                  heights = c(1.3,1,1,1), 
                                                  name = "loon_ts")
    ),
    name = name, vp = vp , gp = gp)        
}

