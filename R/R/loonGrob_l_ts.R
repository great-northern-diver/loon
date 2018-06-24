#' Create a time series grob
#' 
#' @param widget an object of class inheriting from "l_ts"
#' 
#' @return a grid grob
#' 
#' @import grid
#' 
#' @export
#' 
#' @examples
#'  
#' decompose <- decompose(co2) 
#' # or decompose <- stl(co2, "per")
#' p <- l_plot(decompose, title = "Atmospheric carbon dioxide over Mauna Loa")
#' 
#' library(grid)
#' lgrob <- loonGrob(p)
#' grid.newpage()
#' grid.draw(lgrob)


loonGrob.l_ts <- function(widget, name = name, gp = gp, vp = vp){

  lgrobObject <- lapply(seq_len(length(widget)), function(i){
    loonGrob(widget[[i]])
  })
  
  gridExtra::arrangeGrob(grobs = lgrobObject, nrow=4, 
                         heights = c(1.3,1,1,1.3), name = "loon_ts")

}

