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
#' loonGrob <- loon2grid(p)
#' grid.newpage()
#' grid.draw(loonGrob)


loon2grid.l_ts <- function(widget){
  
  lenGrobs <- length(widget)
  
  margins <- c(2.1, 5.1, 0.1, 1.1)
  
  loonGrobObject <- lapply(1:lenGrobs, function(i){
    if(i == 1) {
      margins[3] <- 3.1
    } else if(i == 4) {
      margins[1] <- 4.1
    }
    loon2grid(widget[[i]], margins = margins)
  })
  
  gridExtra::arrangeGrob(grobs = loonGrobObject, nrow=4, 
                         heights = c(1.3,1,1,1.3), name = "loon_ts")

}

