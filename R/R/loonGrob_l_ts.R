#' @export

loonGrob.l_ts <- function(widget){
  
  lenGrobs <- length(widget)
  
  margins <- c(2.1, 5.1, 0.1, 1.1)
  
  loonGrobObject <- lapply(1:lenGrobs, function(i){
    if(i == 1) {
      margins[3] <- 3.1
    } else if(i == 4) {
      margins[1] <- 4.1
    }
    loonGrob(widget[[i]], margins = margins)
  })
  
  gridExtra::arrangeGrob(grobs = loonGrobObject, nrow=4, 
                         heights = c(1.3,1,1,1.3), name = "loon_ts")

}

