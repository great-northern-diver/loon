
#' @rdname loonGrob
#' 
#' @examples 
#' 
#' ## l_pairs (scatterplot matrix) examples
#' 
#' p <- l_pairs(iris[,-5], color=iris$Species)
#' 
#' library(grid)
#' lgrob <- loonGrob(p)
#' grid.newpage()
#' grid.draw(lgrob)
#' 
#' @export

loonGrob.l_pairs <- function(target, name = NULL, gp = NULL, vp = NULL){
    
    widget <- target
    len_pairs <- length(widget)
    names <- c( sapply(widget, function(l){
        l['ylabel']
    }), sapply(widget, function(l){
        l['xlabel']
    }) )
    names <- unique(names)
    len_names <- length(names)
    textGrobId <- c()
    for(i in 1:len_names){
        if(i == 1){
            textGrobId[i] <- i
        }else{
            textGrobId[i] <- textGrobId[i - 1] + len_names + 1 - i
        }
    }
    grobId <- c(1:len_pairs, textGrobId - 0.5)
    lenGrobs <- len_pairs + len_names
    lgrobObject <- lapply(1:lenGrobs, function(i){
        if(i <= len_pairs){
            widgeti <- widget[[i]]
            widgeti['foreground'] <- "white"
            widgeti['minimumMargins'] <- rep(2,4)
            loonGrob(widgeti)
        }else{
            textGrob(names[i - len_pairs], gp = gpar(fontsize = 9))
        }
    })
    lgrobObject <- lgrobObject[order(grobId)]
    # layout matrix
    layout_matrix <- matrix(rep(NA, len_names^2), nrow = len_names)
    seq_len <- seq(lenGrobs)
    for(i in 1:len_names){
        layout_matrix[i, i:len_names] <- seq_len[1: (len_names + 1 - i)]
        seq_len <- seq_len[- c(1:(len_names + 1 - i) )]
    }
    # to generate layout matrix
    # 1  2  3  4  5
    # NA 6  7  8  9
    # NA NA 10 11 12
    # NA NA NA 13 14
    # NA NA NA NA 15
    backgroundCol <- "#EBEBEB"   # TODO grab this from the canvas or widget background
    gTree(
        children = gList(
            rectGrob(gp  = gpar(fill = backgroundCol, col = NA)),
            gridExtra::arrangeGrob(grobs = lgrobObject, 
                                   layout_matrix = layout_matrix,
                                   name = "l_pairs")
        ),
        name = name, gp = gp, vp = vp
    )
}
