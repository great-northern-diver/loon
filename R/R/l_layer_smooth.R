
l_layer.loess <- function(widget, x, index="0", ...) {
    l_layer_line(widget, x = x$x, y=x$fitted, label=deparse(x$call), index=index, ...)
}


#' @title layer a 2d smooth
#' 
#' 
#' @examples 
#' 
#' p <- l_plot(iris)
#' 
#' l_layer_smooth(p)
#' 
#' l_layer_smooth(p, method="lm", formula=y~x+x^2+2)
#' 
#' 
#' fit <- lm(dist ~ speed, data=cars)
#'
#' head(predict(fit, interval=c("confidence", "prediction")))
#' 
#' 
l_layer_smooth <- function(
    widget,
    method=c("lm", "glm", "gam", "loess", "rlm"),
    formula=y~x, # formula of x only!
    loon.args=list(fit, confidence),
    ...
) {
    
    x <- l_cget(widget, "x")
    y <- l_cget(widget, "y")
    
    method <- match.arg(method)
    
    args <- list(...)
    
    fit <- switch(
        method,
        lm = {
            lm(formula)
        }, 
        glm = {
            
        },
        gam = {
            
        },
        loess = {
            
        },
        rlm = {
            
        }
    )
    
    
    
    do.call("l_layer", c(widget, predict(fit), loon.args))
}
