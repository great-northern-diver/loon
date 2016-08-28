#' @examples 
#' set.seed(500)
#' x <- rnorm(30)
#' y <- 4 + 3*x + rnorm(30)
#' fit <- lm(y~x)
#' xseq <- seq(min(x)-1, max(x)+1, length.out = 50)
#' fit_line <- predict(fit, data.frame(x=range(xseq)))
#' ci <- predict(fit, data.frame(x=xseq), 
#'               interval="confidence", level=0.95)
#' pi <- predict(fit, data.frame(x=xseq),
#'               interval="prediction", level=0.95)
#' 
#' 
#' p <- l_plot(y~x, color='black', showScales=TRUE, showGuides=TRUE)
#' gLayer <- l_layer_group(
#'     p, label="simple linear regression",
#'     parent="root", index="end"
#' )
#' fitLayer <- l_layer_line(
#'     p, x=range(xseq), y=fit_line, color="#04327F",
#'     linewidth=4, label="fit", parent=gLayer
#' )
#' ciLayer <- l_layer_polygon(
#'     p,
#'     x = c(xseq, rev(xseq)), 
#'     y = c(ci[,'lwr'], rev(ci[,'upr'])),
#'     color = "#96BDFF", linecolor="",
#'     label = "95 % confidence interval",
#'     parent = gLayer, index='end'
#' )
#' piLayer <- l_layer_polygon(
#'     p,
#'     x = c(xseq, rev(xseq)), 
#'     y = c(pi[,'lwr'], rev(pi[,'upr'])),
#'     color = "#E2EDFF", linecolor="",
#'     label = "95 % prediction interval",
#'     parent = gLayer, index='end'
#' )
#' 
#' l_info_states(piLayer)

