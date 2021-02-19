#' @title Layer a smooth line for \code{loon}
#' @description Display a smooth line layer
#' @param widget widget path name as a string
#' @param x The \code{x} coordinates of line. If it is not provided, \code{x} will be inherited from widget
#' @param y The \code{y} coordinates of line. If it is not provided, \code{y} will be inherited from widget
#' @param method Smoothing method (function) to use, accepts either a character vector,
#' e.g. "lm", "glm", "loess" or a function, e.g. MASS::rlm or mgcv::gam, stats::lm, or stats::loess.
#' @param group Data can be grouped by n dimensional aesthetics attributes, e.g. "color", "size".
#' In addition, any length n vector or data.frame is accommodated.
#' @param formula Formula to use in smoothing function, eg. y ~ x, y ~ poly(x, 2), y ~ log(x)
#' @param interval type of interval, could be "none", "confidence" or "prediction" (not for \code{glm})
#' @param n Number of points at which to evaluate smoother.
#' @param span Controls the amount of smoothing for the default \code{loess} smoother.
#' Smaller numbers produce wigglier lines, larger numbers produce smoother lines.
#' @param level Level of confidence interval to use (0.95 by default).
#' @param methodArgs List of additional arguments passed on to the modelling function defined by method.
#' @param linecolor fitted line color.
#' @param linewidth fitted line width
#' @param linedash fitted line dash
#' @param confidenceIntervalArgs the line color, width and dash for confidence interval
#' @param predictionIntervalArgs the line color, width and dash for prediction interval
#' @param label label used in the layers inspector
#' @param parent group layer
#' @param index index of the newly added layer in its parent group
#' @param ... additional state initialization arguments, see \code{\link{l_info_states}}
#'
#' @export
#'
#' @examples
#' if(interactive()) {
#' # loess fit
#' p <- l_plot(iris, color = iris$Species)
#' l1 <- l_layer_smooth(p, interval = "confidence")
#' l_layer_hide(l1)
#'
#' # the fits are grouped by points color
#' l2 <- l_layer_smooth(p, group = "color",
#'                      method = "lm")
#'
#' # so far, all intervals are hidden
#' ls <- l_layer_getChildren(l2)
#' intervals <- l_layer_getChildren(l_create_handle(c(p,ls[3])))
#' ci <- l_create_handle(c(p,intervals[3]))
#' l_layer_show(ci)
#' # show prediction interval
#' pi <- l_create_handle(c(p,intervals[2]))
#' l_layer_show(pi)
#' # hide all
#' l_layer_hide(l2)
#'
#' # Draw a fitted line based on a new data set
#' shortSepalLength <- (iris$Sepal.Length < 5)
#' l3 <- l_layer_smooth(p,
#'                      x = iris$Sepal.Length[shortSepalLength],
#'                      y = iris$Sepal.Width[shortSepalLength],
#'                      method = "lm",
#'                      linecolor = "firebrick",
#'                      interval = "prediction")
#' l_layer_hide(l3)
#'
#' if(require(mgcv)) {
#'   # a full tensor product smooth
#'   ## linecolor is the same with the points color
#'   l4 <- l_layer_smooth(p,
#'                        method = "gam",
#'                        formula = y~te(x))
#'   l_layer_hide(l4)
#' }
#'
#' # facets
#' fp <- l_facet(p, by = iris$Species, inheritLayers = FALSE)
#' l5 <- l_layer_smooth(fp, method = "lm")
#'
#' # generalized linear model
#' if(require("loon.data")) {
#'   data("SAheart")
#'   # logit regression
#'   chd <- as.numeric(SAheart$chd) - 1
#'   age <- SAheart$age
#'   p1 <- l_plot(age, chd,
#'                title = "logit regression")
#'   gl1 <- l_layer_smooth(p1,
#'                         method = "glm",
#'                         methodArgs = list(family = binomial()),
#'                         interval = "conf")
#'
#'   # log linear regression
#'   counts <- c(18,17,15,20,10,20,25,13,12)
#'   age <- c(40,35,53,46,20,33,48,25,23)
#'   p2 <- l_plot(age, counts,
#'                title = "log-linear regression")
#'   gl2 <- l_layer_smooth(p2,
#'                         method = "glm",
#'                         methodArgs = list(family = poisson()),
#'                         interval = "conf")
#' }
#' }

l_layer_smooth <- function(widget, x = NULL, y = NULL, method = "loess", group = "",
                           formula = y ~ x, interval = c("none","confidence","prediction"),
                           n = 80, span = 0.75, level = 0.95,
                           methodArgs = list(), linecolor="steelblue", linewidth=2, linedash = "",
                           confidenceIntervalArgs = list(linecolor="gray80", linewidth=4, linedash = ""),
                           predictionIntervalArgs = list(linecolor="gray50", linewidth=3, linedash = 1),
                           label="smooth", parent="root", index=0, ...) {

  UseMethod("l_layer_smooth")
}

#' @export
l_layer_smooth.default <- function(widget, x = NULL, y = NULL, method = "loess", group = "",
                                   formula = y ~ x, interval = c("none","confidence","prediction"),
                                   n = 80, span = 0.75, level = 0.95,
                                   methodArgs = list(), linecolor="steelblue", linewidth=2, linedash = "",
                                   confidenceIntervalArgs =  list(linecolor="gray80", linewidth=4, linedash = ""),
                                   predictionIntervalArgs = list(linecolor="gray50", linewidth=3, linedash = 1),
                                   label="smooth", parent="root", index=0, ...) {
  stop("Not approperiate widget")
}


#' @export
l_layer_smooth.l_plot <- function(widget, x = NULL, y = NULL, method = "loess", group = "",
                                  formula = y ~ x, interval = c("none", "confidence","prediction"),
                                  n = 80, span = 0.75, level = 0.95,
                                  methodArgs = list(), linecolor="steelblue", linewidth=2, linedash = "",
                                  confidenceIntervalArgs =  list(linecolor="gray80", linewidth=4, linedash = ""),
                                  predictionIntervalArgs = list(linecolor="gray50", linewidth=3, linedash = 1),
                                  label= "smooth", parent="root", index=0, ...) {

  l_throwErrorIfNotLoonWidget(widget)

  interval <- match.arg(interval)

  # inherits coords from widget
  active <- widget['active']
  if(is.null(x)) {
    x <- widget['x'][active]
  }
  if(is.null(y)) {
    y <- try(widget['y'][active])
    if(length(y) != length(x))
      y <- NULL
  }

  xy <- try(grDevices::xy.coords(x, y))
  x <- xy$x
  y <- xy$y

  N <- length(x)

  if(N <= 1) {
    warning("Not enough points to fit the line")
    return(
      l_layer_group(widget,
                    label = label,
                    parent = parent,
                    index = index)
    )
  }

  originalData <- data.frame(x = x,
                             y = y,
                             stringsAsFactors = FALSE)

  if(length(group) == 0 || (length(group) == 1 && group == "") ||
     (length(group) == 1 && is.na(group))) {
    data <- list(originalData)
  } else {

    stateNames <- l_nDimStateNames(widget)
    states <- setNames(lapply(stateNames, function(x) widget[x]), stateNames)

    group <- by2Data(by = group,
                     bySubstitute = substitute(group),
                     n = N, args = states, l_className = "l_plot")

    data <- split(originalData,
                  f = as.list(group),
                  drop = TRUE)
  }

  model <- l_layer_smooth_model(data = data, method = method,
                                formula, span, methodArgs)

  l_layer_smooth_group(widget = widget, data = data, model = model, method = method, formula = formula,
                       interval = interval, n = n, level = level,
                       linecolor = linecolor, linewidth = linewidth, linedash = linedash,
                       confidenceIntervalArgs = confidenceIntervalArgs,
                       predictionIntervalArgs = predictionIntervalArgs,
                       label = label, parent = parent, index = index, ...)
}

#' @export
l_layer_smooth.l_graph <- function(widget, x = NULL, y = NULL, method = "loess", group = "",
                                   formula = y ~ x, interval = c("none", "confidence","prediction"),
                                   n = 80, span = 0.75, level = 0.95,
                                   methodArgs = list(), linecolor="steelblue", linewidth=2, linedash = "",
                                   confidenceIntervalArgs =  list(linecolor="gray80", linewidth=4, linedash = ""),
                                   predictionIntervalArgs = list(linecolor="gray50", linewidth=3, linedash = 1),
                                   label="smooth", parent="root", index=0, ...) {

  l_layer_smooth.l_plot(widget, x = x, y = y, method = method, group = group,
                        formula = formula, interval = interval, n = n, span = span,
                        level = level, methodArgs = methodArgs,
                        linecolor=linecolor, linewidth=linewidth, linedash = linedash,
                        confidenceIntervalArgs = confidenceIntervalArgs,
                        predictionIntervalArgs = predictionIntervalArgs,
                        label=label, parent=parent, index=index, ...)
}

#' @export
l_layer_smooth.l_compound <- function(widget, x = NULL, y = NULL, method = "loess", group = "",
                                      formula = y ~ x, interval = c("none", "confidence","prediction"),
                                      n = 80, span = 0.75, level = 0.95,
                                      methodArgs = list(), linecolor="steelblue", linewidth=2, linedash = "",
                                      confidenceIntervalArgs =  list(linecolor="gray80", linewidth=4, linedash = ""),
                                      predictionIntervalArgs = list(linecolor="gray50", linewidth=3, linedash = 1),
                                      label="smooth", parent="root", index=0, ...) {
  lapply(widget,
         function(w) {
           if(inherits(w, "l_plot") || inherits(w, "l_graph"))
             l_layer_smooth(w, x = x, y = y, method = method, group = group,
                            formula = formula, interval = interval, n = n, span = span,
                            level = level, methodArgs = methodArgs,
                            linecolor=linecolor, linewidth=linewidth, linedash = linedash,
                            confidenceIntervalArgs = confidenceIntervalArgs,
                            predictionIntervalArgs = predictionIntervalArgs,
                            label=label, parent=parent, index=index, ...)
         })
}

l_layer_smooth_model <- function(data, method = "loess",
                                 formula = y ~ x, span = 0.75, methodArgs = list()) {

  stopifnot(
    !missing(data)
  )

  # return fitted models
  models <- lapply(data,
                   function(d) {
                     # Special case span because it's the most commonly used model argument
                     ####################################################
                     m <- method_adjustment(method, methodArgs, span = span)
                     method <- m$method
                     methodArgs <- m$methodArgs
                     ####################################################

                     tryCatch(
                       {
                         base.args <- list(quote(formula), data = quote(d))
                         do.call(method, c(base.args, methodArgs))
                       },
                       error = function(e) {
                         # no formula
                         base.args <- as.list(d)
                         do.call(method, c(base.args, methodArgs))
                       }
                     )
                   })

  models
}

l_layer_smooth_group <- function(widget, data, model, method = "loess", formula = y ~ x,
                                 interval = c("none", "confidence","prediction"),
                                 n = 80, level = 0.95, linecolor="steelblue", linewidth=2, linedash = "",
                                 confidenceIntervalArgs = list(linecolor="gray80", linewidth=4, linedash = ""),
                                 predictionIntervalArgs = list(linecolor="gray50", linewidth=3, linedash = 1),
                                 label="smooth", parent="root", index=0, ...) {

  l_throwErrorIfNotLoonWidget(widget)

  interval <- match.arg(interval)

  smooth_group <- l_layer_group(widget,
                                label = label,
                                parent = parent,
                                index = index)

  len_model <- length(model)

  stopifnot(
    length(data) == len_model
  )

  linewidth <- rep_len(linewidth, len_model)
  linecolor <- rep_len(linecolor, len_model)
  linedash <- rep_len(linedash, len_model)

  # confidence interval
  c.sewidth <- rep_len(confidenceIntervalArgs$linewidth, len_model)
  c.secolor <- rep_len(confidenceIntervalArgs$linecolor, len_model)
  c.sedash <- rep_len(confidenceIntervalArgs$linedash, len_model)
  # prediction interval
  p.sewidth <- rep_len(predictionIntervalArgs$linewidth, len_model)
  p.secolor <- rep_len(predictionIntervalArgs$linecolor, len_model)
  p.sedash <- rep_len(predictionIntervalArgs$linedash, len_model)

  for(j in seq(len_model)) {

    d <- data[[j]]
    m <- model[[j]]

    range <- range(d$x, na.rm = TRUE)

    s <- l_layer_group(widget,
                       label = paste(j, ".", method, deparse(formula)),
                       parent = smooth_group)
    pred <- NULL

    # display both intervals
    lapply(c("confidence", "prediction"),
           function(int) {

             pre <- tryCatch(
               expr = {
                 l_predict(model = m, newdata = data.frame(x = seq(range[1], range[2], length.out = n)),
                           interval = int, level = level, ...)
               },
               error = function(e) {
                 return(NULL)
               }
             )

             # prediction interval (or confidence interval) is not implemented in
             # `l_predict.xx` function
             if(is.null(pre)) return(NULL)

             len <- length(pre$x)

             i <- l_layer_group(widget,
                                label = paste(level, int, "interval"),
                                parent = s)

             if(int == "confidence") {
               secolor <- c.secolor[j]
               sewidth <- c.sewidth[j]
               sedash <- c.sedash[j]
             } else {
               # prediction
               secolor <- p.secolor[j]
               sewidth <- p.sewidth[j]
               sedash <- p.sedash[j]
             }

             if(is.null(pre$x)) {
               x <- m$x
               pred <<- data.frame(x = m$x, y = m$y)
             } else {
               x <- pre$x
               pred <<- pre
             }

             # intervals
             il <- l_layer_line(widget,
                                x = x,
                                y = pre$lower,
                                linewidth = sewidth,
                                color = secolor,
                                dash = sedash,
                                label = "lower bound",
                                parent = i)

             iu <- l_layer_line(widget,
                               x = x,
                               y = pre$upper,
                               linewidth = sewidth,
                               color = secolor,
                               dash = sedash,
                               label = "upper bound",
                               parent = i)

             if(int != interval)
               l_layer_hide(i)
           })

    l_layer_line(widget,
                 x = pred$x,
                 y = pred$y,
                 linewidth = linewidth[j],
                 color = linecolor[j],
                 dash = linedash[j],
                 tag = "model",
                 label = "fitted line",
                 parent = s)

  }

  smooth_group
}


method_adjustment <- function(method, methodArgs, span = 0.75) {

  if (identical(method, "loess")) {
    methodArgs$span <- span
  }
  if (is.character(method)) {
    # if (identical(method, "gam")) {
    #   method <- mgcv::gam
    # } else {
    #   method <- match.fun(method)
    # }
    method <- match.fun(method)
  }
  # If gam and gam's method is not specified by the user then use REML
  # if (identical(method, mgcv::gam) && is.null(methodArgs$method)) {
  #   methodArgs$method <- "REML"
  # }

  list(
    method = method,
    methodArgs = methodArgs
  )
}
