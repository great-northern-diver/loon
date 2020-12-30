#' @title Layer a smooth line for \code{loon}
#' @description Display a smooth line layer
#' @param widget widget path name as a string
#' @param x The \code{x} coordinates of line. If it is not provided, \code{x} will be inherited from widget
#' @param y The \code{y} coordinates of line. If it is not provided, \code{y} will be inherited from widget
#' @param method Smoothing method (function) to use, accepts either a character vector,
#' e.g. "lm", "glm", "loess" or a function, e.g. MASS::rlm or mgcv::gam, stats::lm, or stats::loess.
#' @param group_by Coordinates can be grouped by aesthetics attributes, e.g. "color".
#' If \code{x} and \code{y} are inherited from widget, \code{group_by} can be set as any N
#' dimensional states of corresponding widget; if not, \code{group_by} can only be set as either "linecolor" or "linewidth"
#' @param formula Formula to use in smoothing function, eg. y ~ x, y ~ poly(x, 2), y ~ log(x)
#' @param se Display confidence interval around smooth? (TRUE by default, see level to control.)
#' @param n Number of points at which to evaluate smoother.
#' @param span Controls the amount of smoothing for the default loess smoother. Smaller numbers produce wigglier lines, larger numbers produce smoother lines.
#' @param level Level of confidence interval to use (0.95 by default).
#' @param method.args List of additional arguments passed on to the modelling function defined by method.
#' @param linecolor fitted line color. Note that the \code{linecolor} of fitted lines are respect to whether \code{x} and \code{y} are provided.
#' If coordinates are inherited from widget, the linecolor can only be set via \code{\link{l_configure}}
#' @param se_color fitted standard deviation polyline color
#' @param linewidth fitted line width
#' @param se_width fitted standard deviation polyline width
#' @param label label used in the layers inspector
#' @param parent group layer
#' @param index index of the newly added layer in its parent group
#' @param ... additional state initialization arguments, see \code{\link{l_info_states}}
#'
#' @importFrom grDevices xy.coords
#'
#' @export
#'
#' @examples
#' if(interactive()) {
#' p <- l_plot(iris, color = iris$Species)
#' # the fits are grouped by points color
#' l1 <- l_layer_smooth(p)
#' l_layer_hide(l1)
#' # the fitted line is based on all points
#' l2 <- l_layer_smooth(p, group_by = "")
#' l_layer_hide(l2)
#'
#' # Draw a fitted line based on a new data set
#' shortSepalLength <- (iris$Sepal.Length < 5)
#' l3 <- l_layer_smooth(p,
#'                      x = iris$Sepal.Length[shortSepalLength],
#'                      y = iris$Sepal.Width[shortSepalLength],
#'                      method = "lm",
#'                      linecolor = "firebrick")
#'
#' if(require(mgcv)) {
#'   p1 <- l_plot(iris)
#'   # a full tensor product smooth
#'   ## linecolor is the same with the points color
#'   l4 <- l_layer_smooth(p1,
#'                        method = "gam",
#'                        formula = y~te(x))
#'   l_layer_hide(l4)
#'   ## linecolor is set as the default input color 'steelblue'
#'   l5 <- l_layer_smooth(p1,
#'                        x = p1['x'],
#'                        y = p1['y'],
#'                        method = "gam",
#'                        formula = y~te(x))
#' }
#' }
#'
l_layer_smooth <- function(widget, x, y = NULL, method = "loess", group_by = "color",
                           formula = y ~ x, se = TRUE, n = 80, span = 0.75, level = 0.95, method.args = list(),
                           linecolor="steelblue", se_color = "gray80", linewidth=2, se_width = 4,
                           label="smooth", parent="root", index=0, ...) {

  l_throwErrorIfNotLoonWidget(widget)

  data <- l_layer_smooth_data(widget = widget, x, y = y, group_by = group_by, linecolor = linecolor, linewidth=linewidth)
  model <- l_layer_smooth_model(data = data, method = method,
                                formula, span, method.args)

  l_layer_smooth_group(widget = widget,
                       data = data, model = model, se = se, n = n, level = level,
                       linecolor = linecolor, se_color = se_color, linewidth = linewidth, se_width = se_width,
                       label = label, parent = parent, index = index, ...)
}

# get data
l_layer_smooth_data <- function(widget, x, y = NULL, group_by = "color", linecolor="steelblue", linewidth=2) {

  l_throwErrorIfNotLoonWidget(widget)

  # inherits coords from widget
  inherit <- FALSE
  if(missing(x)) {
    x <- widget['x']
    inherit <- TRUE
  }
  if(is.null(y)) {
    y <- try(widget['y'])
    if(length(y) != length(x))
      y <- NULL
  }

  xy <- try(grDevices::xy.coords(x, y))
  x <- xy$x
  y <- xy$y

  if(inherit) {

    group_by <- valid_group_by(widget, group_by)

    data <- data.frame(x = x,
                       y = y, stringsAsFactors = FALSE)

    if(length(group_by) == 0) return(list(data))
    if(length(group_by) == 1 && group_by == "") return(list(data))

    data <- cbind(data,
                  groupedStates(widget,
                                group_by = group_by),
                  stringsAsFactors = FALSE)
    split(data,
          f = lapply(group_by, function(by) data[[by]]),
          drop = TRUE)

  } else {

    data <- data.frame(x = x,
                       y = y,
                       color = linecolor,
                       width = linewidth, stringsAsFactors = FALSE)
    if(length(group_by) == 0) return(list(data))
    if(length(group_by) == 1 && group_by == "") return(list(data))

    split(data,
          f = lapply(group_by,
                     function(by) {
                       if(grepl("color", by)) {
                         data[["color"]]
                       } else if(grepl("width", by)) {
                         data[["width"]]
                       } else NULL
                     }),
          drop = TRUE)
  }
}

# get data
l_layer_smooth_model <- function(data, method = "loess",
                                 formula = y ~ x, span = 0.75, method.args = list()) {

  stopifnot(
    !missing(data)
  )

  # return fitted models
  models <- lapply(data,
                   function(d) {
                     # Special case span because it's the most commonly used model argument
                     ####################################################
                     m <- method_adjustification(method, method.args, span = span)
                     method <- m$method
                     method.args <- m$method.args
                     ####################################################

                     tryCatch(
                       {
                         base.args <- list(quote(formula), data = quote(d))
                         do.call(method, c(base.args, method.args))
                       },
                       error = function(e) {
                         formula <- y~x
                         base.args <- list(quote(formula), data = quote(d))
                         do.call(method, c(base.args, method.args))
                       }
                     )
                   })

  models
}

l_layer_smooth_group <- function(widget, data, model, se = TRUE, n = 80, level = 0.95,
                                 linecolor="steelblue", se_color="gray80", linewidth=2, se_width = 4,
                                 label="smooth", parent="root", index=0, ...) {

  l_throwErrorIfNotLoonWidget(widget)

  smooth_group <- l_layer_group(widget,
                                label = label,
                                parent = parent,
                                index = index)

  stopifnot(
    length(data) == length(model)
  )

  i <- 1
  lapply(model,
         function(m) {

           d <- data[[i]]

           range <- range(d$x, na.rm = TRUE)

           pre <- l_predict(m, xseq = seq(range[1], range[2], length.out = n),
                            se, level)

           s <- l_layer_group(widget,
                              label = paste0("line ", i),
                              parent = smooth_group)

           unique_color <- if(is.null(d$color)) unique(linecolor) else unique(d$color)
           color <- if(length(unique_color) == 1) {
             unique_color
           } else linecolor

           if(se) {
             # standard deviation
             l_layer_line(widget,
                          x = c(pre$x, rev(pre$x), pre$x[1]),
                          y = c(pre$ymin, rev(pre$ymax), pre$ymin[1]),
                          linewidth = se_width,
                          color = se_color,
                          tag = "CI",
                          label = paste0("confidence interval ", i),
                          parent = s,
                          ...)
           }
           l_layer_line(widget,
                        x = pre$x,
                        y = pre$y,
                        linewidth = linewidth,
                        color = color,
                        tag = "model",
                        label = paste0("fitted line ", i),
                        parent = s,
                        ...)
           i <<- i + 1
         })

  smooth_group
}


method_adjustification <- function(method, method.args, span = 0.75) {

  if (identical(method, "loess")) {
    method.args$span <- span
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
  # if (identical(method, mgcv::gam) && is.null(method.args$method)) {
  #   method.args$method <- "REML"
  # }

  list(
    method = method,
    method.args = method.args
  )
}

groupedStates <- function(widget, group_by) {

  states <- stats::setNames(
    lapply(group_by,
           function(g) {
             state <- tryCatch(
               {
                 if(g == "color")
                   hex12tohex6(widget[g])
                 else widget[g]
               },
               error = function(e) {
                 NULL
               })
           }),
    group_by
  )

  Filter(Negate(is.null), states)
}

valid_group_by <- function(widget, group_by) {
  nDimStates <- l_nDimStateNames(widget)

  if(length(group_by) == 0) return(NULL)
  if(!group_by %in% nDimStates) return(NULL)
  else intersect(group_by, nDimStates)
}
