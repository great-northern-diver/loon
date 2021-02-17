#' @title Layer a smooth line for \code{loon}
#' @description Display a smooth line layer
#' @param widget widget path name as a string
#' @param x The \code{x} coordinates of line. If it is not provided, \code{x} will be inherited from widget
#' @param y The \code{y} coordinates of line. If it is not provided, \code{y} will be inherited from widget
#' @param method Smoothing method (function) to use, accepts either a character vector,
#' e.g. "lm", "glm", "loess" or a function, e.g. MASS::rlm or mgcv::gam, stats::lm, or stats::loess.
#' @param group Coordinates can be grouped by aesthetics attributes, e.g. "color".
#' If \code{x} and \code{y} are inherited from widget, \code{group} can be set as any N
#' dimensional states of corresponding widget; if not, \code{group} can only be set as either "linecolor" or "linewidth"
#' @param formula Formula to use in smoothing function, eg. y ~ x, y ~ poly(x, 2), y ~ log(x)
#' @param se Display confidence interval around smooth? (TRUE by default, see level to control.)
#' @param n Number of points at which to evaluate smoother.
#' @param span Controls the amount of smoothing for the default loess smoother. Smaller numbers produce wigglier lines, larger numbers produce smoother lines.
#' @param level Level of confidence interval to use (0.95 by default).
#' @param method.args List of additional arguments passed on to the modelling function defined by method.
#' @param linecolor fitted line color. Note that the \code{linecolor} of fitted lines are respect to whether \code{x} and \code{y} are provided.
#' If coordinates are inherited from widget, the linecolor can only be set via \code{\link{l_configure}}
#' @param secolor fitted standard deviation line color
#' @param linewidth fitted line width
#' @param sewidth fitted standard deviation line width
#' @param linedash fitted line dash
#' @param sedash fitted standard deviation line dash
#' @param label label used in the layers inspector
#' @param parent group layer
#' @param index index of the newly added layer in its parent group
#' @param ... additional state initialization arguments, see \code{\link{l_info_states}}
#'
#' @export
#'
#' @examples
#' if(interactive()) {
#'
#' p <- l_plot(iris, color = iris$Species)
#' #' # the fitted line is based on all active points
#' l1 <- l_layer_smooth(p)
#' l_layer_hide(l1)
#' # the fits are grouped by points color
#' l2 <- l_layer_smooth(p, group = "color")
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
#'   l_layer_hide(l3)
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
#' }
#'
#'
#'

l_layer_smooth <- function(widget, x = NULL, y = NULL, method = "loess", group = "",
                           formula = y ~ x, se = TRUE, n = 80, span = 0.75, level = 0.95, method.args = list(),
                           linecolor="steelblue", secolor = "gray80", linewidth=2, sewidth = 4,
                           linedash = "", sedash = "",
                           label="smooth", parent="root", index=0, ...) {

  UseMethod("l_layer_smooth")
}

#' @export
l_layer_smooth.l_plot <- function(widget, x = NULL, y = NULL, method = "loess", group = "",
                                  formula = y ~ x, se = TRUE, n = 80, span = 0.75, level = 0.95, method.args = list(),
                                  linecolor="steelblue", secolor = "gray80", linewidth=2, sewidth = 4,
                                  linedash = "", sedash = "", label="smooth", parent="root", index=0, ...) {

  l_throwErrorIfNotLoonWidget(widget)

  # inherits coords from widget
  inherit <- FALSE
  active <- widget['active']
  if(is.null(x)) {
    x <- widget['x'][active]
    inherit <- TRUE
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

  if(length(group) == 0 || (length(group) == 1 && group == "") || is.na(group)) {
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
                                formula, span, method.args)

  l_layer_smooth_group(widget = widget, data = data, model = model,
                       se = se, n = n, level = level,
                       linecolor = linecolor, secolor = secolor,
                       linewidth = linewidth, sewidth = sewidth,
                       linedash = linedash, sedash = sedash,
                       label = label, parent = parent, index = index, ...)
}

#' @export
l_layer_smooth.l_graph <- function(widget, x = NULL, y = NULL, method = "loess", group = "",
                                   formula = y ~ x, se = TRUE, n = 80, span = 0.75, level = 0.95, method.args = list(),
                                   linecolor="steelblue", secolor = "gray80", linewidth=2, sewidth = 4,
                                   linedash = "", sedash = "", label="smooth", parent="root", index=0, ...) {
  l_layer_smooth.l_plot(widget, x = x, y = y, method = method, group = group,
                        formula = formula, se = se, n = n, span = span,
                        level = level, method.args = method.args,
                        linecolor=linecolor, secolor = secolor,
                        linewidth=linewidth, sewidth = sewidth,
                        linedash = linedash, sedash = sedash,
                        label=label, parent=parent, index=index, ...)
}

#' @export
l_layer_smooth.l_hist <- function(widget, x = NULL, y = NULL, method = "loess", group = "",
                                  formula = y ~ x, se = TRUE, n = 80, span = 0.75, level = 0.95, method.args = list(),
                                  linecolor="steelblue", secolor = "gray80", linewidth=2, sewidth = 4,
                                  linedash = "", sedash = "", label="smooth", parent="root", index=0, ...) {
  stop("Smooth line for histogram?")
}

#' @export
l_layer_smooth.l_serialaxes <- function(widget, x = NULL, y = NULL, method = "loess", group = "",
                                        formula = y ~ x, se = TRUE, n = 80, span = 0.75, level = 0.95, method.args = list(),
                                        linecolor="steelblue", secolor = "gray80", linewidth=2, sewidth = 4,
                                        linedash = "", sedash = "", label="smooth", parent="root", index=0, ...) {
  stop("Smooth line for serial axes?")
}


#' @export
l_layer_smooth.l_compound <- function(widget, x = NULL, y = NULL, method = "loess", group = "",
                                      formula = y ~ x, se = TRUE, n = 80, span = 0.75, level = 0.95, method.args = list(),
                                      linecolor="steelblue", secolor = "gray80", linewidth=2, sewidth = 4,
                                      linedash = "", sedash = "", label="smooth", parent="root", index=0, ...) {
  lapply(widget,
         function(w) {
           if(inherits(w, "l_plot") || inherits(w, "l_graph"))
             l_layer_smooth(w, x = x, y = y, method = method, group = group,
                            formula = formula, se = se, n = n, span = span,
                            level = level, method.args = method.args,
                            linecolor=linecolor, secolor = secolor,
                            linewidth=linewidth, sewidth = sewidth,
                            label=label, parent=parent, index=index, ...)
         })
}

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
                                 linecolor="steelblue", secolor="gray80", linewidth=2, sewidth = 4,
                                 linedash = "", sedash = "", label="smooth", parent="root", index=0, ...) {

  l_throwErrorIfNotLoonWidget(widget)

  smooth_group <- l_layer_group(widget,
                                label = label,
                                parent = parent,
                                index = index)

  len_model <- length(model)

  stopifnot(
    length(data) == len_model
  )

  linewidth <- rep_len(linewidth, len_model)
  sewidth <- rep_len(sewidth, len_model)
  linecolor <- rep_len(linecolor, len_model)
  secolor <- rep_len(secolor, len_model)
  linedash <- rep_len(linedash, len_model)
  sedash <- rep_len(sedash, len_model)

  for(i in seq(len_model)) {

    d <- data[[i]]
    m <- model[[i]]

    range <- range(d$x, na.rm = TRUE)

    pre <- l_predict(m, xseq = seq(range[1], range[2], length.out = n),
                     se, level)

    s <- l_layer_group(widget,
                       label = paste0("line ", i),
                       parent = smooth_group)

    if(se) {
      # standard deviation
      l_layer_line(widget,
                   x = c(pre$x, rev(pre$x), pre$x[1]),
                   y = c(pre$ymin, rev(pre$ymax), pre$ymin[1]),
                   linewidth = sewidth[i],
                   color = secolor[i],
                   dash = sedash[i],
                   tag = "CI",
                   label = paste0("confidence interval ", i),
                   parent = s,
                   ...)
    }
    l_layer_line(widget,
                 x = pre$x,
                 y = pre$y,
                 linewidth = linewidth[i],
                 color = linecolor[i],
                 dash = linedash[i],
                 tag = "model",
                 label = paste0("fitted line ", i),
                 parent = s,
                 ...)
  }

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

# groupedStates <- function(widget, group) {
#
#   states <- stats::setNames(
#     lapply(group,
#            function(g) {
#              state <- tryCatch(
#                {
#                  if(g == "color")
#                    hex12tohex6(widget[g])
#                  else widget[g]
#                },
#                error = function(e) {
#                  NULL
#                })
#            }),
#     group
#   )
#
#   Filter(Negate(is.null), states)
# }
#
# valid_group_by <- function(widget, group) {
#   nDimStates <- l_nDimStateNames(widget)
#
#   if(length(group) == 0) return(NULL)
#   if(!group %in% nDimStates) return(NULL)
#   else intersect(group, nDimStates)
# }
