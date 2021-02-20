#  inspired by `geom_smooth` and `stat_smooth` in ggplot2
#' @title Model Prediction
#' @name l_predict
#' @description It is entirely for the purpose of plotting fits and intervals on a scatterplot (or histogram).
#' It is a generic function to predict models for \code{loon} smooth layer (a wrap of the function \code{predict}).
#' However, the output is unified.
#' @param model a model object for which prediction is desired
#' @param ... some arguments passed into the function \code{predict}
#' @return
#' A data frame is returned with \code{x} (if \code{newdata} is given) and \code{y}.
#' If the \code{interval} is not \code{none},
#' two more columns, \code{lower} (lower interval) and \code{upper} (upper interval) will be returned.
#'
#' @examples
#' y <- rnorm(10)
#' x <- rnorm(10)
#' model1 <- lm(y ~ x)
#' # formal output
#' pre <- l_predict(model1, newdata = data.frame(x = sort(x)),
#'                  interval = "conf")
#' head(pre)
#'
#' if(interactive()) {
#' p <- with(cars, l_plot(speed, dist))
#'
#' # Example taken from
#' # https://stackoverflow.com/questions/23852505/how-to-get-confidence-interval-for-smooth-spline
#' #
#' l_predict.smooth.spline <- function(model, interval = c("confidence", "none"),
#'                                     level = 0.95, ...) {
#' # confidence interval of `smooth.spline`
#'   interval <- match.arg(interval)
#'
#'   res <- (model$yin - model$y)/(1 - model$lev)     # jackknife residuals
#'   sigma <- sqrt(var(res))                          # estimate sd
#'   std <- stats::qnorm(level / 2 + 0.5)
#'   upper <- model$y + std * sigma * sqrt(model$lev) # upper 95% conf. band
#'   lower <- model$y - std * sigma * sqrt(model$lev) # lower 95% conf. band
#'
#'   data.frame(y = model$yin, lower = lower, upper = upper)
#' }
#' l <- l_layer_smooth(p, method = "smooth.spline", interval = "confidence")
#' }
#'
#' @export
l_predict <- function(model, ...)
  UseMethod("l_predict")

#' @export
#' @rdname l_predict
l_predict.default <- function(model, ...) {
  stop("Unknown method")
}

#' @export
#' @rdname l_predict
#' @param newdata optionally, a data frame in which to look for variables with which to predict.
#' If omitted, the fitted linear predictors are used.
#' @param interval type of interval, could be "none", "confidence" or "prediction" (not for \code{glm})
#' @param level confidence level
#' @param ... arguments passed in \code{predict}
l_predict.lm <- function(model, newdata = NULL,
                         interval = c("none", "confidence", "prediction"),
                         level = 0.95, ...) {

  interval <- match.arg(interval)

  se.fit <- TRUE
  if(interval == "none") se.fit <- FALSE

  pred <- stats::predict(model,
                         newdata,
                         se.fit = se.fit,
                         level = level,
                         interval = interval, ...)

  if (se.fit) {
    fit <- as.data.frame(pred$fit)
    names(fit) <- c("y", "lower", "upper")

    if(is.null(newdata)) {
      data.frame(fit)
    } else {
      data.frame(newdata, fit)
    }

  } else {

    if(is.null(newdata)) {
      data.frame(y = as.vector(pred))
    } else {
      data.frame(newdata, y = as.vector(pred))
    }
  }
}

#' @rdname l_predict
#' @export
l_predict.nls <- function(model, newdata = NULL,
                          interval = c("none", "confidence", "prediction"),
                          level = 0.95, ...) {

  l_predict.lm(model, newdata,
               interval = interval,
               level = level, ...)
}

#' @rdname l_predict
#' @export
l_predict.glm <- function(model, newdata = NULL,
                          interval = c("none", "confidence"),
                          level = 0.95, ...) {

  interval <- match.arg(interval)

  se.fit <- TRUE
  if(interval == "none") se.fit <- FALSE

  pred <- stats::predict(model, newdata = newdata,
                         se.fit = se.fit, type = "link", ...)

  if (se.fit) {

    if(interval == "prediction") {
      message("Prediction interval is not provided for `glm`")
    }

    se <- as.vector(pred$se.fit)

    std <- stats::qnorm(level / 2 + 0.5)
    y <- model$family$linkinv(as.vector(pred$fit))
    lower <- model$family$linkinv(as.vector(pred$fit - std * se))
    upper <- model$family$linkinv(as.vector(pred$fit + std * se))

    if(is.null(newdata)) {
      data.frame(
        y = y,
        lower = lower,
        upper = upper
      )
    } else {
      data.frame(
        newdata,
        y = y,
        lower = lower,
        upper = upper
      )
    }

  } else {

    if(is.null(newdata)) {
      data.frame(y = model$family$linkinv(as.vector(pred)))
    } else {
      data.frame(newdata, y = model$family$linkinv(as.vector(pred)))
    }
  }
}

#' @rdname l_predict
#' @export
l_predict.loess <- function(model, newdata = NULL,
                            interval = c("none", "confidence", "prediction"),
                            level = 0.95, ...) {

  interval <- match.arg(interval)

  se.fit <- TRUE
  if(interval == "none") se.fit <- FALSE

  pred <- stats::predict(model, newdata, se = se.fit, ...)

  if (se.fit) {

    se <- if(interval == "confidence") {
      as.vector(pred$se.fit)
    } else {
      # predict interval
      sqrt(as.vector(pred$se.fit)^2 + as.vector(pred$residual.scale)^2)
    }

    y <- pred$fit
    ci <- se * stats::qt(level / 2 + .5, pred$df)
    lower <- y - ci
    upper <- y + ci

    if(is.null(newdata)) {
      data.frame(y = y,
                 lower = lower,
                 upper = upper)
    } else {
      data.frame(newdata, y = y,
                 lower = lower,
                 upper = upper)
    }

  } else {

    if(is.null(newdata)) {
      data.frame(y = as.vector(pred))
    } else {
      data.frame(newdata, y = as.vector(pred))
    }
  }
}

# l_predict.locfit <- function(model, newdata, se, level) {
#   pred <- stats::predict(model, newdata, se.fit = se)
#
#   if (se) {
#     y <- pred$fit
#     lower <- y - pred$se.fit
#     upper <- y + pred$se.fit
#
#     if(is.null(newdata)) {
#        data.frame(y, lower, upper)
#     } else {
#        data.frame(newdata, y, lower, upper)
#     }
#
#   } else {
#     if(is.null(newdata)) {data.frame(y = as.vector(pred))}
#     else {data.frame(newdata, y = as.vector(pred))}
#   }
# }
