# inspired by `geom_smooth` and `stat_smooth` in ggplot2
l_predict <- function(model, xseq, se, level) UseMethod("l_predict")

l_predict.default <- function(model, xseq, se, level) {

  pred <- stats::predict(model,
                         newdata = data.frame(x = xseq),
                         se.fit = se,
                         level = level,
                         interval = if (se) "confidence" else "none")

  if (se) {
    fit <- as.data.frame(pred$fit)
    names(fit) <- c("y", "ymin", "ymax")
    data.frame(x = xseq, fit, se = pred$se.fit)
  } else {
    data.frame(x = xseq, y = as.vector(pred))
  }
}

l_predict.glm <- function(model, xseq, se, level) {
  pred <- stats::predict(model, newdata = data.frame(x = xseq), se.fit = se,
                         type = "link")

  if (se) {
    std <- stats::qnorm(level / 2 + 0.5)
    data.frame(
      x = xseq,
      y = model$family$linkinv(as.vector(pred$fit)),
      ymin = model$family$linkinv(as.vector(pred$fit - std * pred$se.fit)),
      ymax = model$family$linkinv(as.vector(pred$fit + std * pred$se.fit)),
      se = as.vector(pred$se.fit)
    )
  } else {
    data.frame(x = xseq, y = model$family$linkinv(as.vector(pred)))
  }
}

l_predict.loess <- function(model, xseq, se, level) {
  pred <- stats::predict(model, newdata = data.frame(x = xseq), se = se)

  if (se) {
    y = pred$fit
    ci <- pred$se.fit * stats::qt(level / 2 + .5, pred$df)
    ymin = y - ci
    ymax = y + ci
    data.frame(x = xseq, y, ymin, ymax, se = pred$se.fit)
  } else {
    data.frame(x = xseq, y = as.vector(pred))
  }
}

l_predict.locfit <- function(model, xseq, se, level) {
  pred <- stats::predict(model, newdata = data.frame(x = xseq), se.fit = se)

  if (se) {
    y = pred$fit
    ymin = y - pred$se.fit
    ymax = y + pred$se.fit
    data.frame(x = xseq, y, ymin, ymax, se = pred$se.fit)
  } else {
    data.frame(x = xseq, y = as.vector(pred))
  }
}
