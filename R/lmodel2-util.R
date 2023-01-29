#' Extract Model Coefficients
#'
#' \code{coef} is a generic function which extracts model coefficients from
#' objects returned by modeling functions. \code{coefficients} is an alias for
#' it.
#'
#' @details Function \code{lmodel2()} from package 'lmodel2' returns a fitted
#'   model object of class \code{"lmodel2"} which differs from that returned by
#'   \code{lm()}. Here we implement a \code{coef()} method for objects of this
#'   class. It differs from de generic method and that for lm objects in having
#'   an additional formal parameter \code{method} that must be used to select
#'   estimates based on which of the methods supported by \code{lmodel2()} are
#'   to be extracted. The returned object is identical in its structure to that
#'   returned by \code{coef.lm()}.
#'
#' @param object a fitted model object.
#' @param method character One of the methods available in \code{object}.
#' @param ... ignored by this method.
#'
#' @return A named numeric vector of length two.
#'
#' @export
#'
#' @seealso \code{\link[lmodel2]{lmodel2}}
#'
coef.lmodel2 <- function(object,
                         method = "MA",
                         ...) {
  if (! method %in% object[["regression.results"]][["Method"]]) {
    stop("Method '", method, "' not in fit object ", call. = FALSE)
  }
  idx <- which(object[["regression.results"]][["Method"]] == method)
  Slope.var <- gsub("^.*~[ ]*", "", as.character(object$call[2]))
  z <- c(object[["regression.results"]][["Intercept"]][[idx]],
         object[["regression.results"]][["Slope"]][[idx]])
  names(z) <- c("(Intercept)", Slope.var)
  z
}

#' Confidence Intervals for Model Parameters
#'
#' Computes confidence intervals for one or more parameters in a fitted model.
#' This a method for objects inheriting from class "lmodel2".
#'
#' @details Function \code{lmodel2()} from package 'lmodel2' returns a fitted
#'   model object of class \code{"lmodel2"} which differs from that returned by
#'   \code{lm()}. Here we implement a \code{confint()} method for objects of
#'   this class. It differs from the generic method and that for lm objects in
#'   having an additional formal parameter \code{method} that must be used to
#'   select estimates based on which of the methods supported by
#'   \code{lmodel2()} are to be extracted. The returned object is identical in
#'   its structure to that returned by \code{confint.lm()}.
#'
#' @param object a fitted model object.
#' @param method character One of the methods available in \code{object}.
#' @param parm a specification of which parameters are to be given confidence
#'   intervals, either a vector of numbers or a vector of names. If missing, all
#'   parameters are considered.
#' @param level the confidence level required. Currently only 0.95 accepted.
#' @param ... ignored by this method.
#'
#' @return A data frame with two rows and three columns.
#'
#' @export
#'
#' @seealso \code{\link[lmodel2]{lmodel2}}
#'
confint.lmodel2 <- function (object,
                             parm,
                             level = 0.95,
                             method = "MA",
                             ...) {
  if (! method %in% object[["regression.results"]][["Method"]]) {
    stop("Method '", method, "' not in fit object ", call. = FALSE)
  }
  if (missing(parm)) {
    parm <- 1:2 # we always have two parametes
  }
  idx <- which(object[["regression.results"]][["Method"]] == method)
  Slope.var <- gsub("^.*~[ ]*", "", as.character(object$call[2]))
  if (level != 0.95) {
    warning("Currently only 'level = 0.95' is supported.")
    level <- 0.95
  }
  z <- matrix(unlist(object[["confidence.intervals"]][idx, 2:5]),
              ncol = 2, byrow = TRUE)
  rownames(z) <- c("(Intercept)", Slope.var)
  colnames(z) <- c("2.5 %", "97.5 %")
  z[parm, ]
}

#' Model Predictions
#'
#' \code{predict} is a generic function for predictions from the results of
#'   various model fitting functions. \code{predict.lmodel2} is the method
#'   for model fit objects of class \code{"lmodel2"}.
#'
#' @details Function \code{lmodel2()} from package 'lmodel2' returns a fitted
#'   model object of class \code{"lmodel2"} which differs from that returned by
#'   \code{lm()}. Here we implement a \code{predict()} method for objects of
#'   this class. It differs from the generic method and that for \code{lm}
#'   objects in having an additional formal parameter \code{method} that must be
#'   used to select which of the methods supported by \code{lmodel2()} are to be
#'   used in the prediction. The returned object is similar in its structure to
#'   that returned by \code{predict.lm()} but lacking names or rownames.
#'
#' @param object a fitted model object.
#' @param method character One of the methods available in \code{object}.
#' @param newdata An optional data frame in which to look for variables with
#'   which to predict. If omitted, the fitted values are used.
#' @param interval Type of interval calculation.
#' @param level the confidence level required. Currently only 0.95 accepted.
#' @param ... ignored by this method.
#'
#' @return If \code{interval = "none"} a numeric vector is returned, while if
#'   \code{interval = "confidence"} a data frame with columns \code{fit},
#'   \code{lwr} and \code{upr} is returned.
#'
#' @export
#'
#' @seealso \code{\link[lmodel2]{lmodel2}}
#'
predict.lmodel2 <-function(object,
                           method = "MA",
                           newdata = NULL,
                           interval = c("none", "confidence"),
                           level = 0.95,
                           ...) {
  # this code is in most part borrowed from lmodel2::plot.lmodel2()
  stopifnot(interval %in% c("none", "confidence"))
  y <- object$y
  x <- object$x
  Slope.var <- gsub("^.*~[ ]*", "", as.character(object$call[2]))
  if (!is.null(newdata)) {
    new.x <- newdata[[Slope.var]]
  } else {
    new.x <- x
  }
  centr.y <- mean(y)
  centr.x <- mean(x)
  row <- which(object$regression.results == method)
  a <- object$regression.results[row, 2]
  b <- object$regression.results[row, 3]
  b1 <- object$confidence.intervals[row, 4]
  a1 <- centr.y - b1 * centr.x
  b2 <- object$confidence.intervals[row, 5]
  a2 <- centr.y - b2 * centr.x

  if ((row != 1) && (object$rsquare <= object$epsilon)) {
    warning("R-square = 0: model and C.I. not computed for MA, SMA or RMA")
    y.predicted <- rep(NA_real_, length(new.x))
  } else {
    y.predicted <- a + b * new.x
  }

  if (interval == "confidence") {
    if (is.na(a1) || all(is.na(y.predicted))) {
      y1.predicted <- y2.predicted <- rep(NA_real_, length(new.x))
    } else {
      y1.predicted <- a1 + b1 * new.x
      y2.predicted <- a2 + b2 * new.x
    }

    data.frame(fit = y.predicted,
               lwr = ifelse(y1.predicted < y2.predicted,
                            y1.predicted,
                            y2.predicted),
               upr = ifelse(y1.predicted < y2.predicted,
                            y2.predicted,
                            y1.predicted))
  } else {
    y.predicted
  }
}
