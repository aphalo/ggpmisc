#' Residuals from a model fit
#'
#' \code{stat_fit_residuals} fits a linear model and returns
#'    residuals ready to be plotted as points.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs
#'   to be set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points on this
#'   layer
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and should not inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param method function or character If character, "lm", "rlm", "rq" and the
#'   name of a function to be matched, possibly followed by the fit function's
#'   \code{method} argument separated by a colon (e.g. \code{"rq:br"}).
#'   Functions implementing methods must accept arguments to parameters
#'   \code{formula}, \code{data}, \code{weights} and \code{method}. A
#'   \code{residuals()} method must exist for the returned model fit object
#'   class.
#' @param method.args named list with additional arguments.
#' @param formula a "formula" object. Using aesthetic names instead of
#'   original variable names.
#' @param resid.type character passed to \code{residuals()} as argument for
#'   \code{type} (defaults to \code{"working"} except if \code{weighted = TRUE}
#'   when it is forced to \code{"deviance"}).
#' @param weighted logical If true weighted residuals will be returned.
#' @param orientation character Either "x" or "y" controlling the default for
#'   \code{formula}.
#'
#' @details This stat can be used to automatically plot residuals as points in a
#'   plot. At the moment it supports only linear models fitted with function
#'   \code{lm()} or \code{rlm()}. It applies to the fitted model object methods
#'   \code{\link[stats]{residuals}} or \code{\link[stats]{weighted.residuals}}
#'   depending on the argument passed to parameter \code{weighted}.
#'
#'   A ggplot statistic receives as data a data frame that is not the one passed
#'   as argument by the user, but instead a data frame with the variables mapped
#'   to aesthetics. In other words, it respects the grammar of graphics and
#'   consequently within the model \code{formula} names of
#'   aesthetics like $x$ and $y$ should be used instead of the original variable
#'   names, while data is automatically passed the data frame. This helps ensure
#'   that the model is fitted to the same data as plotted in other layers.
#'
#' @note How weights are applied to residuals depends on the method used to fit
#'   the model. For ordinary least squares (OLS), weights are applied to the
#'   squares of the residuals, so the weighted residuals are obtained by
#'   multiplying the "deviance" residuals by the square root of the weights.
#'   When residuals are penalized differently to fit a model, the weighted
#'   residuals need to be computed accordingly. Say if we use the absolute value
#'   of the residuals instead of the squared values, weighted residuals are
#'   obtained by multiplying the residuals by the weights.
#'
#' @section Computed variables: Data frame with same value of \code{nrow} as
#'   \code{data} as subset for each group containing five numeric variables.
#'   \describe{ \item{x}{x coordinates of observations or x residuals from
#'   fitted values}, \item{y}{y coordinates of observations or y residuals from
#'   fitted values}, \item{x.resid}{residuals from fitted values},
#'   \item{y.resid}{residuals from fitted values}, \item{weights}{the weights
#'   passed as input to lm or those computed by rlm}}.
#'
#'   For \code{orientation = "x"}, the default, \code{stat(y.resid)} is copied
#'   to variable \code{y}, while for \code{orientation = "y"}
#'   \code{stat(x.resid)} is copied to variable \code{x}.
#'
#' @family ggplot statistics for model fits
#'
#' @examples
#' # generate artificial data
#' set.seed(4321)
#' x <- 1:100
#' y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
#' my.data <- data.frame(x, y)
#'
#' # plot residuals from linear model
#' ggplot(my.data, aes(x, y)) +
#'   geom_hline(yintercept = 0, linetype = "dashed") +
#'   stat_fit_residuals(formula = y ~ x)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_hline(yintercept = 0, linetype = "dashed") +
#'   stat_fit_residuals(formula = y ~ x, weighted = TRUE)
#'
#' # plot residuals from linear model with y as explanatory variable
#' ggplot(my.data, aes(x, y)) +
#'   geom_vline(xintercept = 0, linetype = "dashed") +
#'   stat_fit_residuals(formula = x ~ y) +
#'   coord_flip()
#'
#' # give a name to a formula
#' my.formula <- y ~ poly(x, 3, raw = TRUE)
#'
#' # plot residuals from linear model
#' ggplot(my.data, aes(x, y)) +
#'   geom_hline(yintercept = 0, linetype = "dashed") +
#'   stat_fit_residuals(formula = my.formula) +
#'   coord_flip()
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_hline(yintercept = 0, linetype = "dashed") +
#'   stat_fit_residuals(formula = my.formula, resid.type = "response")
#'
#' # plot residuals from robust regression
#' ggplot(my.data, aes(x, y)) +
#'   geom_hline(yintercept = 0, linetype = "dashed") +
#'   stat_fit_residuals(formula = my.formula, method = "rlm")
#'
#' # plot residuals with weights indicated by colour
#' my.data.outlier <- my.data
#' my.data.outlier[6, "y"] <- my.data.outlier[6, "y"] * 10
#' ggplot(my.data.outlier, aes(x, y)) +
#'   stat_fit_residuals(formula = my.formula, method = "rlm",
#'                       mapping = aes(colour = after_stat(weights)),
#'                       show.legend = TRUE) +
#'   scale_color_gradient(low = "red", high = "blue", limits = c(0, 1),
#'                        guide = "colourbar")
#'
#' # plot weighted residuals with weights indicated by colour
#' ggplot(my.data.outlier) +
#'   stat_fit_residuals(formula = my.formula, method = "rlm",
#'                      mapping = aes(x = x,
#'                                    y = stage(start = y, after_stat = y * weights),
#'                                    colour = after_stat(weights)),
#'                      show.legend = TRUE) +
#'   scale_color_gradient(low = "red", high = "blue", limits = c(0, 1),
#'                        guide = "colourbar")
#'
#' # plot residuals from quantile regression (median)
#' ggplot(my.data, aes(x, y)) +
#'   geom_hline(yintercept = 0, linetype = "dashed") +
#'   stat_fit_residuals(formula = my.formula, method = "rq")
#'
#' # plot residuals from quantile regression (upper quartile)
#' ggplot(my.data, aes(x, y)) +
#'   geom_hline(yintercept = 0, linetype = "dashed") +
#'   stat_fit_residuals(formula = my.formula, method = "rq",
#'   method.args = list(tau = 0.75))
#'
#' # inspecting the returned data
#' if (requireNamespace("gginnards", quietly = TRUE)) {
#'   library(gginnards)
#'
#'   ggplot(my.data, aes(x, y)) +
#'    stat_fit_residuals(formula = my.formula, resid.type = "working",
#'                       geom = "debug")
#'
#'   ggplot(my.data, aes(x, y)) +
#'     stat_fit_residuals(formula = my.formula, method = "rlm",
#'                        geom = "debug")
#' }
#'
#' @export
#'
stat_fit_residuals <- function(mapping = NULL,
                               data = NULL,
                               geom = "point",
                               method = "lm",
                               method.args = list(),
                               formula = NULL,
                               resid.type = NULL,
                               weighted = FALSE,
                               position = "identity",
                               na.rm = FALSE,
                               orientation = NA,
                               show.legend = FALSE,
                               inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatFitResiduals, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(method = method,
                  method.args = method.args,
                  formula = formula,
                  resid.type = resid.type,
                  weighted = weighted,
                  na.rm = na.rm,
                  orientation = orientation,
                  ...)
  )
}

#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
residuals_compute_group_fun <- function(data,
                                        scales,
                                        method,
                                        method.args,
                                        formula,
                                        resid.type,
                                        weighted,
                                        orientation) {
  stopifnot(!any(c("formula", "data") %in% names(method.args)))
  if (is.null(data$weight)) {
    data$weight <- 1
  }

  # we guess formula from orientation
  if (is.null(formula)) {
    if (is.na(orientation) || orientation == "x") {
      formula = y ~ x
    } else if (orientation == "y") {
      formula = x ~ y
    }
  }
  # we guess orientation from formula
  if (is.na(orientation)) {
    orientation <- unname(c(x = "y", y = "x")[as.character(formula)[2]])
  }

  # If method was specified as a character string, replace with
  # the corresponding function. Some model fit functions themselves have a
  # method parameter accepting character strings as argument. We support
  # these by splitting strings passed as argument at a colon.
  if (is.character(method)) {
    method <- switch(method,
                     lm = "lm:qr",
                     rlm = "rlm:M",
                     rq = "rq:br",
                     method)
    method.name <- method
    method <- strsplit(x = method, split = ":", fixed = TRUE)[[1]]
    if (length(method) > 1L) {
      fun.method <- method[2]
      method <- method[1]
    } else {
      fun.method <- character()
    }
    if (method == "rq") {
      rlang::check_installed("quantreg", reason = "for `stat_fit_residuals()` with method `rq()`")
    }
    method <- switch(method,
                     lm = stats::lm,
                     rlm = MASS::rlm,
                     rq = quantreg::rq,
                     match.fun(method))
  } else if (is.function(method)) {
    fun.method <- character()
    if (is.name(quote(method))) {
      method.name <- as.character(quote(method))
    } else {
      method.name <- "function"
    }
  }

  fun.args <- list(quote(formula),
                   data = quote(data),
                   weights = data[["weight"]])
  fun.args <- c(fun.args, method.args)
  if (length(fun.method)) {
    fun.args[["method"]] <- fun.method
  }

  # quantreg contains code with partial matching of names!
  # so we silence selectively only these warnings
  withCallingHandlers({
    mf <- do.call(method, args = fun.args)
  }, warning = function(w) {
    if (startsWith(conditionMessage(w), "partial match of") ||
        startsWith(conditionMessage(w), "partial argument match of")) {
      invokeRestart("muffleWarning")
    }
  })

  if (!is.null(resid.type)) {
    if (weighted) {
      if (resid.type != "deviance") {
        warning("Ignoring supplied 'resid.type' as 'weighted = TRUE'")
      }
      resid.args <- list(obj = mf, drop0 = TRUE)
    } else {
      resid.args <- list(object = mf, type = resid.type)
    }
  } else {
    if (weighted) {
      resid.args <- list(obj = mf, drop0 = TRUE)
    } else {
      resid.args <- list(object = mf)
    }
  }
  if (weighted) {
    fit.residuals <- do.call(stats::weighted.residuals, args = resid.args)
  } else {
    fit.residuals <- do.call(stats::residuals, args = resid.args)
  }

  if (exists("w", mf)) {
    weight.vals <- mf[["w"]]
  } else {
    weight.vals <- stats::weights(mf)
    weight.vals <- ifelse(length(weight.vals) == length(fit.residuals),
                          weight.vals,
                          rep_len(NA_real_, length(fit.residuals)))
  }

  if (orientation == "y") {
    data.frame(y = data$y,
               x = fit.residuals,
               x.resid = fit.residuals,
               y.resid = NA_real_,
               weights = weight.vals)
  } else {
    data.frame(x = data$x,
               y = fit.residuals,
               y.resid = fit.residuals,
               x.resid = NA_real_,
               weights = weight.vals)
  }
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFitResiduals <-
  ggplot2::ggproto("StatFitResiduals", ggplot2::Stat,
                   compute_group = residuals_compute_group_fun,
                   required_aes = c("x", "y")
  )
