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
#' @param method function or character If character, "lm", "rlm", and "rq"
#'   are implemented. If a function, it must support parameters \code{formula}
#'   and \code{data}.
#' @param method.args named list with additional arguments.
#' @param formula a "formula" object. Using aesthetic names instead of
#'   original variable names.
#' @param resid.type character passed to \code{residuals()} as argument for
#'   \code{type}.
#' @param orientation character Either "x" or "y" controlling the default for
#'   \code{formula}.
#'
#' @details This stat can be used to automatically plot residuals as points in a
#'   plot. At the moment it supports only linear models fitted with function
#'   \code{lm()}. This stat only generates the residuals.
#'
#'   A ggplot statistic receives as data a data frame that is not the one passed
#'   as argument by the user, but instead a data frame with the variables mapped
#'   to aesthetics. In other words, it respects the grammar of graphics and
#'   consequently within the model \code{formula} names of
#'   aesthetics like $x$ and $y$ should be used intead of the original variable
#'   names, while data is automatically passed the data frame. This helps ensure
#'   that the model is fitted to the same data as plotted in other layers.
#'
#' @note Parameter \code{orientation} is redundant as it only affects the default
#'   for \code{formula} but is included for consistency with
#'   \code{ggplot2}.
#'
#' @section Computed variables: Data frame with same \code{nrow} as \code{data}
#'   as subset for each group containing five numeric variables. \describe{
#'   \item{x}{x coordinates of observations} \item{y.resid}{residuals from
#'   fitted values} \item{y.resid.abs}{absolute residuals from the fit}}.
#'
#'   By default \code{stat(y.resid)} is mapped to the \code{y} aesthetic.
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
#' library(gginnards) # needed for geom_debug()
#'
#' # print to the console the returned data
#' ggplot(my.data, aes(x, y)) +
#'   stat_fit_residuals(formula = my.formula, resid.type = "working",
#'                      geom = "debug")
#'
#' ggplot(my.data, aes(x, y)) +
#'   stat_fit_residuals(formula = my.formula, method = "rlm",
#'                      geom = "debug")
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

  if (is.function(method)) {
    fun <- method
  } else if (is.character(method)) {
    if (method == "rq" && length(method.args) == 0) {
      method.args <- list(tau = 0.5)
    }
    fun <- switch(method,
                  lm = stats::lm,
                  rlm = MASS::rlm,
#                  lqs = MASS::lqs,
                  rq = quantreg::rq,
                  stop("Method '", method, "' not yet implemented.")
    )
  } else {
    stop("Method '", method, "' not yet implemented.")
  }

  mf <- do.call(fun,
                args = c(list(formula = formula, data = data,
                              weights = quote(weight)),
                         method.args))

  if (!is.null(resid.type)) {
    resid.args <- list(object = mf, type = resid.type)
  } else {
    resid.args <- list(object = mf)
  }
  fit.residuals <- do.call(stats::residuals, args = resid.args)

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
