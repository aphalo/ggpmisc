#' Residuals from model fit as segments
#'
#' \code{stat_fit_deviations} fits a model and returns fitted values and
#' residuals ready for highlighting them as segments in a \emph{y} vs. \emph{x}
#' plot.
#'
#' @inheritParams stat_poly_line
#'
#' @aesthetics StatFitDeviations
#' @aesthetics StatFitFitted
#'
#' @details \code{stat_fit_deviations()} can be used to automatically highlight
#'   residuals as segments in a plot of a fitted model equation. This stat only
#'   returns the fitted values and observations, the prediction and its
#'   confidence need to be separately added to the plot when desired. Thus, to
#'   make sure that the same model formula is used in all plot layers, it is
#'   best to save the formula as an object and supply this object as argument to
#'   the different statistics.
#'
#'   \code{stat_fit_fitted()} only returns the original \code{x} and the fitted
#'   values, by default mapped to \code{y} and can be used to add them to a
#'   plot. e.g., as points. Fitted values are available for all model fit
#'   methods.
#'
#' @inheritSection stat_fit_residuals Prior and posterior weights
#'
#' @note In the case of \code{method = "rq"} quantiles are fixed at \code{tau =
#'   0.5} unless \code{method.args} has length > 0. Parameter \code{orientation}
#'   is redundant as it only affects the default for \code{formula} but is
#'   included for consistency with \code{ggplot2}.
#'
#' @inheritSection stat_poly_line Model formula and model fitting
#'
#' @inheritSection stat_poly_line Model fit methods supported
#'
#' @section Computed variables: Data frame with same \code{nrow} as \code{data}
#'   as subset for each group containing five numeric variables.
#'
#'   \describe{
#'   \item{x}{x coordinates of observations}
#'   \item{x.fitted}{x coordinates of fitted values}
#'   \item{y}{y coordinates of observations}
#'   \item{y.fitted}{y coordinates of fitted values}
#'   \item{weights}{the weights passed as input to \code{lm()}, \code{rlm()}, or \code{lmrob()},
#'   using aesthetic weight. More generally the value returned by
#'   \code{weights()}}
#'   \item{robustness.weights}{the "weights"
#'   of the applied minimization criterion relative to those of OLS in
#'   \code{rlm()}, or \code{lmrob()}}
#'   }
#'
#'   To explore the values returned by this statistic we suggest the use of
#'   \code{\link[gginnards]{geom_debug}()}. An example is shown below, where one
#'   can also see in addition to the computed values the default mapping of the
#'   fitted values to aesthetics \code{xend} and \code{yend}.
#'
#' @inherit stat_poly_line seealso
#'
#' @family 'ggpmisc' statistics for model fits
#'
#' @examples
#' # generate artificial data
#' library(MASS)
#'
#' set.seed(4321)
#' x <- 1:100
#' y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
#' my.data <- data.frame(x, y)
#'
#' # plot residuals from linear model
#' ggplot(my.data, aes(x, y)) +
#'   stat_poly_line(method = "lm", formula = y ~ x) +
#'   stat_fit_deviations(method = "lm", formula = y ~ x, colour = "red") +
#'   geom_point()
#'
#' # plot residuals from linear model with y as explanatory variable
#' ggplot(my.data, aes(x, y)) +
#'   stat_poly_line(method = "lm", formula = x ~ y) +
#'   stat_fit_deviations(method = "lm", formula = x ~ y, colour = "red") +
#'   geom_point()
#'
#' # both regressions and their deviations
#' ggplot(my.data, aes(x, y)) +
#'   stat_poly_line(method = "lm", formula = y ~ x) +
#'   stat_fit_deviations(method = "lm", formula = y ~ x, colour = "red") +
#'   stat_poly_line(method = "lm", formula = x ~ y) +
#'   stat_fit_deviations(method = "lm", formula = x ~ y, colour = "orange") +
#'   geom_point()
#'
#' # give a name to a formula
#' my.formula <- y ~ poly(x, 3, raw = TRUE)
#'
#' # plot linear regression
#' ggplot(my.data, aes(x, y)) +
#'   stat_poly_line(method = "lm", formula = my.formula) +
#'   stat_fit_deviations(formula = my.formula, colour = "red") +
#'   geom_point()
#'
#' ggplot(my.data, aes(x, y)) +
#'   stat_poly_line(formula = my.formula, method = "lm") +
#'   stat_fit_deviations(formula = my.formula, method = "lm", colour = "red") +
#'   geom_point()
#'
#' # plot robust regression
#' ggplot(my.data, aes(x, y)) +
#'   stat_poly_line(formula = my.formula, method = "rlm") +
#'   stat_fit_deviations(formula = my.formula, method = "rlm", colour = "red") +
#'   geom_point()
#'
#' # plot robust regression with weights indicated by colour
#' my.data.outlier <- my.data
#' my.data.outlier[6, "y"] <- my.data.outlier[6, "y"] * 10
#' ggplot(my.data.outlier, aes(x, y)) +
#'   stat_poly_line(method = MASS::rlm, formula = my.formula) +
#'   stat_fit_deviations(formula = my.formula, method = "rlm",
#'                       mapping = aes(colour = after_stat(robustness.weights)),
#'                       show.legend = TRUE) +
#'   scale_color_gradient(low = "red", high = "blue", limits = c(0, 1),
#'                        guide = "colourbar") +
#'   geom_point()
#'
#' # plot quantile regression (= median regression)
#' ggplot(my.data, aes(x, y)) +
#'   stat_quantile(formula = my.formula, quantiles = 0.5) +
#'   stat_fit_deviations(formula = my.formula, method = "rq", colour = "red") +
#'   geom_point()
#'
#' # plot quantile regression (= "quartile" regression)
#' ggplot(my.data, aes(x, y)) +
#'   stat_quantile(formula = my.formula, quantiles = 0.75) +
#'   stat_fit_deviations(formula = my.formula, colour = "red",
#'                       method = "rq", method.args = list(tau = 0.75)) +
#'   geom_point()
#'
#' # inspecting the returned data with geom_debug_group()
#' gginnards.installed <- requireNamespace("gginnards", quietly = TRUE)
#'
#' if (gginnards.installed)
#'   library(gginnards)
#'
#' # plot, using geom_debug_group() to explore the after_stat data
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     stat_poly_line(method = "lm", formula = my.formula) +
#'     stat_fit_deviations(formula = my.formula,
#'                         geom = "debug_group") +
#'     geom_point()
#'
#' if (gginnards.installed)
#'   ggplot(my.data.outlier, aes(x, y)) +
#'     stat_poly_line(method = "rlm", formula = my.formula) +
#'     stat_fit_deviations(formula = my.formula, method = "rlm",
#'                         geom = "debug_group") +
#'     geom_point()
#'
#' @export
#'
stat_fit_deviations <- function(mapping = NULL,
                                data = NULL,
                                geom = "segment",
                                position = "identity",
                                ...,
                                orientation = NA,
                                method = "lm",
                                method.args = list(),
                                n.min = 2L,
                                formula = NULL,
                                fit.seed = NA,
                                na.rm = FALSE,
                                show.legend = TRUE,
                                inherit.aes = TRUE) {

  if (is.character(method)) {
    method <- trimws(method, which = "both")
    method.name <- method
  } else if (is.function(method)) {
    method.name <- deparse(substitute(method))
    if (grepl("^function[ ]*[(]", method.name[1])) {
      method.name <- "function"
    }
  } else {
    method.name <- "missing"
  }

  temp <- guess_orientation(orientation = orientation,
                            formula = formula,
                            default.formula = y ~ x,
                            formula.on.x = FALSE)
  orientation <- temp[["orientation"]]
  formula <- temp[["formula"]]

  ggplot2::layer(
    stat = StatFitDeviations,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params =
      rlang::list2(method = method,
                   method.name = method.name,
                   method.args = method.args,
                   n.min = n.min,
                   formula = formula,
                   fit.seed = fit.seed,
                   na.rm = na.rm,
                   orientation = orientation,
                   ...)
  )
}

# Define here to avoid a note in check as the imports are not seen by checks
# when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
deviations_compute_group_fun <- function(data,
                                         scales,
                                         method,
                                         method.name,
                                         method.args = list(),
                                         n.min = 2L,
                                         formula = y ~ x,
                                         fit.seed = NA,
                                         orientation = "x") {

  temp.ls <- fit_models_internal(data = data,
                                 method = method,
                                 method.name = method.name,
                                 method.args = method.args,
                                 n.min = n.min,
                                 formula = formula,
                                 fit.seed = fit.seed,
                                 orientation = orientation)
  if (!length(temp.ls) || !length(temp.ls[["fm"]])) {
    # An empty data.frame results in no plot layer when passed to geoms
    return(data.frame())
  }
  fm <- temp.ls[["fm"]]
  method.name <- temp.ls[["method.name"]]
  method.args <- temp.ls[["method.args"]]


  fitted.vals <- extract_fitted(fm, n.row = nrow(data))
  weights.ls <- extract_weights(fm, n.row = nrow(data))

  if (orientation == "y") {
    data.frame(x = data$x,
               y = data$y,
               x.fitted = fitted.vals,
               y.fitted = data$y,
               weights = weights.ls[["weight.vals"]],
               robustness.weights = weights.ls[["rob.weight.vals"]],
               hjust = 0)
  } else {
    data.frame(x = data$x,
               y = data$y,
               x.fitted = data$x,
               y.fitted = fitted.vals,
               weights = weights.ls[["weight.vals"]],
               robustness.weights = weights.ls[["rob.weight.vals"]],
               hjust = 0)
  }
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFitDeviations <-
  ggplot2::ggproto("StatFitDeviations", ggplot2::Stat,
                   extra_params = c("na.rm", "orientation"),
                   compute_group = deviations_compute_group_fun,
                   dropped_aes = "weight",
                   default_aes =
                     ggplot2::aes(xend = after_stat(x.fitted),
                                  yend = after_stat(y.fitted)),
                   required_aes = c("x", "y")
  )

#' @rdname stat_fit_deviations
#'
#' @export
#'
stat_fit_fitted <- function(mapping = NULL,
                            data = NULL,
                            geom = "point",
                            position = "identity",
                            orientation = NA,
                            ...,
                            method = "lm",
                            method.args = list(),
                            n.min = 2L,
                            formula = NULL,
                            fit.seed = NA,
                            na.rm = FALSE,
                            show.legend = FALSE,
                            inherit.aes = TRUE) {

  if (is.character(method)) {
    method <- trimws(method, which = "both")
    method.name <- method
  } else if (is.function(method)) {
    method.name <- deparse(substitute(method))
    if (grepl("^function[ ]*[(]", method.name[1])) {
      method.name <- "function"
    }
  } else {
    method.name <- "missing"
  }

  temp <- guess_orientation(orientation = orientation,
                            formula = formula,
                            default.formula = y ~ x,
                            formula.on.x = FALSE)
  orientation <- temp[["orientation"]]
  formula <-  temp[["formula"]]

  ggplot2::layer(
    stat = StatFitFitted, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params =
      rlang::list2(method = method,
                   method.name = method.name,
                   method.args = method.args,
                   n.min = n.min,
                   formula = formula,
                   fit.seed = fit.seed,
                   na.rm = na.rm,
                   orientation = orientation,
                   ...)
  )
}

# Define here to avoid a note in check as the imports are not seen by checks
# when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
fitted_compute_group_fun <- function(data,
                                     scales,
                                     method,
                                     method.name,
                                     method.args,
                                     n.min = 2L,
                                     formula =  y ~ x,
                                     fit.seed = NA,
                                     orientation = "x",
                                     return.fitted = FALSE) {

  temp.ls <- fit_models_internal(data = data,
                                 method = method,
                                 method.name = method.name,
                                 method.args = method.args,
                                 n.min = n.min,
                                 formula = formula,
                                 fit.seed = fit.seed,
                                 orientation = orientation)
  if (!length(temp.ls) || !length(temp.ls[["fm"]])) {
    # An empty data.frame results in no plot layer when passed to geoms
    return(data.frame())
  }
  fm <- temp.ls[["fm"]]
  method.name <- temp.ls[["method.name"]]
  method.args <- temp.ls[["method.args"]]

  fitted.vals <- extract_fitted(fm, n.row = nrow(data))

  if (orientation == "y") {
    data.frame(x = fitted.vals,
               y = data$y)
  } else {
    data.frame(x = data$x,
               y = fitted.vals)
  }
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#'
#' @export
#'
StatFitFitted <-
  ggplot2::ggproto("StatFitFitted", ggplot2::Stat,
                   extra_params = c("na.rm", "orientation"),
                   compute_group = fitted_compute_group_fun,
                   required_aes = c("x", "y")
  )
