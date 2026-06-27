#' Residuals and fitted values from model fit
#'
#' Statistic \code{stat_fit_residuals()} fits a model and plots residuals vs.
#' \code{x}. Statistic \code{stat_fit_deviations()} fits a model and and
#' highlighting residuals as segments in a \emph{y} vs. \emph{x} plot. Statistic
#' \code{stat_fit_fitted()} plots the fitted values vs. \emph{x}.
#'
#' @inheritParams stat_poly_eq
#' @param resid.type character passed to \code{\link[stats]{residuals}()} as
#'   argument for \code{type} (defaults to \code{"working"} except if
#'   \code{weighted = TRUE} when it is forced to \code{"deviance"}).
#' @param weighted logical If true weighted residuals will be returned.
#'
#' @aesthetics StatFitResiduals
#' @aesthetics StatFitDeviations
#' @aesthetics StatFitFitted
#'
#' @details \code{stat_fit_deviations()} can be used to highlight residuals as
#'   segments in a plot of a fitted model prediction. This statistic returns the
#'   original \code{x} and \code{y} values and the fitted \code{y} or \code{x}
#'   values depending on the \emph{orientation}, together with prior and
#'   posterior weights.
#'
#'   \code{stat_fit_fitted()} can be used to highlight as points the fitted
#'   values.  This statistic returns the original \code{x} or \code{y} values
#'   and the fitted \code{y} or \code{x} values depending on the
#'   \emph{orientation}.
#'
#'   \code{stat_fit_residuals()} plots residuals as points. It applies to the
#'   fitted model object methods \code{\link[stats]{residuals}()} or
#'   \code{\link[stats]{weighted.residuals}()} depending on the argument passed
#'   to parameter \code{weighted}. This statistic returns the original \code{x}
#'   and \code{y} values and residuals depending on the \emph{orientation},
#'   together with prior and posterior weights.
#'
#' @section Prior and posterior weights:
#'   Two types of weights are possible: prior ones supplied in the call, and
#'   posterior weights (called "robustness weights" in robust regression
#'   methods) implicitly or explicitly used by fit methods to address
#'   heterogeneity of error variance, including the presence of outlier
#'   observations . Not all the supported methods accepts prior weights and
#'   \code{gls()} returns posterior weights that are not in 0..1 like in the
#'   case of most other fits. When not accessible weights are set to 1 when
#'   known to be equal to 1, which is the most frequent case, or to \code{NA}
#'   otherwise.
#'
#'   How weights are applied to residuals depends on the method used to fit the
#'   model. For ordinary least squares (OLS), weights are applied to the squares
#'   of the residuals, so the weighted residuals are obtained by multiplying the
#'   "deviance" residuals by the square root of the weights. When residuals are
#'   penalized differently to fit a model, the weighted residuals need to be
#'   computed accordingly.
#'
#' @note In the case of \code{method = "rq"} quantiles are fixed at \code{tau =
#'   0.5} unless \code{method.args} has length > 0. Parameter \code{orientation}
#'   is redundant as it only affects the default for \code{formula} but is
#'   included for consistency with \code{ggplot2}.
#'
#' @inheritSection stat_poly_eq Model formula and model fitting
#'
#' @inheritSection stat_poly_eq Model fit methods supported
#'
#' @return The returned value is always a data frame with the same number of
#'   rows as the argument passed to \code{data}, except for the case failure of
#'   the model fitting, in which case a data frame with no rows is returned. The
#'   columns returned vary between the three statistics, and for each statistic
#'   depending on the orientation..
#'
#'   To explore the values returned by statistics we suggest the use of
#'   \code{\link[gginnards]{geom_debug_group}()}. Examples are shown below,
#'   where one can also see in addition to the computed values the default
#'   mapping of the fitted values to aesthetics \code{xend} and \code{yend}.
#'
#' @section Variables returned by \code{stat_fit_residuals()}:
#'
#'   \describe{
#'   \item{x}{x coordinates of observations}
#'   \item{y}{y coordinates of observations}
#'   \item{x.resid}{x residuals from fitted values}
#'   \item{y.resid}{y residuals from fitted values}
#'   \item{weights}{the weights
#'   passed as input to \code{lm()}, \code{rlm()}, \code{lmrob()},
#'   or to other model fit functions
#'   using aesthetic weight. More generally the value returned by
#'   method \code{weights()} applied to the model fit object}
#'   \item{posterior.weights}{the "weights"
#'   of the applied minimization criterion relative to those of OLS in
#'   \code{rlm()} or \code{lmrob()} or the divisor weights from
#'    \code{gls()}, \code{lme()} or \code{nlme()}}
#'   }
#'
#' @section Variables returned by \code{stat_fit_deviations()}:
#'
#'   \describe{
#'   \item{x}{x coordinates of observations}
#'   \item{y}{y coordinates of observations}
#'   \item{x.fitted}{x coordinates of fitted values}
#'   \item{y.fitted}{y coordinates of fitted values}
#'   \item{weights}{the weights passed as input to \code{lm()}, \code{rlm()}, or \code{lmrob()},
#'   using aesthetic weight. More generally the value returned by
#'   \code{weights()}}
#'   \item{posterior.weights}{the "weights"
#'   of the applied minimization criterion relative to those of OLS in
#'   \code{rlm()}, or \code{lmrob()}}
#'   }
#'
#' @section Variables returned by \code{stat_fit_fitted()}:
#'
#'   \describe{
#'   \item{x}{x coordinates of observations or fitted}
#'   \item{y}{y coordinates of observations or fitted}
#'   }
#'
#' @inherit stat_poly_eq
#'
#' @seealso \code{\link[stats]{residuals}()} and \code{\link[stats]{weights}()}
#'   and their specializations for the \code{method} used.
#'
#'   Please, see the articles at
#'   \href{https://docs.r4photobiology.info/ggpmisc/}{online-only documentation}
#'   for additional use examples and guidance.
#'
#' @family \emph{statistics} for model fits residuals
#'
#' @examples
#' # generate artificial data
#' set.seed(4321)
#' x <- 1:100
#' y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
#' my.data <- data.frame(x, y)
#'
#' # give a name to a formula
#' my.formula <- y ~ poly(x, 3, raw = TRUE)
#' my.y.formula <- x ~ poly(y, 3, raw = TRUE)
#'
#' # plot residuals from linear model
#' ggplot(my.data, aes(x, y)) +
#'   stat_poly_line(method = "lm", formula = my.formula) +
#'   stat_fit_deviations(method = "lm", formula = my.formula, colour = "red") +
#'   geom_point()
#'
#' # plot residuals from linear model with y as explanatory variable
#' ggplot(my.data, aes(x, y)) +
#'   stat_poly_line(method = "lm", formula = my.y.formula) +
#'   stat_fit_deviations(method = "lm", formula = my.y.formula, colour = "red") +
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
#' my.data.outlier[6, "y"] <- my.data.outlier[6, "y"] * 5
#' ggplot(my.data.outlier, aes(x, y)) +
#'   stat_poly_line(method = MASS::rlm, formula = my.formula) +
#'   stat_fit_deviations(formula = my.formula, method = "rlm",
#'                       mapping = aes(colour = after_stat(posterior.weights)),
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
#' # plot residuals from linear model
#' ggplot(my.data, aes(x, y)) +
#'   geom_hline(yintercept = 0, linetype = "dashed") +
#'   stat_fit_residuals(formula = my.formula)
#'
#' # plot residuals from linear model with y as explanatory variable
#' ggplot(my.data, aes(x, y)) +
#'   geom_vline(xintercept = 0, linetype = "dashed") +
#'   stat_fit_residuals(formula = my.y.formula) +
#'   coord_flip()
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_hline(yintercept = 0, linetype = "dashed") +
#'   stat_fit_residuals(formula = my.formula, resid.type = "response")
#'
#' # plot residuals with weights indicated by colour
#' my.data.outlier <- my.data
#' my.data.outlier[6, "y"] <- my.data.outlier[6, "y"] * 5
#' ggplot(my.data.outlier, aes(x, y)) +
#'   stat_fit_residuals(formula = my.formula, method = "rlm",
#'                       mapping = aes(colour = after_stat(posterior.weights)),
#'                       show.legend = TRUE) +
#'   scale_color_gradient(low = "red", high = "blue", limits = c(0, 1),
#'                        guide = "colourbar")
#'
#' # plot weighted residuals with weights indicated by colour
#' ggplot(my.data.outlier) +
#'   stat_fit_residuals(formula = my.formula, method = "rlm",
#'                      mapping = aes(x = x,
#'                                    y = stage(start = y, after_stat = y * weights),
#'                                    colour = after_stat(posterior.weights)),
#'                      show.legend = TRUE) +
#'   scale_color_gradient(low = "red", high = "blue", limits = c(0, 1),
#'                        guide = "colourbar")
#'
#' # inspecting the returned data
#' gginnards.installed <- requireNamespace("gginnards", quietly = TRUE)
#'
#' if (gginnards.installed)
#'   library(gginnards)
#'
#' # plot, using geom_debug_group() to explore the after_stat data
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     stat_fit_deviations(formula = my.formula,
#'                         geom = "debug_group")
#'
#' if (gginnards.installed)
#'   ggplot(my.data.outlier, aes(x, y)) +
#'     stat_fit_deviations(formula = my.formula, method = "rlm",
#'                         geom = "debug_group")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'    stat_fit_residuals(formula = my.formula, resid.type = "working",
#'                       geom = "debug_group")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     stat_fit_residuals(formula = my.formula, method = "rlm",
#'                        geom = "debug_group")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'    stat_fit_fitted(formula = my.formula,
#'                    geom = "debug_group")
#'
#' @export
#'
stat_fit_residuals <- function(mapping = NULL,
                               data = NULL,
                               geom = "point",
                               position = "identity",
                               ...,
                               orientation = NA,
                               method = "lm",
                               method.args = list(),
                               n.min = 2L,
                               formula = NULL,
                               fit.seed = NA,
                               resid.type = NULL,
                               weighted = FALSE,
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
    stat = StatFitResiduals,
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
                                        method.name,
                                        method.args = list(),
                                        n.min = 2L,
                                        formula = y ~ x,
                                        fit.seed = NA,
                                        resid.type = NULL,
                                        weighted = FALSE,
                                        flipped_aes = NA,
                                        orientation = "x") {

  # we flip the model formula, not the data

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

  fit.residuals <- extract_residuals(fm,
                                     resid.type = resid.type,
                                     weighted = weighted)
  weights.ls <- extract_weights(fm, n.row = nrow(data))

  if (orientation == "y") {
    z <- data.frame(y = data$y,
                    x = fit.residuals,
                    x.resid = fit.residuals,
                    y.resid = NA_real_,
                    weights = weights.ls[["weight.vals"]],
                    posterior.weights = weights.ls[["rob.weight.vals"]])
  } else {
    z <- data.frame(x = data$x,
                    y = fit.residuals,
                    y.resid = fit.residuals,
                    x.resid = NA_real_,
                    weights = weights.ls[["weight.vals"]],
                    posterior.weights = weights.ls[["rob.weight.vals"]])
  }

  z$flipped_aes <- flipped_aes
  # no need to flip the results, but we record the flipping

  show_colnames(z, stat.name = "stat_fit_residuals")

  z
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFitResiduals <-
  ggplot2::ggproto("StatFitResiduals", ggplot2::Stat,
                   setup_params = function(data, params) {
                     params[["flipped_aes"]] <-
                       ggplot2::has_flipped_aes(data, params, ambiguous = TRUE)
                     params
                   },
                   extra_params = c("na.rm", "orientation"),
                   compute_group = residuals_compute_group_fun,
                   dropped_aes = "weight",
                   required_aes = c("x", "y")
  )
