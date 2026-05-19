#' Residuals from a model fit
#'
#' \code{stat_fit_residuals} fits a model and returns residuals ready to be
#' plotted vs. \emph{x}.
#'
#' @inheritParams stat_poly_eq
#'
#' @param resid.type character passed to \code{\link[stats]{residuals}()} as
#'   argument for \code{type} (defaults to \code{"working"} except if
#'   \code{weighted = TRUE} when it is forced to \code{"deviance"}).
#' @param weighted logical If true weighted residuals will be returned.
#'
#' @aesthetics StatFitResiduals
#'
#' @details This stat can be used to automatically plot residuals as points. It
#'   applies to the fitted model object methods \code{\link[stats]{residuals}()}
#'   or \code{\link[stats]{weighted.residuals}()} depending on the argument
#'   passed to parameter \code{weighted}.
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
#' @inheritSection stat_poly_eq Model formula and model fitting
#'
#' @inheritSection stat_poly_eq Model fit methods supported
#'
#' @section Computed variables:
#'   Data frame with same value of \code{nrow} as
#'   \code{data} as subset for each group containing six numeric variables.
#'
#'   \describe{
#'   \item{x}{x coordinates of observations or x residuals from
#'   fitted values}
#'   \item{y}{y coordinates of observations or y residuals from
#'   fitted values}, \item{x.resid}{residuals from fitted values}
#'   \item{y.resid}{residuals from fitted values},
#'   \item{weights}{the weights
#'   passed as input to \code{lm()}, \code{rlm()}, or \code{lmrob()},
#'   using aesthetic weight. More generally the value returned by
#'   \code{weights()} }
#'   \item{robustness.weights}{the "weights"
#'   of the applied minimization criterion relative to those of OLS in
#'   \code{rlm()}, or \code{lmrob()}}
#'   }
#'
#'   For \code{orientation = "x"}, the default, \code{stat(y.resid)} is copied
#'   to variable \code{y}, while for \code{orientation = "y"}
#'   \code{stat(x.resid)} is copied to variable \code{x}.
#'
#' @inherit stat_poly_line seealso
#'
#' @family 'ggpmisc' statistics for model fits
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
#' gginnards.installed <- requireNamespace("gginnards", quietly = TRUE)
#'
#' if (gginnards.installed)
#'   library(gginnards)
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

  fit.residuals <- extract_residuals(fm,
                                     resid.type = resid.type,
                                     weighted = weighted)
  weights.ls <- extract_weights(fm, n.row = nrow(data))

  if (orientation == "y") {
    data.frame(y = data$y,
               x = fit.residuals,
               x.resid = fit.residuals,
               y.resid = NA_real_,
               weights = weights.ls[["weight.vals"]],
               robustness.weights = weights.ls[["rob.weight.vals"]])
  } else {
    data.frame(x = data$x,
               y = fit.residuals,
               y.resid = fit.residuals,
               x.resid = NA_real_,
               weights = weights.ls[["weight.vals"]],
               robustness.weights = weights.ls[["rob.weight.vals"]])
  }
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFitResiduals <-
  ggplot2::ggproto("StatFitResiduals", ggplot2::Stat,
                   compute_group = residuals_compute_group_fun,
                   dropped_aes = "weight",
                   required_aes = c("x", "y")
  )
