#' Residuals from a model fit
#'
#' \code{stat_fit_residuals} fits a linear model and returns
#'    residuals ready to be plotted as points.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}}. Only needs
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
#' @param n.min integer Minimum number of distinct values in the explanatory
#'   variable (on the rhs of formula) for fitting to the attempted.
#' @param formula a "formula" object. Using aesthetic names instead of
#'   original variable names.
#' @param fit.seed RNG seed argument passed to \code{\link[base:Random]{set.seed}()}.
#'   Defaults to \code{NA}, which means that \code{set.seed()} will not be
#'   called.
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
#'   residuals need to be computed accordingly. Two types of weights are
#'   possible: prior ones supplied in the call, and "robustness weights"
#'   implicitly or explicitly used by robust regression methods. Not all the
#'   supported methods return prior weights and \code{gls()} does not return
#'   weights of any type. When not available weights are set to NA unless when
#'   known to be equal to 1.
#'
#' @section Computed variables: Data frame with same value of \code{nrow} as
#'   \code{data} as subset for each group containing six numeric variables.
#'   \describe{ \item{x}{x coordinates of observations or x residuals from
#'   fitted values}, \item{y}{y coordinates of observations or y residuals from
#'   fitted values}, \item{x.resid}{residuals from fitted values},
#'   \item{y.resid}{residuals from fitted values}, \item{weights}{the weights
#'   passed as input to \code{lm()}, \code{rlm()}, or \code{lmrob()},
#'   using aesthetic weight. More generally the value returned by
#'   \code{weights()} }, \item{robustness.weights}{the "weights"
#'   of the applied minimization criterion relative to those of OLS in
#'   \code{rlm()}, or \code{lmrob()}} }.
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
                               method = "lm",
                               method.args = list(),
                               n.min = 2L,
                               formula = NULL,
                               fit.seed = NA,
                               resid.type = NULL,
                               weighted = FALSE,
                               na.rm = FALSE,
                               orientation = NA,
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
  formula <- temp[["formula"]]

  ggplot2::layer(
    stat = StatFitResiduals, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
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
  stopifnot(!any(c("formula", "data") %in% names(method.args)))
  if (is.null(data$weight)) {
    data$weight <- 1
  }

  if (length(unique(data[[orientation]])) < n.min) {
    return(data.frame())
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
                     lqs = "lqs:lqs",
                     gls = "gls:REML",
                     method)
    method.name <- method
    method <- strsplit(x = method, split = ":", fixed = TRUE)[[1]]
    if (length(method) > 1L) {
      fun.method <- method[2]
      method <- method[1]
    } else {
      fun.method <- character()
    }

    method <- switch(method,
                     lm = stats::lm,
                     rlm = MASS::rlm,
                     rq = quantreg::rq,
                     lqs = MASS::lqs,
                     gls = nlme::gls,
                     match.fun(method))
  } else if (is.function(method)) {
    fun.method <- character()
  }

  if (exists("weight", data) && !all(data[["weight"]] == 1)) {
    stopifnot("A mapping to 'weight' and a named argument 'weights' cannot co-exist" =
                !"weights" %in% method.args)
    fun.args <- list(formula = quote(formula),
                     data = quote(data),
                     weights = data[["weight"]])
  } else {
    fun.args <- list(formula = quote(formula),
                     data = quote(data))
  }
  fun.args <- c(fun.args, method.args)

  if (length(fun.method)) {
    fun.args[["method"]] <- fun.method
  }

  # gls() parameter for formula is called model
  if (grepl("gls", method.name)) {
    names(fun.args)[1] <- "model"
  }

  if (!is.na(fit.seed)) {
    set.seed(fit.seed)
  }
  # quantreg contains code with partial matching of names!
  # so we silence selectively only these warnings
  withCallingHandlers({
    fm <- do.call(method, args = fun.args)
  }, warning = function(w) {
    if (startsWith(conditionMessage(w), "partial match of") ||
        startsWith(conditionMessage(w), "partial argument match of")) {
      invokeRestart("muffleWarning")
    }
  })

  if (!length(fm) || (is.atomic(fm) && is.na(fm))) {
    return(data.frame())
  } else if (!(inherits(fm, "lm") || inherits(fm, "lmrob") ||
               inherits(fm, "gls") || inherits(fm, "lqs") ||
               inherits(fm, "lts") || inherits(fm, "sma"))) {
    message("Method \"", method.name,
            "\" did not return a ",
            "\"lm\", \"lmrob\", \"lqs\", \"lts\", \"gls\" or \"sma\" ",
            "object, possible failure ahead.")
  }

  if (!is.null(resid.type)) {
    if (weighted) {
      if (resid.type != "deviance") {
        warning("Ignoring supplied 'resid.type' as 'weighted = TRUE'")
      }
      resid.args <- list(obj = fm, drop0 = TRUE)
    } else {
      resid.args <- list(object = fm, type = resid.type)
    }
  } else {
    if (weighted) {
      resid.args <- list(obj = fm, drop0 = TRUE)
    } else {
      resid.args <- list(object = fm)
    }
  }
  if (weighted) {
    fit.residuals <- do.call(stats::weighted.residuals, args = resid.args)
  } else {
    fit.residuals <- do.call(stats::residuals, args = resid.args)
  }

  if (inherits(fm, "lmrob")) {
    rob.weight.vals <- stats::weights(fm, type = "robustness")
    weight.vals <- stats::weights(fm, type = "prior")
    if (!length(weight.vals)) {
      weight.vals <- rep_len(1, nrow(data))
    }
  } else if (inherits(fm, "lts")) {
    rob.weight.vals <- fm[["lts.wt"]]
    weight.vals <- rep_len(1, nrow(data))
  } else if (inherits(fm, "rlm")) {
    rob.weight.vals <- fm[["w"]]
    weight.vals <- stats::weights(fm)
  } else if (inherits(fm, "lqs")) {
    ## what does fm$bestone contain?
    warning("Returned \"robustness weights\" are likely incorrect")
    rob.weight.vals <- rep_len(0, nrow(data))
    rob.weight.vals[fm[["bestone"]]] <- 1
    weight.vals <- rep_len(1, nrow(data))
  } else {
    rob.weight.vals <- rep(NA_real_, nrow(data))
    try(weight.vals <- stats::weights(fm))
    if (inherits(weight.vals, "try-error") ||
        length(weight.vals) != length(fit.residuals)) {
      if (exists("weights", fm) &&  # defensive
          length(fm[["weights"]]) == length(fit.residuals)) {
        weight.vals <- fm[["weights"]]
      } else {
        weight.vals <- rep_len(NA_real_, nrow(data))
      }
    }
   }

  if (orientation == "y") {
    data.frame(y = data$y,
               x = fit.residuals,
               x.resid = fit.residuals,
               y.resid = NA_real_,
               weights = weight.vals,
               robustness.weights = rob.weight.vals)
  } else {
    data.frame(x = data$x,
               y = fit.residuals,
               y.resid = fit.residuals,
               x.resid = NA_real_,
               weights = weight.vals,
               robustness.weights = rob.weight.vals)
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
