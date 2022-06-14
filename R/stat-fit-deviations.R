#' Residuals from model fit as segments
#'
#' \code{stat_fit_deviations} fits a linear model and returns fitted values and
#' residuals ready to be plotted as segments.
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
#' @param method function or character If character, "lm", "rlm", "lqs", "rq"
#'   and the name of a function to be matched, possibly followed by the fit
#'   function's \code{method} argument separated by a colon (e.g.
#'   \code{"rq:br"}). Functions implementing methods must accept arguments to
#'   parameters \code{formula}, \code{data}, \code{weights} and \code{method}. A
#'   \code{fitted()} method must exist for the returned model fit object class.
#' @param method.args named list with additional arguments.
#' @param formula a "formula" object. Using aesthetic names instead of
#'   original variable names.
#' @param orientation character Either "x" or "y" controlling the default for
#'   \code{formula}.
#'
#' @details This stat can be used to automatically highlight residuals as
#'   segments in a plot of a fitted model equation. This stat only generates the
#'   residuals, the predicted values need to be separately added to the plot, so
#'   to make sure that the same model formula is used in all steps it is best to
#'   save the formula as an object and supply this object as argument to the
#'   different statistics.
#'
#'   A ggplot statistic receives as data a data frame that is not the one passed
#'   as argument by the user, but instead a data frame with the variables mapped
#'   to aesthetics. In other words, it respects the grammar of graphics and
#'   consequently within the model \code{formula} names of aesthetics like $x$
#'   and $y$ should be used instead of the original variable names. This helps
#'   ensure that the model is fitted to the same data as plotted in other
#'   layers.
#'
#' @note In the case of \code{method = "rq"} quantiles are fixed at \code{tau =
#'   0.5} unless \code{method.args} has length > 0. Parameter \code{orientation}
#'   is redundant as it only affects the default for \code{formula} but is
#'   included for consistency with \code{ggplot2}.
#'
#' @section Computed variables: Data frame with same \code{nrow} as \code{data}
#'   as subset for each group containing five numeric variables. \describe{
#'   \item{x}{x coordinates of observations} \item{y.fitted}{x coordinates of
#'   fitted values} \item{y}{y coordinates of observations} \item{y.fitted}{y
#'   coordinates of fitted values}}
#'
#'   To explore the values returned by this statistic we suggest the use of
#'   \code{\link[gginnards]{geom_debug}}. An example is shown below, where one
#'   can also see in addition to the computed values the default mapping of the
#'   fitted values to aesthetics \code{xend} and \code{yend}.
#'
#' @family ggplot statistics for model fits
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
#'   geom_smooth(method = "lm", formula = y ~ x) +
#'   stat_fit_deviations(method = "lm", formula = y ~ x, colour = "red") +
#'   geom_point()
#'
#' # plot residuals from linear model with y as explanatory variable
#' ggplot(my.data, aes(x, y)) +
#'   geom_smooth(method = "lm", formula = y ~ x, orientation = "y") +
#'   stat_fit_deviations(method = "lm", formula = x ~ y, colour = "red") +
#'   geom_point()
#'
#' # as above using orientation
#' ggplot(my.data, aes(x, y)) +
#'   geom_smooth(method = "lm", orientation = "y") +
#'   stat_fit_deviations(orientation = "y", colour = "red") +
#'   geom_point()
#'
#' # both regressions and their deviations
#' ggplot(my.data, aes(x, y)) +
#'   geom_smooth(method = "lm") +
#'   stat_fit_deviations(colour = "blue") +
#'   geom_smooth(method = "lm", orientation = "y", colour = "red") +
#'   stat_fit_deviations(orientation = "y", colour = "red") +
#'   geom_point()
#'
#' # give a name to a formula
#' my.formula <- y ~ poly(x, 3, raw = TRUE)
#'
#' # plot linear regression
#' ggplot(my.data, aes(x, y)) +
#'   geom_smooth(method = "lm", formula = my.formula) +
#'   stat_fit_deviations(formula = my.formula, colour = "red") +
#'   geom_point()
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_smooth(method = "lm", formula = my.formula) +
#'   stat_fit_deviations(formula = my.formula, method = stats::lm, colour = "red") +
#'   geom_point()
#'
#' # plot robust regression
#' ggplot(my.data, aes(x, y)) +
#'   stat_smooth(method = "rlm", formula = my.formula) +
#'   stat_fit_deviations(formula = my.formula, method = "rlm", colour = "red") +
#'   geom_point()
#'
#' # plot robust regression with weights indicated by colour
#' my.data.outlier <- my.data
#' my.data.outlier[6, "y"] <- my.data.outlier[6, "y"] * 10
#' ggplot(my.data.outlier, aes(x, y)) +
#'   stat_smooth(method = MASS::rlm, formula = my.formula) +
#'   stat_fit_deviations(formula = my.formula, method = "rlm",
#'                       mapping = aes(colour = after_stat(weights)),
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
#' # inspecting the returned data
#' if (requireNamespace("gginnards", quietly = TRUE)) {
#'   library(gginnards)
#'
#' # plot, using geom_debug() to explore the after_stat data
#'   ggplot(my.data, aes(x, y)) +
#'     geom_smooth(method = "lm", formula = my.formula) +
#'     stat_fit_deviations(formula = my.formula, geom = "debug") +
#'     geom_point()
#'
#'   ggplot(my.data.outlier, aes(x, y)) +
#'     stat_smooth(method = MASS::rlm, formula = my.formula) +
#'     stat_fit_deviations(formula = my.formula, method = "rlm", geom = "debug") +
#'     geom_point()
#' }
#'
#' @export
#'
stat_fit_deviations <- function(mapping = NULL, data = NULL, geom = "segment",
                               method = "lm",
                               method.args = list(),
                               formula = NULL,
                               position = "identity",
                               na.rm = FALSE,
                               orientation = NA,
                               show.legend = FALSE,
                               inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatFitDeviations, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(method = method,
                  method.args = method.args,
                  formula = formula,
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
                                         method.args,
                                         formula,
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
                     lqs = "lqs:lts",
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
      rlang::check_installed("quantreg", reason = "for `stat_fit_deviations()` with method `rq()`")
    }

    method <- switch(method,
                     lm = stats::lm,
                     rlm = MASS::rlm,
                     lqs = MASS::lqs,
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

  fitted.vals <- stats::fitted(mf)
  if (exists("w", mf)) {
    weight.vals <- mf[["w"]]
  } else {
    weight.vals <- stats::weights(mf)
    weight.vals <- ifelse(length(weight.vals) == length(fitted.vals),
                          weight.vals,
                          rep_len(NA_real_, length(fitted.vals)))
  }
  if (orientation == "y") {
    data.frame(x = data$x,
               y = data$y,
               x.fitted = fitted.vals,
               y.fitted = data$y,
               weights = weight.vals,
               hjust = 0)
  } else {
    data.frame(x = data$x,
               y = data$y,
               x.fitted = data$x,
               y.fitted = fitted.vals,
               weights = weight.vals,
               hjust = 0)
  }
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFitDeviations <-
  ggplot2::ggproto("StatFitDeviations", ggplot2::Stat,
                   compute_group = deviations_compute_group_fun,
                   default_aes =
                     ggplot2::aes(xend = after_stat(x.fitted),
                                  yend = after_stat(y.fitted)),
                   required_aes = c("x", "y")
  )
