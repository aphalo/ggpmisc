#' Predicted line from a model fit
#'
#' \code{stat_poly_line()} fits a polynomial or other models, by default with
#' \code{stats::lm()}, but alternatively using robust regression or generalized
#' least squares. Predicted values and a confidence band, if possible, are
#' computed and, by default, plotted.
#'
#' @details This statistic is similar to \code{\link[ggplot2]{stat_smooth}()}
#'   but has different defaults and supports a different set of model fit
#'   functions. It is similar to \code{\link{stat_poly_eq}()} but adds a fitted
#'   curve to a plot instead of textual labels for \eqn{R^2}, adjusted
#'   \eqn{R^2}, the fitted model equation, \eqn{P}, and other parameters from a
#'   fitted model. Parameters and default arguments are consistent between
#'   \code{stat_poly_line()} and \code{stat_poly_eq()}.
#'
#'   As some model fitting approaches depend on the RNG
#'   (pseudo-Random Number Generator), \code{fit.seed} if not \code{NA} is used
#'   as argument in a call to \code{\link[base:Random]{set.seed}()} immediately
#'   ahead of model fitting.
#'
#'   The minimum number of observations with distinct values in the explanatory
#'   variable can be set through parameter \code{n.min}. The default \code{n.min
#'   = 2L} is the smallest suitable for method \code{"lm"} but too small for
#'   method \code{"rlm"} for which \code{n.min = 3L} is needed. Anyway, model
#'   fits with very few observations are of little interest and using larger
#'   values of \code{n.min} than the default is wise.
#'
#'   With method \code{"lm"}, singularity results in terms being dropped with a
#'   message if more numerous than can be fitted with a singular (exact) fit. In
#'   this case and if the model results in a perfect fit due to low number of
#'   observation, estimates for various parameters are \code{NaN} or \code{NA}.
#'   With methods other than \code{"lm"}, the model fit functions simply fail in
#'   case of singularity, e.g., singular fits are not implemented in
#'   \code{"rlm"}.
#'
#' @inheritParams fit_models_internal
#'
#' @param data A layer specific dataset, only needed if you want to override the
#'   plot defaults.
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}()}. Only needs to be set at the layer level if you
#'   are overriding the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points on this
#'   layer.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical indicating whether NA values should be stripped before
#'   the computation proceeds.
#' @param method function or character If character, "lm", "rlm", "lmrob",
#'   "lts", "gls", "ma", "sma", "segreg", "rq" or the name of a model fit
#'   function are accepted, possibly followed by the fit function's
#'   \code{method} argument separated by a colon (e.g. \code{"rlm:M"}). If a
#'   function is different to \code{lm()}, \code{rlm()}, \code{ltsReg()},
#'   \code{gls()}, \code{ma}, \code{sma}, it must have formal parameters named
#'   \code{formula}, \code{data}, and \code{weights}. See Details.
#' @param se Display confidence interval around smooth? (`TRUE` by default only
#'   for fits with \code{lm()} and \code{rlm()}, see `level` to control.)
#' @param fm.values logical Add metadata and parameter estimates extracted from
#'   the fitted model object; \code{FALSE} by default.
#' @param fullrange Should the fit span the full range of the plot, or just the
#'   range of the data group used in each fit?
#' @param level Level of confidence interval to use (0.95 by default).
#' @param n Number of points at which to predict with the fitted model.
#'
#' @aesthetics StatPolyLine
#'
#' @return The value returned by the statistic is a data frame, with \code{n}
#'   rows of predicted values and their confidence limits. Optionally it will
#'   also include additional values related to the model fit. When a
#'   \code{predict()} method is not available for the fitted model class, the
#'   value returned by calling \code{fitted()}, if available, is returned
#'   instead, with a message. In case of failure \code{data.frame()}, an empty
#'   data frame, is returned resulting in the plot layer addition being skipped
#'   silently, on a per group basis.
#'
#' @section Model formula and model fitting:
#'   A ggplot statistic receives as \code{data} a data frame that is not the one
#'   passed as argument by the user, but instead a data frame with the variables
#'   mapped to aesthetics. In \code{stat_poly_eq()} the compute function is
#'   applied by group, each call "seeing" the subset of \code{data} for an
#'   individual group. As supported models are for regression lines,
#'   variables mapped to \code{x} and \code{y} should both be continuous, i.e.,
#'   numeric or date time and model formulas defined using \code{x} and \code{y}
#'   as variable names.
#'
#'   The interpretation of the argument passed to \code{formula} is enhanced
#'   compared to \code{stat_smooth()}. Formulas with \code{x} as explanatory
#'   variable work as in \code{stat_smooth()} but formulas with \code{y} as
#'   explanatory variable are also accepted. \code{orientation} is set
#'   automatically based on which explanatory variable appears in the formula.
#'   Spline-based smoothers are only partially supported.
#'
#' @section Model fit methods supported: Several model fit functions are
#'   supported explicitly (see tables), and some of their differences smoothed
#'   out. Compatibility is checked late, based on the class of the returned
#'   fitted model object. This makes it possible to use wrapper functions that
#'   do model selection or other adjustments to the fit procedure on a per panel
#'   or per group basis. Moreover, if the value returned as model fit object is
#'   \code{NULL}, no layer is added to the plot on a per group within panel
#'   basis.
#'
#'   In the case of fitted model objects of classes not explicitly supported an
#'   attempt is made to find the usual accessors and/or fitted object members,
#'   and if found, either complete or partial support is frequently achieved.
#'   In this case a message is issued encouraging users to check the validity
#'   of the values extracted.
#'
#'   The argument to parameter \code{method} can be either the name of a
#'   function object, possibly using double colon notation in case its package
#'   is not attached, or a character string matching the function name for
#'   functions in the search path. This approach makes it possible to support
#'   model fit functions that are not dependencies of 'ggpmisc'. Either by
#'   attaching the package where the function is defined and passing it by name
#'   or as string, or using double colon notation when passing the name of the
#'   function.
#'
#'   User-defined functions can be passed as argument to parameter
#'   \code{method} as long as they have parameters \code{formula}, \code{data}
#'   \code{subset} and possibly \code{weights}. Additional arguments can be
#'   passed to any method as a named list as argument to parameter
#'   \code{method.args}. As in \code{\link[ggplot2:geom_smooth]{stat_smooth}()}
#'   prior \code{weights} are passed to the model fit functions' \code{weights}
#'   (plural!) parameter by mapping a numeric variable to plot aesthetic
#'   \code{weight} (singular!).
#'
#'   Tables 1 lists natively supported model fit functions, with the
#'   caveat that only some 'broom' methods' specializations have been actually
#'   tested with statistics from 'ggpmisc'. In addition, the statistics based
#'   on 'broom' methods require the user to tailor their behaviour by passing
#'   additional arguments in the call and occasionally some detective work to
#'   find out the names of variables in the returned data frame.
#'
#'   \strong{Table 1.} Model fit methods supported by the different statistics
#'   available in package 'ggpmisc'. Column \eqn{f} indicates whether
#'   computations are done by group (G) or by plot panel (P).
#'   \tabular{lcl}{
#'   \strong{Statistic} \tab \eqn{f} \tab \strong{Supported model fit methods} \cr
#'   \code{\link{stat_poly_line}()} \tab G \tab "lm", "rlm", "lts", "sma", "ma", "gls", \emph{others with methods} \code{\link[stats]{predict}()} or \code{\link[stats]{fitted}()} \cr
#'   \code{\link{stat_poly_eq}()}   \tab G \tab "lm", "rlm", "lts", "sma", "ma", "gls",  \emph{others with needed accesors} \cr
#'   \code{\link{stat_quant_line}()} \tab G \tab "rq", "rqss" \cr
#'   \code{\link{stat_quant_band}()} \tab G \tab "rq", "rqss" \cr
#'   \code{\link{stat_quant_eq}()} \tab G \tab "rq", "rqss" \cr
#'   \code{\link{stat_ma_line}()} \tab G \tab "SMA", "MA", "RMA", "OLS" \cr
#'   \code{\link{stat_ma_eq}()} \tab G \tab "SMA", "MA", "RMA", "OLS" \cr
#'   \code{\link{stat_fit_residuals}()} \tab G \tab "lm", "rlm", "lts", "sma", "ma", "gls", "rq", "rqss" \emph{others with method} \code{\link[stats]{residuals}()} \cr
#'   \code{\link{stat_fit_fitted}()} \tab G \tab "lm", "rlm", "lts", "gls", "rq", "rqss" \emph{others with method} \code{\link[stats]{fitted}()} \cr
#'   \code{\link{stat_fit_deviations}()} \tab G \tab "lm", "rlm", "lts", "gls", "rq", "rqss" \emph{others with methods} \code{\link[stats]{fitted}()} and \code{\link[stats]{weights}()} \cr
#'   \code{\link{stat_fit_augment}()} \tab G \tab \emph{any with 'broom' method} \code{\link[broom]{augment}()} \cr
#'   \code{\link{stat_fit_glance}()} \tab G \tab \emph{any with 'broom' method} \code{\link[broom]{glance}()} \cr
#'   \code{\link{stat_fit_tidy}()} \tab G \tab \emph{any with 'broom' method} \code{\link[broom]{tidy}()} \cr
#'   \code{\link{stat_fit_tb}()} \tab P \tab \emph{any with 'broom' method} \code{\link[broom]{tidy}()} \cr
#'   }
#'
#'   The single colon notation is based on parsing
#'   the name and is available whenever passing the name of the fit method as a
#'   character string. In a string such as "head:tail" the "head" gives the name
#'   of the model fit function and the "tail" gives the argument to pass it's
#'   \code{method} parameter. This is only a convenience, as \code{method.args}
#'   can be also used. In some methods, i.e., splines, the default
#'   \code{formula = y ~ x} needs to be overridden with an explicit argument.
#'
#'   Table 2 lists the correspondence of pre-defined \emph{method names}
#'   to model fit method functions. As mentioned above, these are only
#'   a subset of the model fit methods that are expected to work. When using
#'   these names there is no need for users to attach additional packages but
#'   the packages must be available (installed).
#'
#'   \strong{Table 2.} Available predefined method names, the model fit functions
#'   they call, the packages where the functions reside, the class of the
#'   returned fitted model object and the arguments that can be
#'   passed to their \code{method} parameter using single colon notation.
#'   \tabular{llll}{
#'   \strong{Predefined method names} \tab \strong{Model fit methods} \tab \strong{R package} \tab \strong{Object class} \cr
#'   "lm", "lm:qr" \tab \code{\link[stats]{lm}()} \tab 'stats' \tab "lm" \cr
#'   "rlm", "rlm:M", "rlm:MM" \tab \code{\link[MASS]{rlm}()} \tab 'MASS' \tab "rlm" ("lm") \cr
#'   "lts", "ltsReg" \tab \code{\link[robustbase]{ltsReg}()} \tab 'robustbase' \tab "lts" \cr
#'   "ma", "sma", "sma:SMA", "sma:MA", "sma:OLS" \tab \code{\link[smatr]{sma}()} \tab 'smatr' \tab "ma" or "sma" \cr
#'   "gls", "gls:REML", "gls:ML" \tab \code{\link[nlme]{gls}()} \tab 'nlme' \tab "gls" \cr
#'   "rq", "rq:sfn", "rq:sfnc", "rq:lasso" \tab \code{\link[quantreg]{rq}()} \tab 'quantreg' \tab "rq" \cr
#'   "rqss", "rqss:sfn", "rqss:sfnc", "rqss:lasso" \tab \code{\link[quantreg]{rqss}()} \tab 'quantreg' \tab "rqss" \cr
#'   "SMA", "MA", "RMA", "OLS" \tab \code{\link[lmodel2]{lmodel2}()} \tab 'lmodel2' \tab ("list") \cr
#'   }
#'
#' @section Computed variables: `stat_poly_line()` provides the following
#'   variables, some of which depend on the orientation:
#'
#'   \describe{ \item{y \strong{or} x}{predicted value}
#'   \item{ymin \strong{or} xmin}{lower confidence limit around the fitted line}
#'   \item{ymax \strong{or} xmax}{upper confidence limit around the fitted line}
#'   \item{se}{standard error} }
#'
#'   If \code{fm.values = TRUE} is passed then columns based on the summary of
#'   the model fit are added, with the same value in each row within a group.
#'   This is wasteful and disabled by default, but provides a simple and robust
#'   approach to achieve effects like colouring or hiding of the model fit line
#'   based on \eqn{P}, \eqn{R^2}, \eqn{R[adj]^2} or the number of
#'   observations in a fit.
#'
#' @note Currently confidence bands for the regression line are not plotted in
#'   some cases, and in the case of MA and SMA models, the band only displays
#'   the uncertainty of the slope rather than for both slope plus intercept.
#'
#' @seealso Consult the documentation of the model fit functions used
#'   for the details and additional arguments that can be passed to
#'   them by name through parameter \code{method.args}.
#'
#'   Please, see the articles in
#'   \href{https://docs.r4photobiology.info/ggpmisc/}{online-only documentation}
#'   for additional use examples and guidance.
#'
#' @family 'ggpmisc' statistics for model fits
#'
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_poly_line()
#'
#' # same as default
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_poly_line(formula = y ~ x)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_poly_line(formula = x ~ y)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_poly_line(formula = y ~ poly(x, 3))
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_poly_line(formula = x ~ poly(y, 3))
#'
#' # Models are automatically fit to each group (defined by categorical
#' # aesthetics or the group aesthetic) and for each facet.
#'
#' ggplot(mpg, aes(displ, hwy, colour = class)) +
#'   geom_point() +
#'   stat_poly_line(se = FALSE)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_poly_line() +
#'   facet_wrap(~drv)
#'
#' # Inspecting the returned data using geom_debug_group()
#' gginnards.installed <- requireNamespace("gginnards", quietly = TRUE)
#'
#' if (gginnards.installed)
#'   library(gginnards)
#'
#' if (gginnards.installed)
#'   ggplot(mpg, aes(displ, hwy)) +
#'     stat_poly_line(geom = "debug_group")
#'
#' if (gginnards.installed)
#'   ggplot(mpg, aes(displ, hwy)) +
#'     stat_poly_line(geom = "debug_group", fm.values = TRUE)
#'
#' if (gginnards.installed)
#'   ggplot(mpg, aes(displ, hwy)) +
#'     stat_poly_line(geom = "debug_group", method = lm, fm.values = TRUE)
#'
#' @export
#'
stat_poly_line <- function(mapping = NULL,
                           data = NULL,
                           geom = "smooth",
                           position = "identity",
                           ...,
                           orientation = NA,
                           method = "lm",
                           formula = NULL,
                           se = NULL,
                           fit.seed = NA,
                           fm.values = FALSE,
                           n = 80,
                           fullrange = FALSE,
                           level = 0.95,
                           method.args = list(),
                           n.min = 2L,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {

  stopifnot("Args 'formula' and/or 'data' in 'method.args'" =
              !any(c("formula", "data") %in% names(method.args)))

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

  if (method.name == "auto") {
    message("Method 'auto' is equivalent to 'lm', splines are not supported.")
    method <- method.name <- "lm"
  } else if (grepl("^rq$|^rq[:]|^rqss$|^rqss[:]", method.name)) {
    stop("Methods 'rq' and 'rqss' not supported, please use 'stat_quant_line()'.")
  } else if (grepl("^lmodel2$|^lmodel2[:]", method.name)) {
    stop("Method 'lmodel2' not supported, please use 'stat_ma_line()'.")
  }

  if (is.null(se)) {
    se <- ifelse(grepl("gls|lqs", method.name), FALSE, TRUE)
  }

  temp <- guess_orientation(orientation = orientation,
                            formula = formula,
                            default.formula = y ~ x,
                            formula.on.x = TRUE)
  orientation <- temp[["orientation"]]
  formula <-  temp[["formula"]]

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatPolyLine,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      method = method,
      method.name = method.name,
      formula = formula,
      se = se,
      fit.seed = fit.seed,
      fm.values = fm.values,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      orientation = orientation,
      method.args = method.args,
      n.min = n.min,
      ...
    )
  )
}

poly_line_compute_group_fun <-
  function(data,
           scales,
           method,
           method.name,
           formula = NULL,
           se,
           fit.seed = NA,
           fm.values = FALSE,
           n = 80,
           fullrange = FALSE,
           xseq = NULL,
           level = 0.95,
           method.args = list(),
           n.min = 2L,
           na.rm = FALSE,
           flipped_aes = NA,
           orientation = "x") {

    data <- ggplot2::flip_data(data, flipped_aes)

    temp.ls <- fit_models_internal(data = data,
                                   method = method,
                                   method.name = method.name,
                                   method.args = method.args,
                                   n.min = n.min,
                                   formula = formula,
                                   fit.seed = fit.seed,
                                   orientation = "x") # data already flipped
    if (!length(temp.ls) || !length(temp.ls[["fm"]])) {
      # An empty data.frame results in no plot layer when passed to geoms
      return(data.frame())
    }
    fm <- temp.ls[["fm"]]
    method.name <- temp.ls[["method.name"]] # argument or default which varies
    method.args <- temp.ls[["method.args"]] # argument or default which varies

    if (is.null(xseq)) {
      if (fullrange) {
        xrange <- scales[[orientation]]$dimension()
      } else {
        xrange <- range(data$x, na.rm = TRUE)
      }
      xseq <- seq(from = xrange[1], to = xrange[2], length.out = n)
    }

    has.predict.method <- FALSE
    for (cl in class(fm)) {
      if (cl == "sma") break() # has dummy predict() method
      if (any(grepl("^predict", utils::methods(class = cl)))) {
        has.predict.method <- TRUE
        break()
      }
    }
    if (has.predict.method) {
      # We try hard to extract predicted values
      newdata <- data.frame(x = xseq)
      try(
        prediction <- stats::predict(fm,
                                     newdata = newdata,
                                     se.fit = se,
                                     level = level,
                                     interval = if (se) "confidence" else "none"
        )
      )
      if (inherits(prediction, "try-error")) {
        if (se) {
          message("Confidence band not supported: overridding 'se = TRUE'")
          se <- FALSE
        }
        try(
          prediction <- stats::predict(fm, newdata = newdata)
        )
        if (inherits(prediction, "try-error")) {
          warning("Prediction failed!")
          prediction <- rep_len(NA_real_, nrow(data))
        }
      }
      if (se) {
        if (exists("fit", prediction)) {
          prediction <- as.data.frame(prediction[["fit"]])
        }
        names(prediction) <- c("y", "ymin", "ymax")
      } else {
        prediction <- data.frame(y = as.vector(prediction))
      }
      prediction <- cbind(newdata, prediction)
    } else { # does not have predict method
      if (class(fm)[1] == "sma") {
        if (se) {
          message("SMA/MA, band is currently for slope only!")
          # fm$coef[[1]] is a data.frame
          #
          #           coef(SMA) lower limit upper limit
          # elevation 39.441685   38.011038    40.87233
          # slope     -4.609003   -5.008148    -4.24167
          ### parameter estimates assumed independent!!
          ## bootstraping would be more appropriate
          coef.sma <- fm$coef[[1]]
          b0 <- unlist(coef.sma["elevation", ])
          b0 <- ifelse(is.na(b0), 0, b0)
          b1 <- unlist(coef.sma["slope", ])
          # centering is needed
          b0delta <- b0 - b0[1]
          if (all(b0 == 0)) {
            # center on zero
            center.y <- 0
            center.x <- 0
          } else {
            # data centroid
            center.y <- mean(data[["y"]])
            center.x <- mean(data[["x"]])
          }
          rightside <- xseq > center.x
          leftside <- !rightside
          prediction <- data.frame(x = xseq,
                                   y = center.y + b0delta[1] +
                                     b1[1] * (xseq - center.x),
                                   ymin = center.y +
                                     (rightside * b1[2] + leftside * b1[3]) *
                                     (xseq - center.x),
                                   ymax = center.y +
                                     (rightside * b1[3] + leftside * b1[2]) *
                                     (xseq - center.x))
        } else { # se is FALSE
          coefs <- stats::coefficients(fm)
          # named vector
          #
          # elevation     slope
          # 39.441685 -4.609003
          prediction <-
            data.frame(x = xseq, # data[["x"]]
                       y = coefs["elevation"] + coefs["slope"] * xseq)
        }
      } else {
        message("Fitted line plotted")
        prediction <-
          data.frame(x = data[["x"]], y = fitted(fm))
      }
    }

    if (fm.values) {
      fm.summary <- summary(fm)
      # summary methods for different fit methods may no return some slots
      if (exists("fstatistic", fm.summary)) {
        prediction[["p.value"]] <-
          stats::pf(q = fm.summary[["fstatistic"]][["value"]],
                    df1 = fm.summary[["fstatistic"]][["numdf"]],
                    df2 = fm.summary[["fstatistic"]][["dendf"]],
                    lower.tail = FALSE)
      } else {
        prediction[["p.value"]] <- NA_real_
      }
      if (exists("r.squared", fm.summary)) {
        prediction[["r.squared"]] <- fm.summary[["r.squared"]]
      } else {
        prediction[["r.squared"]] <- NA_real_
      }
      if (exists("adj.r.squared", fm.summary)) {
        prediction[["adj.r.squared"]] <- fm.summary[["adj.r.squared"]]
      } else {
        prediction[["adj.r.squared"]] <- NA_real_
      }
      if (exists("residuals", fm)) {
        prediction[["n"]] <- length(fm[["residuals"]])
      } else {
        prediction[["n"]] <- NA_real_
      }
      prediction[["fm.class"]] <- class(fm)[1]
      prediction[["fm.method"]] <- method.name
      prediction[["fm.formula"]] <- fail_safe_formula(fm, method.args)
      prediction[["fm.formula.chr"]] <- format(prediction[["fm.formula"]])
    }

     prediction$flipped_aes <- flipped_aes
     ggplot2::flip_data(prediction, flipped_aes)
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPolyLine <-
  ggplot2::ggproto("StatPolyLine", Stat,
                   setup_params = function(data, params) {
                     params[["flipped_aes"]] <-
                       ggplot2::has_flipped_aes(data, params, ambiguous = TRUE)
                     params
                   },

                   extra_params = c("na.rm", "orientation"),

                   compute_group = poly_line_compute_group_fun,

                   dropped_aes = c("weight"),
                   required_aes = c("x", "y")
  )
