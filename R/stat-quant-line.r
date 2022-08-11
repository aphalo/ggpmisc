#' Predicted line from quantile regression fit
#'
#' Predicted values are computed and, by default, plotted. Depending on the
#' fit method, a confidence band can be computed and plotted. The confidence
#' band can be interpreted similarly as that produced by \code{stat_smooth()}
#' and \code{stat_poly_line()}.
#'
#' @details \code{stat_quant_line()} behaves similarly to
#'   \code{ggplot2::stat_smooth()} and \code{stat_poly_line()} but supports
#'   fitting regressions for multiple quantiles in the same plot layer. This
#'   statistic interprets the argument passed to \code{formula} accepting
#'   \code{y} as well as \code{x} as explanatory variable, matching
#'   \code{stat_quant_eq()}. While \code{stat_quant_eq()} supports only method
#'   \code{"rq"}, \code{stat_quant_line()} and \code{stat_quant_band()} support
#'   both \code{"rq"} and \code{"rqss"}, In the case of \code{"rqss"} the model
#'   formula makes normally use of \code{qss()} to formulate the spline and its
#'   constraints.
#'
#'   \code{\link[ggplot2]{geom_smooth}}, which is used by default, treats each
#'   axis differently and thus is dependent on orientation. If no argument is
#'   passed to \code{formula}, it defaults to \code{y ~ x}. Formulas with
#'   \code{y} as explanatory variable are treated as if \code{x} was the
#'   explanatory variable and \code{orientation = "y"}.
#'
#'   Package 'ggpmisc' does not define a new geometry matching this statistic as
#'   it is enough for the statistic to return suitable \code{x}, \code{y},
#'   \code{ymin}, \code{ymax} and \code{group} values.
#'
#'   There are multiple uses for double regression on x and y. For example, when
#'   two variables are subject to mutual constrains, it is useful to consider
#'   both of them as explanatory and interpret the relationship based on them.
#'   So, from version 0.4.1 'ggpmisc' makes it possible to easily implement the
#'   approach described by Cardoso (2019) under the name of "Double quantile
#'   regression".
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset, only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points on this
#'   layer
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
#' @param formula a formula object. Using aesthetic names \code{x} and \code{y}
#'   instead of original variable names.
#' @param quantiles numeric vector Values in 0..1 indicating the quantiles.
#' @param method function or character If character, "rq", "rqss" or the name of
#'   a model fit function are accepted, possibly followed by the fit function's
#'   \code{method} argument separated by a colon (e.g. \code{"rq:br"}). If a
#'   function different to \code{rq()}, it must accept arguments named
#'   \code{formula}, \code{data}, \code{weights}, \code{tau} and \code{method}
#'   and return a model fit object of class \code{rq}, \code{rqs} or
#'   \code{rqss}.
#' @param method.args named list with additional arguments passed to
#'   \code{rq()}, \code{rqss()} or to a function passed as argument to
#'   \code{method}.
#' @param n Number of points at which to evaluate smoother.
#' @param orientation character Either "x" or "y" controlling the default for
#'   \code{formula}.
#' @param se logical Passed to \code{quantreg::predict.rq()}.
#' @param fm.values logical Add n as a column to returned data? (`FALSE` by
#'   default.)
#' @param level numeric in range [0..1] Passed to \code{quantreg::predict.rq()}.
#' @param type character Passed to \code{quantreg::predict.rq()}.
#' @param interval character Passed to \code{quantreg::predict.rq()}.
#'
#' @return The value returned by the statistic is a data frame, that will have
#'   \code{n} rows of predicted values and and their confidence limits for each
#'   quantile, with each quantile in a group. The variables are \code{x} and
#'   \code{y} with \code{y} containing predicted values. In addition,
#'   \code{quantile} and \code{quantile.f} indicate the quantile used and
#'   and edited \code{group} preserves the original grouping adding a new
#'   "level" for each quantile. Is \code{se = TRUE}, a confidence band is
#'   computed and values for it returned in \code{ymax} and \code{ymin}.
#'
#' @return The value returned by the statistic is a data frame, that will have
#'   \code{n} rows of predicted values and their confidence limits. Optionally
#'   it will also include additional values related to the model fit.
#'
#' @section Computed variables: `stat_quant_line()` provides the following
#'   variables, some of which depend on the orientation: \describe{ \item{y *or*
#'   x}{predicted value} \item{ymin *or* xmin}{lower confidence
#'   interval around the mean} \item{ymax *or* xmax}{upper confidence
#'   interval around the mean}}
#'
#'   If \code{fm.values = TRUE} is passed then one column with the number of
#'   observations \code{n} used for each fit is also included, with the same
#'   value in each row within a group. This is wasteful and disabled by default,
#'   but provides a simple and robust approach to achieve effects like colouring
#'   or hiding of the model fit line based on the number of observations.
#'
#' @section Aesthetics: \code{stat_quant_line} understands \code{x} and \code{y},
#'   to be referenced in the \code{formula} and \code{weight} passed as argument
#'   to parameter \code{weights}. All three must be mapped to \code{numeric}
#'   variables. In addition, the aesthetics understood by the geom
#'   (\code{"geom_smooth"} is the default) are understood and grouping
#'   respected.
#'
#' @references
#' Cardoso, G. C. (2019) Double quantile regression accurately assesses
#'   distance to boundary trade-off. Methods in ecology and evolution,
#'   10(8), 1322-1331.
#'
#' @seealso \code{\link[quantreg]{rq}}, \code{\link[quantreg]{rqss}} and
#'   \code{\link[quantreg]{qss}}.
#'
#' @family ggplot statistics for quantile regression
#'
#' @export
#'
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_line()
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_line(se = TRUE)
#'
#' # If you need the fitting to be done along the y-axis set the orientation
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_line(orientation = "y")
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_line(orientation = "y", se = TRUE)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_line(formula = y ~ x)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_line(formula = x ~ y)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_line(formula = y ~ poly(x, 3))
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_line(formula = x ~ poly(y, 3))
#'
#' # Instead of rq() we can use rqss() to fit an additive model:
#' library(quantreg)
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_line(method = "rqss",
#'                   formula = y ~ qss(x, constraint = "D"),
#'                   quantiles = 0.5)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_line(method = "rqss",
#'                   formula = x ~ qss(y, constraint = "D"),
#'                   quantiles = 0.5)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()+
#'   stat_quant_line(method="rqss",
#'                   interval="confidence",
#'                   se = TRUE,
#'                   mapping = aes(fill = factor(after_stat(quantile)),
#'                                 color = factor(after_stat(quantile))),
#'                   quantiles=c(0.05,0.5,0.95))
#'
#' # Smooths are automatically fit to each group (defined by categorical
#' # aesthetics or the group aesthetic) and for each facet.
#'
#' ggplot(mpg, aes(displ, hwy, colour = drv, fill = drv)) +
#'   geom_point() +
#'   stat_quant_line(method = "rqss",
#'                   formula = y ~ qss(x, constraint = "V"),
#'                    quantiles = 0.5)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_line(formula = y ~ poly(x, 2)) +
#'   facet_wrap(~drv)
#'
#' # Inspecting the returned data using geom_debug()
#' if (requireNamespace("gginnards", quietly = TRUE)) {
#'   library(gginnards)
#'
#'   ggplot(mpg, aes(displ, hwy)) +
#'     stat_quant_line(geom = "debug")
#'
#'   ggplot(mpg, aes(displ, hwy)) +
#'     stat_quant_line(geom = "debug", fm.values = TRUE)
#'
##' }
#'
#' @export
#'
stat_quant_line <- function(mapping = NULL,
                            data = NULL,
                            geom = "smooth",
                            position = "identity",
                            ...,
                            quantiles = c(0.25, 0.5, 0.75),
                            formula = NULL,
                            se = length(quantiles) == 1L,
                            fm.values = FALSE,
                            n = 80,
                            method = "rq",
                            method.args = list(),
                            level = 0.95,
                            type = "direct",
                            interval = "confidence",
                            na.rm = FALSE,
                            orientation = NA,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  if (is.null(formula)) {
    if (is.character(method)) {
      if (method == "rq") {
        formula <- y ~ x
      } else if (method == "rqss") {
        qss <- quantreg::qss
        formula <- y ~ qss(x)
        # emit message only if formula is not y ~ x
        message("Smoothing formula not specified. Using: ",
                deparse(formula))
      }
    }
    if (is.na(orientation)) {
      orientation = "x"
    }
  } else {
    formula.chr <- as.character(formula)
    if (is.na(orientation)) {
      # we guess orientation from formula
      if (formula.chr[2] == "y") {
        orientation <- "x"
      } else if (formula.chr[2] == "x") {
        orientation <- "y"
        formula <- swap_xy(formula)
      }
    } else if (formula.chr[2] != "y"){
      stop("When both 'orientation' and 'formula' are passed arguments ",
           "the formula should have 'x' as explanatory variable.")
    }
  }
  if (is.character(method)) {
    if (grepl("^lm|^rlm", method)) {
      stop("Methods 'lm' and 'rlm' not supported, please use 'stat_poly_eq()'.")
    } else if (grepl("^lmodel2", method)) {
      stop("Method 'lmodel2' not supported, please use 'stat_ma_eq()'.")
    }
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatQuantLine,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      quantiles = quantiles,
      formula = formula,
      se = se,
      fm.values = fm.values,
      n = n,
      method = method,
      method.args = method.args,
      na.rm = na.rm,
      orientation = orientation,
      level = level,
      type = type,
      interval = interval,
      ...
    )
  )
}

# Defined here to avoid a note in check --as-cran as the import from 'polynom'
# is not seen when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
quant_line_compute_group_fun <- function(data,
                                         scales,
                                         quantiles = c(0.25, 0.5, 0.75),
                                         formula = NULL,
                                         n = 80,
                                         method = "rq",
                                         method.args = list(),
                                         lambda = 1,
                                         level = 0.95,
                                         type = "none",
                                         interval = "none",
                                         se = TRUE,
                                         fm.values = FALSE,
                                         na.rm = FALSE,
                                         flipped_aes = NA) {
  rlang::check_installed("quantreg", reason = "for `stat_quant_line()`")

  data <- ggplot2::flip_data(data, flipped_aes)

  if (is.null(data[["weight"]])) {
    data[["weight"]] <- 1
  }

  min.indep <- min(data[["x"]], na.rm = TRUE)
  max.indep <- max(data[["x"]], na.rm = TRUE)
  seq.indep <- seq(min.indep, max.indep, length.out = n)

  grid <- data.frame(x = seq.indep)

  # If method was specified as a character string, replace with
  # the corresponding function. Some model fit functions themselves have a
  # method parameter accepting character strings as argument. We support
  # these by splitting strings passed as argument at a colon.
  if (is.character(method)) {
    if (method %in% c("br", "fn", "pfn", "sfn", "fnc", "conquer",
                      "pfnb", "qfnb", "ppro", "lasso")) {
      method <- paste("rq", method, sep = ":")
      message("Using method: ", method)
    }
    method.name <- method
    method <- strsplit(x = method, split = ":", fixed = TRUE)[[1]]
    if (length(method) > 1L) {
      fun.method <- method[2]
      method <- method[1]
    } else {
      fun.method <- character()
    }
    method <- switch(method,
                     rq = quantreg::rq,
                     rqss = quantreg::rqss,
                     match.fun(method))
  } else if (is.function(method)) {
    fun.method <- method.args[["method"]]
    if (is.name(quote(method))) {
      method.name <- as.character(quote(method))
    } else {
      method.name <- "function"
    }
    if (length(fun.method)) {
      method.name <- paste(method.name, fun.method, sep = ":")
    }
  }

  if (length(fun.method)) {
    method.args[["method"]] <- fun.method
  }

  z <- dplyr::bind_rows(
    lapply(quantiles, quant_pred, data = data, method = method,
           formula = formula, weight = data[["weight"]], grid = grid,
           method.args = method.args, orientation = "x",
           level = level, type = type, interval = interval)
  )
  if (is.matrix(z[["y"]])) {
    z[["ymin"]] <- z[["y"]][ , 2L]
    z[["ymax"]] <- z[["y"]][ , 3L]
    z[["y"]] <- z[["y"]][ , 1L]
  } else {
    z[["ymin"]] <- z[["ymax"]] <- NA_real_
  }

  if (fm.values) {
      z[["n"]] <- nrow(na.omit(data[, c("x", "y")]))
      z[["method"]] <- method.name
  }

  # a factor with nicely formatted labels for levels is helpful
  quant.digits <- ifelse(min(z[["quantile"]]) < 0.01 || max(z[["quantile"]]) > 0.99,
                         3, 2)
  quant.levels <- sort(unique(z[["quantile"]]), decreasing = TRUE)
  quant.labels <- sprintf("%#.*f", quant.digits, quant.levels)
  z[["quantile.f"]] <-
    factor(z[["quantile"]], levels = quant.levels, labels = quant.labels)

  z[["flipped_aes"]] <- flipped_aes
  ggplot2::flip_data(z, flipped_aes)
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatQuantLine <-
  ggplot2::ggproto("StatQuantLine", ggplot2::Stat,
                   setup_params = function(data, params) {
                     params[["flipped_aes"]] <-
                       ggplot2::has_flipped_aes(data, params, ambiguous = TRUE)
                     params
                   },
                   extra_params = c("na.rm", "orientation"),
                   compute_group = quant_line_compute_group_fun,
                   default_aes = ggplot2::aes(group = after_stat(group),
                                              weight = 1),
                   required_aes = c("x", "y")
  )

# modified from 'ggplot2'
quant_pred <- function(quantile, data, method, formula, weight, grid,
                       method.args = method.args, orientation = "x",
                       level = 0.95, type = "none", interval = "none",
                       make.groups = TRUE) {
  args <- c(list(quote(formula), data = quote(data), tau = quote(quantile),
    weights = quote(weight)), method.args)
  # quantreg contains code with partial matching of names!
  # so we silence selectively only these warnings
  withCallingHandlers({
    model <- do.call(method, args)
  }, warning = function(w) {
    if (startsWith(conditionMessage(w), "partial match of") ||
        startsWith(conditionMessage(w), "partial argument match of")) {
      invokeRestart("muffleWarning")
    }
  })

  if (orientation == "x") {
    grid[["y"]] <- stats::predict(model, newdata = grid, level = level,
                                  type = type, interval = interval)
  } else {
    grid[["x"]] <- stats::predict(model, newdata = grid, level = level,
                                  type = type, interval = interval)
  }
  grid[["quantile"]] <- quantile
  if (make.groups) {
    grid[["group"]] <- paste(data[["group"]][1], quantile, sep = "-")
  } else {
    grid[["group"]] <- data[["group"]][1]
  }

  grid
}
