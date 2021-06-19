#' Compute predicted line from model fit
#'
#' Predicted values are computed and, by default, plotted.
#' \code{stat_quantile_xy()} behaves like \code{\link[ggplot2]{stat_quantile}}
#' except for supporting the use of \code{y} as explanatory variable in the
#' model formula.
#'
#' This statistic is based on \code{\link[ggplot2]{stat_quantile}} updated so
#' that it interprets the argument passed to \code{formula} differently
#' accepting \code{y} as well as \code{x} as explanatory variable, matching
#' \code{stat_poly_quant()}.
#'
#' When two variables are subject to mutual constrains, it is useful to consider
#' both of them as explanatory and interpret the relationship based on them. So,
#' from version 0.4.1 'ggpmisc' makes it possible to easily implement the
#' approach described by Cardoso (2019) under the name of "Double quantile
#' regression".
#'
#' @details
#' \code{\link[ggplot2]{geom_quantile}}, which is used by default, treats each
#' axis equally and thus independent of orientation. If no argument is passed
#' to \code{formula}, it defaults to `y ~ x`.
#' Package 'ggpmisc' does not define a new geometry matching this statistic
#' as it is enough for the statistic to return suitable `x` and `y` values.
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
#' @param method function or character If character, "lm", "rlm" and
#'   "rq" are accepted. If a function, it must have formal parameters
#'   \code{formula} and \code{data} and return a model fit object for which
#'   \code{summary()} and \code{coefficients()} are consistent with those for
#'   \code{lm} fits.
#' @param method.args named list with additional arguments.
#' @param n Number of points at which to evaluate smoother.
#' @param orientation character Either "x" or "y" controlling the default for
#'   \code{formula}.
#'
#' @return The value returned by the statistic is a data frame, that will have
#'   \code{n} rows of predicted values and and their confidence limits.
#'
#' @section Computed variables: `stat_smooth_xy()` provides the following
#'   variables, some of which depend on the orientation: \describe{ \item{y *or*
#'   x}{predicted value} \item{ymin *or* xmin}{lower pointwise confidence
#'   interval around the mean} \item{ymax *or* xmax}{upper pointwise confidence
#'   interval around the mean} \item{se}{standard error} }
#'
#' @section Aesthetics: \code{stat_poly_eq} understands \code{x} and \code{y},
#'   to be referenced in the \code{formula} and \code{weight} passed as argument
#'   to parameter \code{weights}. All three must be mapped to \code{numeric}
#'   variables. In addition, the aesthetics understood by the geom
#'   (\code{"geom_quantile"} is the default) are understood and grouping
#'   respected.
#'
#' @references
#' Cardoso, G. C. (2019) Double quantile regression accurately assesses
#'   distance to boundary trade‐off. Methods in ecology and evolution, 2019-08,
#'   10 (8), pp. 1322-1331.
#'
#' @export
#'
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quantile_xy()
#'
#' # If you need the fitting to be done along the y-axis set the orientation
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quantile_xy(orientation = "y")
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quantile_xy(formula = y ~ x)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quantile_xy(formula = x ~ y)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quantile_xy(formula = y ~ poly(x, 3))
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quantile_xy(formula = x ~ poly(y, 3))
#'
#' # Instead of a loess smooth, you can use any other modelling function:
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quantile_xy(method = "rqss", se = FALSE)
#'
#' # Smooths are automatically fit to each group (defined by categorical
#' # aesthetics or the group aesthetic) and for each facet.
#'
#' ggplot(mpg, aes(displ, hwy, colour = class)) +
#'   geom_point() +
#'   stat_quantile_xy(formula = y ~ x)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quantile_xy(formula = y ~ x) +
#'   facet_wrap(~drv)
#'
#' @export
#'
stat_quantile_xy <- function(mapping = NULL,
                             data = NULL,
                             geom = "quantile",
                             position = "identity",
                             ...,
                             quantiles = c(0.25, 0.5, 0.75),
                             formula = NULL,
                             n = 80,
                             method = "rq",
                             method.args = list(),
                             na.rm = FALSE,
                             orientation = NA,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatQuantileXY,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      quantiles = quantiles,
      formula = formula,
      n = n,
      method = method,
      method.args = method.args,
      na.rm = na.rm,
      orientation = orientation,
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
                                         na.rm = FALSE,
                                         orientation = NA) {
  rlang::check_installed("quantreg", reason = "for `stat_quantile()`")

  force(data)
  num.quantiles <- length(quantiles)
  # we guess formula from orientation
  if (is.null(formula)) {
    if (is.na(orientation) || orientation == "x") {
      if (method == "rqss") {
        formula <- eval(
          substitute(y ~ qss(x, lambda = lambda)),
          list(lambda = lambda)
        )
        # make qss function available in case it is needed;
        # works around limitation in quantreg
        qss <- quantreg::qss
      } else {
        formula <- y ~ x
      }
    } else if (orientation == "y") {
      if (method == "rqss") {
        formula <- eval(
          substitute(x ~ qss(y, lambda = lambda)),
          list(lambda = lambda)
        )
        # make qss function available in case it is needed;
        # works around limitation in quantreg
        qss <- quantreg::qss
      } else {
        formula <- x ~ y
      }
    }
    message("Smoothing formula not specified. Using: ",
            deparse(formula))
  }
  # we guess orientation from formula
  if (is.na(orientation)) {
    orientation <- unname(c(x = "y", y = "x")[as.character(formula)[2]])
  }

  if (is.null(data$weight)) data$weight <- 1

  min.indep <- min(data[[orientation]], na.rm = TRUE)
  max.indep <- max(data[[orientation]], na.rm = TRUE)
  seq.indep <- seq(min.indep, max.indep, length.out = n)

  grid <- data.frame(seq.indep)
  names(grid) <- orientation

  # if method was specified as a character string, replace with
  # the corresponding function
  if (is.character(method)) {
    if (identical(method, "rq")) {
      method <- quantreg::rq
    } else if (identical(method, "rqss")) {
      method <- quantreg::rqss
    } else {
      method <- match.fun(method) # allow users to supply their own methods
    }
  } else {
    stopifnot(is.function(method))
  }

  dplyr::bind_rows(
    lapply(quantiles, quant_pred, data = data, method = method,
           formula = formula, weight = data$weight, grid = grid,
           method.args = method.args, orientation = orientation)
    )
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatQuantileXY <- ggplot2::ggproto("StatQuantileXY", ggplot2::Stat,
  required_aes = c("x", "y"),

  compute_group = quant_line_compute_group_fun
)

quant_pred <- function(quantile, data, method, formula, weight, grid,
                       method.args = method.args, orientation = "x") {
  args <- c(list(quote(formula), data = quote(data), tau = quote(quantile),
    weights = quote(weight)), method.args)
  # quantreg contains code with partial matching of names!
  # so we silence selectively only these warnings
  withCallingHandlers({
    model <- do.call(method, args)
  }, warning = function(w) {
    if (startsWith(conditionMessage(w), "partial match of 'coef'"))
      invokeRestart("muffleWarning")
  })

  if (orientation == "x") {
    grid$y <- stats::predict(model, newdata = grid)
  } else {
    grid$x <- stats::predict(model, newdata = grid)
  }
  grid$quantile <- quantile
  grid$group <- paste(data$group[1], quantile, sep = "-")

  grid
}
