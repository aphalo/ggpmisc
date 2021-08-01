#' Predicted line from model fit
#'
#' Predicted values and a confidence band are computed and, by default, plotted.
#' \code{stat_poly_line()} behaves like \code{\link[ggplot2]{stat_smooth}}
#' except for supporting the use of \code{y} as explanatory variable in the
#' model formula, fits the model with \code{stats::lm()} as default for
#' \code{method}, irrespective of the number of observations. The fit can
#' alternatively by done by any of the methods supported by
#' \code{\link[ggplot2]{stat_smooth}}, including \code{method = "auto"}.
#'
#' @details
#' This statistic is just \code{\link[ggplot2]{stat_smooth}} with different
#' defaults and updated so that it interprets the argument passed to
#' \code{formula} differently, accepting \code{y} as explanatory variable and
#' setting \code{orientation} automatically. In addition the default for
#' \code{method} is \code{"lm"}, matching the default used in
#' \code{stat_poly_eq()} and \code{stat_poly_quant()}. It calls
#' \code{\link[ggplot2]{StatSmooth}} to build a layer.
#'
#' \code{\link[ggplot2]{geom_smooth}}, which is used by default, treats each
#' axis differently and can thus have two orientations. The orientation is easy
#' to deduce from the argument passed to \code{formula}. Thus,
#' \code{stat_smooth_xy()} will by default guess which orientation the layer
#' should have. If no argument is passed to \code{formula}, the orientation is
#' ambiguous. In that case the orientation can be specified directly passing an
#' argument to the \code{orientation} parameter, which can be either \code{"x"}
#' or \code{"y"}. The value gives the axis that is taken as the explanatory
#' variable, \code{"x"} being the default orientation you would expect for the
#' geom. Package 'ggpmisc' does not define new geometries matching the new
#' statistics as they are not needed and conceptually transformations of
#' \code{data} are expressed as statistics.
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
#' @param method function or character If character, "lm", "rlm" and
#'   "rq" are accepted. If a function, it must have formal parameters
#'   \code{formula} and \code{data} and return a model fit object for which
#'   \code{summary()} and \code{coefficients()} are consistent with those for
#'   \code{lm} fits.
#' @param method.args named list with additional arguments.
#' @param se Display confidence interval around smooth? (`TRUE` by default, see
#'   `level` to control.)
#' @param fullrange Should the fit span the full range of the plot, or just
#'   the data?
#' @param level Level of confidence interval to use (0.95 by default).
#' @param span Controls the amount of smoothing for the default loess smoother.
#'   Smaller numbers produce wigglier lines, larger numbers produce smoother
#'   lines. Only used with loess, i.e. when `method = "loess"`,
#'   or when `method = NULL` (the default) and there are fewer than 1,000
#'   observations.
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
#'   (\code{"geom_smooth"} is the default) are understood and grouping
#'   respected.
#'
#' @family ggplot statistics for linear and polynomial regression
#'
#' @export
#'
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_poly_line()
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
#' # The default behavior of geom_smooth()
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_poly_line(method = "auto")
#'
#' # Use span to control the "wiggliness" of the default loess smoother.
#' # The span is the fraction of points used to fit each local regression:
#' # small numbers make a wigglier curve, larger numbers make a smoother curve.
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_poly_line(method = "loess", span = 0.3)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_poly_line(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_poly_line(method = lm, formula = x ~ splines::bs(y, 3), se = FALSE)
#'
#' # Smooths are automatically fit to each group (defined by categorical
#' # aesthetics or the group aesthetic) and for each facet.
#'
#' ggplot(mpg, aes(displ, hwy, colour = class)) +
#'   geom_point() +
#'   stat_poly_line(se = FALSE)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_poly_line(method = "auto", span = 0.8) +
#'   facet_wrap(~drv)
#'
#' @export
#'
stat_poly_line <- function(mapping = NULL, data = NULL,
                           geom = "smooth", position = "identity",
                           ...,
                           method = "lm",
                           formula = NULL,
                           se = TRUE,
                           n = 80,
                           span = 0.75,
                           fullrange = FALSE,
                           level = 0.95,
                           method.args = list(),
                           na.rm = FALSE,
                           orientation = NA,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  if (is.null(formula)) {
    formula = y ~ x
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
    if (method == "rlm") {
      method <- MASS::rlm
    } else if (method == "rq") {
      warning("Method 'rq' not supported, please use 'stat_quant_line()'.")
      method <- "auto"
    }
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = ggplot2::StatSmooth,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      orientation = orientation,
      method.args = method.args,
      span = span,
      ...
    )
  )
}

#' Swap x and y in a formula
#'
#' By default a formula of x on y is converted into a formula of y
#' on x, while the reverse swap is done only if \code{backward = TRUE}.
#'
#' @param f formula An R model formula
#' @param backwards logical
#'
#' @details
#' This function is meant to be used only as a helper within 'ggplot2'
#' statistics. Normally together with geometries supporting orientation when
#' we want to automate the change in orientation based on a user-supplied
#' formula. Only \code{x} and \code{y} are changed, and in other respects
#' the formula is rebuilt copying the environment from \code{f}.
#'
#' @return A copy of \code{f} with \code{x} and \code{y} swapped by each other
#'   in the lhs and rhs.
#'
swap_xy <- function(f, backwards = FALSE) {
  f.chr <- as.character(f)
  if (backwards) {
    # lhs
    f.chr[2] <- gsub("\\by\\b", "x", f.chr[2])
    # rhs
    f.chr[-c(1, 2)] <- gsub("\\bx\\b", "y", f.chr[-c(1, 2)])
  } else {
    # lhs
    f.chr[2] <- gsub("\\bx\\b", "y", f.chr[2])
    # rhs
    f.chr[-c(1, 2)] <- gsub("\\by\\b", "x", f.chr[-c(1, 2)])
  }
  # reassemble
  f.chr <- paste(f.chr[2], f.chr[3], sep = f.chr[1])
  # define new formula in the same environment as original
  stats::as.formula(f.chr, env = environment(f))
}
