#' Predicted line from model fit
#'
#' Predicted values and a confidence band are computed and, by default, plotted.
#'
#' @details
#' This statistic is similar to \code{\link[ggplot2]{stat_smooth}} but has
#' different defaults and it interprets the argument passed to \code{formula}
#' differently, accepting \code{y} as explanatory variable and setting
#' \code{orientation} automatically. The default for \code{method} is
#' \code{"lm"} and spline-based smoothers like \code{loess} are not supported.
#' Other defaults are consistent with those in \code{stat_poly_eq()},
#' \code{stat_quant_line()}, \code{stat_quant_eq()}, \code{stat_ma_line()},
#' \code{stat_ma_eq()}.
#'
#' \code{geom_poly_line()} treats the x and y aesthetics differently and can
#' thus have two orientations. The orientation can be deduced from the argument
#' passed to \code{formula}. Thus, \code{stat_poly_line()} will by default guess
#' which orientation the layer should have. If no argument is passed to
#' \code{formula}, the formula defaults to \code{y ~ x}. For consistency with
#' \code{\link[ggplot2]{stat_smooth}} orientation can be also specified directly
#' passing an argument to the \code{orientation} parameter, which can be either
#' \code{"x"} or \code{"y"}. The value of \code{orientation} gives the axis that
#' is taken as the explanatory variable. Package 'ggpmisc' does not define
#' new geometries matching the new statistics as they are not needed and
#' conceptually transformations of \code{data} are statistics in the grammar
#' of graphics.
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
#' @param mf.values logical Add R2, adjusted R2, p-value and n as columns to
#'   returned data? (`FALSE` by default.)
#' @param fullrange Should the fit span the full range of the plot, or just
#'   the data?
#' @param level Level of confidence interval to use (0.95 by default).
#' @param n Number of points at which to evaluate smoother.
#' @param orientation character Either "x" or "y" controlling the default for
#'   \code{formula}.
#'
#' @return The value returned by the statistic is a data frame, that will have
#'   \code{n} rows of predicted values and their confidence limits. Optionally
#'   it will also include additional values related to the model fit.
#'
#' @section Computed variables: `stat_poly_line()` provides the following
#'   variables, some of which depend on the orientation: \describe{ \item{y *or*
#'   x}{predicted value} \item{ymin *or* xmin}{lower pointwise confidence
#'   interval around the mean} \item{ymax *or* xmax}{upper pointwise confidence
#'   interval around the mean} \item{se}{standard error} }
#'
#'   If \code{mf.values = TRUE} is passed then columns based on the summary of
#'   the model fit are added, with the same value in each row within a group.
#'   This is wasteful and disabled by default, but provides a simple and robust
#'   approach to achieve effects like colouring or hiding of the model fit line
#'   based on P-values, r-squared, adjusted r-squared or the number of
#'   observations.
#'
#' @section Aesthetics: \code{stat_poly_line} understands \code{x} and \code{y},
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
#' # Smooths are automatically fit to each group (defined by categorical
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
#' # Inspecting the returned data using geom_debug()
#' if (requireNamespace("gginnards", quietly = TRUE)) {
#'   library(gginnards)
#'
#'   ggplot(mpg, aes(displ, hwy)) +
#'     stat_poly_line(geom = "debug")
#'
#'   ggplot(mpg, aes(displ, hwy)) +
#'     stat_poly_line(geom = "debug", mf.values = TRUE)
#'
##' }
#'
#' @export
#'
stat_poly_line <- function(mapping = NULL, data = NULL,
                           geom = "smooth", position = "identity",
                           ...,
                           method = "lm",
                           formula = NULL,
                           se = TRUE,
                           mf.values = FALSE,
                           n = 80,
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
      method <- "lm"
    } else if (method == "auto") {
      message("Method 'auto' is equivalent to 'lm', please use 'stat_smooth()' for splines.")
      method <- "lm"
    }
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatPolyLine,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      se = se,
      mf.values = mf.values,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      orientation = orientation,
      method.args = method.args,
      ...
    )
  )
}

poly_line_compute_group_fun <-
  function(data, scales,
           method = NULL,
           formula = NULL,
           se = TRUE,
           mf.values = FALSE,
           n = 80,
           fullrange = FALSE,
           xseq = NULL,
           level = 0.95,
           method.args = list(),
           na.rm = FALSE,
           flipped_aes = NA,
           orientation = "x") {
    data <- ggplot2::flip_data(data, flipped_aes)
    if (length(unique(data$x)) < 2) {
      # Not enough data to perform fit
      return(data.frame())
    }

    if (is.null(data$weight)) data$weight <- 1

    if (is.null(xseq)) {
      if (fullrange) {
        xrange <- scales[[orientation]]$dimension()
      } else {
        xrange <- range(data$x, na.rm = TRUE)
      }
      xseq <- seq(from = xrange[1], to = xrange[2], length.out = n)
    }

    if (is.character(method)) {
      method <- match.fun(method)
    }

    base.args <- list(quote(formula),
                      data = quote(data),
                      weights = quote(weight))
    mf <- do.call(method, c(base.args, method.args))

    newdata <- data.frame(x = xseq)
    prediction <- stats::predict(mf,
                                 newdata = newdata,
                                 se.fit = se,
                                 level = level,
                                 interval = if (se) "confidence" else "none"
    )
    if (se) {
      if (exists("fit", prediction)) {
        prediction <- as.data.frame(prediction[["fit"]])
      }
      names(prediction) <- c("y", "ymin", "ymax")
    } else {
      prediction <- data.frame(y = as.vector(prediction))
    }
    prediction <- cbind(newdata, prediction)

    if (mf.values) {
      mf.summary <- summary(mf)
      # summary methods for different fit methods may no return some slots
      if (exists("fstatistic", mf.summary)) {
        prediction[["p.value"]] <-
          stats::pf(q = mf.summary[["fstatistic"]][["value"]],
                    df1 = mf.summary[["fstatistic"]][["numdf"]],
                    df2 = mf.summary[["fstatistic"]][["dendf"]],
                    lower.tail = FALSE)
      } else {
        prediction[["p.value"]] <- NA_real_
      }
      if (exists("r.squared", mf.summary)) {
        prediction[["r.squared"]] <- mf.summary[["r.squared"]]
      } else {
        prediction[["r.squared"]] <- NA_real_
      }
      if (exists("adj.r.squared", mf.summary)) {
        prediction[["adj.r.squared"]] <- mf.summary[["adj.r.squared"]]
      } else {
        prediction[["adj.r.squared"]] <- NA_real_
      }
      if (exists("residuals", mf)) {
        prediction[["n"]] <- length(mf[["residuals"]])
      } else {
        prediction[["n"]] <- NA_real_
      }
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

                   required_aes = c("x", "y")
  )


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
