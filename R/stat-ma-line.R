#' Predicted line from major axis linear fit
#'
#' Predicted values and a confidence band are computed and, by default, plotted.
#' \code{stat_ma_line()} behaves similarly to \code{\link[ggplot2]{stat_smooth}}
#' except for fitting the model with \code{lmodel2::lmodel2()} with \code{"MA"}
#' as default for \code{method}.
#'
#' @details
#' This statistic fits major axis (\code{"MA"}) and other model II regressions
#' with function \code{\link[lmodel2]{lmodel2}}. Model II regression is called
#' for when both \code{x} and \code{y} are subject to random variation and the
#' intention is not to predict \code{y} from \code{x} by means of the model
#' but rather to study the relationship between two independent variables.
#' A frequent case in biology are allometric relationships among body parts.
#'
#' As the fitted line is the same wheter \code{x} or \code{y} is on the rhs of
#' the model equation, \code{orientation} even is accepted does not have an
#' effect on the fit. In contrast, \code{\link[ggplot2]{geom_smooth}} treats
#' each axis differently and can thus have two orientations. The orientation is
#' easy to deduce from the argument passed to \code{formula}. Thus,
#' \code{stat_ma_line()} will by default guess which orientation the layer
#' should have. If no argument is passed to \code{formula}, the orientation can
#' be specified directly passing an argument to the \code{orientation}
#' parameter, which can be either \code{"x"} or \code{"y"}. The value gives the
#' axis that is on the rhs of the model equation, \code{"x"} being the default
#' orientation. Package 'ggpmisc' does not define new geometries matching the
#' new statistics as they are not needed and conceptually transformations of
#' \code{data} are expressed as statistics.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}}. Only needs to be set at the layer level if you
#'   are overriding the plot defaults.
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
#' @param na.rm a logical indicating whether NA values should be stripped before
#'   the computation proceeds.
#' @param formula a formula object. Using aesthetic names \code{x} and \code{y}
#'   instead of original variable names.
#' @param range.y,range.x character Pass "relative" or "interval" if method
#'   "RMA" is to be computed.
#' @param method character "MA", "SMA" , "RMA" and "OLS".
#' @param nperm integer Number of permutation used to estimate significance.
#' @param se logical Return confidence interval around smooth? (`TRUE` by
#'   default, see `level` to control.)
#' @param mf.values logical Add R2, p-value and n as columns to returned data?
#'   (`FALSE` by default.)
#' @param fullrange Should the fit span the full range of the plot, or just
#'   the data?
#' @param level Level of confidence interval to use (only 0.95 currently).
#' @param n Number of points at which to evaluate smoother.
#' @param orientation character Either "x" or "y" controlling the default for
#'   \code{formula}.
#'
#' @return The value returned by the statistic is a data frame, that will have
#'   \code{n} rows of predicted values and and their confidence limits.
#'
#' @section Computed variables: `stat_ma_line()` provides the following
#'   variables, some of which depend on the orientation: \describe{ \item{y *or*
#'   x}{predicted value} \item{ymin *or* xmin}{lower pointwise confidence
#'   interval around the mean} \item{ymax *or* xmax}{upper pointwise confidence
#'   interval around the mean} \item{se}{standard error} }
#'
#' @section Aesthetics: \code{stat_ma_line} understands \code{x} and \code{y},
#'   to be referenced in the \code{formula}. Both must be mapped to
#'   \code{numeric} variables. In addition, the aesthetics understood by the
#'   geom (\code{"geom_smooth"} is the default) are understood and grouping
#'   respected.
#'
#' @family ggplot statistics for major axis regression
#'
#' @export
#'
#' @examples
#' # generate artificial data
#' set.seed(98723)
#' my.data <- data.frame(x = rnorm(100) + (0:99) / 10 - 5,
#'                       y = rnorm(100) + (0:99) / 10 - 5,
#'                       group = c("A", "B"))
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line()
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(method = "MA")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(method = "SMA")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(method = "RMA",
#'                range.y = "interval", range.x = "interval")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(method = "OLS")
#'
#' # plot line to the ends of range of data (the default)
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(fullrange = FALSE) +
#'   expand_limits(x = c(-10, 10), y = c(-10, 10))
#'
#' # plot line to the limits of the scales
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(fullrange = TRUE) +
#'   expand_limits(x = c(-10, 10), y = c(-10, 10))
#'
#' # plot line to the limits of the scales
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(orientation = "y", fullrange = TRUE) +
#'   expand_limits(x = c(-10, 10), y = c(-10, 10))
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(formula = x ~ y)
#'
#' # Smooths are automatically fit to each group (defined by categorical
#' # aesthetics or the group aesthetic) and for each facet.
#'
#' ggplot(my.data, aes(x, y, colour = group)) +
#'   geom_point() +
#'   stat_ma_line()
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line() +
#'   facet_wrap(~group)
#'
#' # Inspecting the returned data using geom_debug()
#' if (requireNamespace("gginnards", quietly = TRUE)) {
#'   library(gginnards)
#'
#'   ggplot(my.data, aes(x, y)) +
#'     stat_ma_line(geom = "debug")
#'
#'   ggplot(my.data, aes(x, y)) +
#'     stat_ma_line(geom = "debug", mf.values = TRUE)
#'
##' }
#'
#' @export
#'
stat_ma_line <- function(mapping = NULL,
                         data = NULL,
                         geom = "smooth",
                         position = "identity",
                         ...,
                         method = "MA",
                         formula = NULL,
                         range.y = NULL,
                         range.x = NULL,
                         se = TRUE,
                         mf.values = FALSE,
                         n = 80,
                         nperm = 99,
                         fullrange = FALSE,
                         level = 0.95,
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

  if (! method %in% c("MA", "SMA", "RMA", "OLS")) {
    warning("Method \"", method, "\" unknown, using \"MA\" instead.")
    method <- "MA"
  }
  if (method == "RMA" & (is.null(range.y) || is.null(range.x))) {
    stop("Method \"RMA\" is computed only if both 'range.x' and 'range.y' are set.")
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatMaLine,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      range.y = range.y,
      range.x = range.x,
      se = se,
      mf.values = mf.values,
      n = n,
      nperm = nperm,
      fullrange = fullrange,
      level = level,
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
ma_line_compute_group_fun <-
  function(data, scales, method = NULL, formula = NULL,
           range.y = NULL, range.x = NULL,
           se = TRUE, mf.values = FALSE,
           n = 80, nperm = 99, fullrange = FALSE,
           xseq = NULL, level = 0.95, method.args = list(),
           na.rm = FALSE, flipped_aes = NA, orientation = "x") {
    data <- ggplot2::flip_data(data, flipped_aes)
    if (length(unique(data$x)) < 2) {
      # Not enough data to perform fit
      return(data.frame())
    }

    if (is.null(xseq)) {
      if (fullrange) {
        xrange <- scales[[orientation]]$dimension()
      } else {
        xrange <- range(data$x, na.rm = TRUE)
      }
      xseq <- seq(from = xrange[1], to = xrange[2], length.out = n)
    }

    if (method == "RMA") {
      fit.args <-
        list(formula = formula,
             data = data,
             range.y = range.y,
             range.x = range.x,
             nperm = nperm
        )
    } else {
      fit.args <-
        list(formula = formula,
             data = data,
             nperm = nperm
        )
    }

    mf <- do.call(what = lmodel2::lmodel2, args = fit.args)

    newdata <- data.frame(x = xseq)

    prediction <- stats::predict(mf,
                                 method = method,
                                 newdata = newdata,
                                 interval = "confidence"
    )
    names(prediction) <- c("y", "ymin", "ymax")
    prediction <- cbind(newdata, prediction)
    if (mf.values) {
      idx <- which(mf[["regression.results"]][["Method"]] == method)
      prediction[["p.value"]] <- mf[["regression.results"]][["P-perm (1-tailed)"]][idx]
      prediction[["r.squared"]] <- mf[["rsquare"]]
      prediction[["n"]] <- mf[["n"]]
    }
    prediction$flipped_aes <- flipped_aes
    ggplot2::flip_data(prediction, flipped_aes)
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatMaLine <-
  ggplot2::ggproto("StatMaLine", Stat,
                   setup_params = function(data, params) {
                     params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
#                     message("`geom_ma_line()` using method ", params$method)
                     params
                   },

                   extra_params = c("na.rm", "orientation"),

                   compute_group = ma_line_compute_group_fun,

                   required_aes = c("x", "y")
  )
