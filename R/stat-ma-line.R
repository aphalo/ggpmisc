#' Predicted line from major axis linear fit
#'
#' Predicted values and a confidence band are computed and, by default, plotted.
#' \code{stat_ma_line()} behaves similarly to \code{\link[ggplot2]{stat_smooth}}
#' except for fitting the model with \code{lmodel2::lmodel2()} with \code{"MA"}
#' as default for \code{method}.
#'
#' @details This statistic fits major axis (\code{"MA"}) and other model II
#'   regressions with function \code{\link[lmodel2]{lmodel2}}. Model II
#'   regression is called for when both \code{x} and \code{y} are subject to
#'   random variation and the intention is not to predict \code{y} from \code{x}
#'   by means of the model but rather to study the relationship between two
#'   independent variables. A frequent case in biology are allometric
#'   relationships among body parts.
#'
#'   As the fitted line is the same whether \code{x} or \code{y} is on the rhs
#'   of the model equation, \code{orientation} even if accepted does not have an
#'   effect on the fitted line. In contrast, \code{\link[ggplot2]{geom_smooth}} treats
#'   each axis differently and can thus have two orientations. The orientation
#'   is easy to deduce from the argument passed to \code{formula}. Thus,
#'   \code{stat_ma_line()} will by default guess which orientation the layer
#'   should have. If no argument is passed to \code{formula}, the orientation
#'   can be specified directly passing an argument to the \code{orientation}
#'   parameter, which can be either \code{"x"} or \code{"y"}. The value gives
#'   the axis that is on the rhs of the model equation, \code{"x"} being the
#'   default orientation. Package 'ggpmisc' does not define new geometries
#'   matching the new statistics as they are not needed and conceptually
#'   transformations of \code{data} are expressed as statistics.
#'
#'   The minimum number of observations with distinct values can be set through
#'   parameter \code{n.min}. The default \code{n.min = 2L} is the smallest
#'   possible value. However, model fits with very few observations are of
#'   little interest and using a larger number for \code{n.min} than the default
#'   is wise.
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
#' @param method function or character If character, "MA", "SMA" , "RMA" or
#'   "OLS", alternatively "lmodel2" or the name of a model fit function are
#'   accepted, possibly followed by the fit function's \code{method} argument
#'   separated by a colon (e.g. \code{"lmodel2:MA"}). If a function different to
#'   \code{lmodel2()}, it must accept arguments named \code{formula},
#'   \code{data}, \code{range.y}, \code{range.x} and \code{nperm} and return a
#'   model fit object of class \code{lmodel2}.
#' @param method.args named list with additional arguments.
#' @param n.min integer Minimum number of distinct values in the explanatory
#'   variable (on the rhs of formula) for fitting to the attempted.
#' @param nperm integer Number of permutation used to estimate significance.
#' @param se logical Return confidence interval around smooth? (`TRUE` by
#'   default, see `level` to control.)
#' @param fm.values logical Add R2, p-value and n as columns to returned data?
#'   (`FALSE` by default.)
#' @param fullrange Should the fit span the full range of the plot, or just
#'   the data?
#' @param level Level of confidence interval to use (only 0.95 currently).
#' @param n Number of points at which to evaluate smoother.
#' @param orientation character Either "x" or "y" controlling the default for
#'   \code{formula}.
#'
#' @return The value returned by the statistic is a data frame, that will have
#'   \code{n} rows of predicted values and their confidence limits. Optionally
#'   it will also include additional values related to the model fit.
#'
#' @section Computed variables: `stat_ma_line()` provides the following
#'   variables, some of which depend on the orientation: \describe{ \item{y *or*
#'   x}{predicted value} \item{ymin *or* xmin}{lower pointwise confidence
#'   interval around the mean} \item{ymax *or* xmax}{upper pointwise confidence
#'   interval around the mean} \item{se}{standard error} }
#'
#'   If \code{fm.values = TRUE} is passed then columns based on the summary of
#'   the model fit are added, with the same value in each row within a group.
#'   This is wasteful and disabled by default, but provides a simple and robust
#'   approach to achieve effects like colouring or hiding of the model fit line
#'   based on P-values, r-squared or the number of observations.
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
#' gginnards.installed <- requireNamespace("gginnards", quietly = TRUE)
#'
#' if (gginnards.installed)
#'   library(gginnards)
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     stat_ma_line(geom = "debug")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     stat_ma_line(geom = "debug", fm.values = TRUE)
#'
#' @export
#'
stat_ma_line <- function(mapping = NULL,
                         data = NULL,
                         geom = "smooth",
                         position = "identity",
                         ...,
                         method = "lmodel2:MA",
                         method.args = list(),
                         n.min = 2L,
                         formula = NULL,
                         range.y = NULL,
                         range.x = NULL,
                         se = TRUE,
                         fm.values = FALSE,
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
      if (grepl("y", formula.chr[2])) {
        orientation <- "x"
      } else if (grepl("x", formula.chr[2])) {
        orientation <- "y"
        formula <- swap_xy(formula)
      }
    } else if (!grepl("y", formula.chr[2])){
      stop("When both 'orientation' and 'formula' are passed arguments ",
           "the formula should have 'x' as explanatory variable.")
    }
  }

  if (is.character(method)) {
    if (grepl("^rq", method)) {
      stop("Method 'rq' not supported, please use 'stat_quant_eq()'.")
    } else if (grepl("^lm$|^lm[:]|^rlm$|^rlm[:]", method)) {
      stop("Methods 'lm' and 'rlm' not supported, please use 'stat_poly_eq()'.")
    }
  }

  if (grepl("RMA$", method) && (is.null(range.y) || is.null(range.x))) {
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
    params = rlang::list2(
      method = method,
      method.args = method.args,
      n.min = n.min,
      formula = formula,
      range.y = range.y,
      range.x = range.x,
      se = se,
      fm.values = fm.values,
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
  function(data, scales,
           method = NULL,
           method.args = list(),
           n.min = 2L,
           formula = NULL,
           range.y = NULL, range.x = NULL,
           se = TRUE,
           fm.values = FALSE,
           n = 80,
           nperm = 99,
           fullrange = FALSE,
           xseq = NULL,
           level = 0.95,
           na.rm = FALSE,
           flipped_aes = NA,
           orientation = "x") {
    data <- ggplot2::flip_data(data, flipped_aes)
    if (length(unique(data$x)) < n.min) {
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

    # If method was specified as a character string, replace with
    # the corresponding function. Some model fit functions themselves have a
    # method parameter accepting character strings as argument. We support
    # these by splitting strings passed as argument at a colon.
    if (is.character(method)) {
      if (method %in% c("MA", "SMA", "RMA", "OLS")) {
        method <- paste("lmodel2", method, sep = ":")
      }
      if (method == "lmodel2") {
        method <- "lmodel2:MA"
      }
      method.name <- method
      method <- strsplit(x = method, split = ":", fixed = TRUE)[[1]]
      if (length(method) > 1L) {
        fun.method <- method[2]
        method <- method[1]
      } else {
        fun.method <- character()
      }
      if (method == "lmodel2") {
        method <- lmodel2::lmodel2
      } else {
        method <- match.fun(method)
      }
    } else if (is.function(method)) {
      fun.method <- method.args[["method"]]
      if (!length(fun.method)) {
        fun.method <- "MA"
      } else {
        method.args[["method"]] <- NULL
      }
      if (is.name(quote(method))) {
        method.name <- as.character(quote(method))
      } else {
        method.name <- "function"
      }
      method.name <- paste(method.name, fun.method, sep = ":")
    }

    if (! fun.method %in% c("MA", "SMA", "RMA", "OLS")) {
      warning("Method \"", method, "\" unknown, using \"MA\" instead.")
      method <- "MA"
    }

    if (fun.method == "RMA") {
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

    if (!grepl("^lmodel2", method.name)) {
      fit.args <- c(fit.args, method.args)
    }

    # lmodel2 issues a warning that is irrelevant here
    # so we silence it selectively
    withCallingHandlers({
      fm <- do.call(what = method, args = fit.args)
    }, message = function(w) {
      if (grepl("RMA was not requested", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleMessage")
      }
    })

    if (!length(fm) || (is.atomic(fm) && is.na(fm))) {
      return(data.frame())
    } else if (!inherits(fm, "lmodel2")) {
      stop("Method \"", method.name, "\" did not return a \"lmodel2\" object")
    }

    newdata <- data.frame(x = xseq)

    prediction <- stats::predict(fm,
                                 method = fun.method,
                                 newdata = newdata,
                                 interval = "confidence"
    )

    names(prediction) <- c("y", "ymin", "ymax")
    if (any(is.na(prediction$ymin)) || any(is.na(prediction$ymax))) {
      warning("Confidence band not available; see 'lmoldel2::lmodel2()'")
      prediction$ymin <- NULL
      prediction$ymax <- NULL
    }
    prediction <- cbind(newdata, prediction)

    if (fm.values) {
      idx <- which(fm[["regression.results"]][["Method"]] == fun.method)
      prediction[["p.value"]] <- fm[["regression.results"]][["P-perm (1-tailed)"]][idx]
      prediction[["r.squared"]] <- fm[["rsquare"]]
      prediction[["n"]] <- fm[["n"]]
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
