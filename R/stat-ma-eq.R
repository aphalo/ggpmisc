#' Equation, p-value, R^2 of major axis regression
#'
#' \code{stat_ma_eq} fits model II regressions. From the fitted model it
#' generates several labels including the equation, p-value,
#' coefficient of determination (R^2), and number of observations.
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
#' @param eq.with.lhs If \code{character} the string is pasted to the front of
#'   the equation label before parsing or a \code{logical} (see note).
#' @param eq.x.rhs \code{character} this string will be used as replacement for
#'   \code{"x"} in the model equation when generating the label before parsing
#'   it.
#' @param small.r,small.p logical Flags to switch use of lower case r and p for
#'   coefficient of determination and p-value.
#' @param coef.digits integer Number of significant digits to use for
#'   the fitted coefficients.
#' @param coef.keep.zeros logical Keep or drop trailing zeros when formatting
#'   the fitted coefficients and F-value.
#' @param decreasing logical It specifies the order of the terms in the
#'   returned character string; in increasing (default) or decreasing powers.
#' @param rr.digits,theta.digits,p.digits integer Number of digits after the
#'   decimal point to use for R^2, theta and P-value in labels. If \code{Inf},
#'   use exponential notation with three decimal places.
#' @param label.x,label.y \code{numeric} with range 0..1 "normalized parent
#'   coordinates" (npc units) or character if using \code{geom_text_npc()} or
#'   \code{geom_label_npc()}. If using \code{geom_text()} or \code{geom_label()}
#'   numeric in native data units. If too short they will be recycled.
#' @param hstep,vstep numeric in npc units, the horizontal and vertical step
#'   used between labels for different groups.
#' @param output.type character One of "expression", "LaTeX", "text",
#'   "markdown" or "numeric".
#' @param orientation character Either "x" or "y" controlling the default for
#'   \code{formula}.
#' @param parse logical Passed to the geom. If \code{TRUE}, the labels will be
#'   parsed into expressions and displayed as described in \code{?plotmath}.
#'   Default is \code{TRUE} if \code{output.type = "expression"} and
#'   \code{FALSE} otherwise.
#'
#' @note For backward compatibility a logical is accepted as argument for
#'   \code{eq.with.lhs}. If \code{TRUE}, the default is used, either
#'   \code{"x"} or \code{"y"}, depending on the argument passed to \code{formula}.
#'   However, \code{"x"} or \code{"y"} can be substituted by providing a
#'   suitable replacement character string through \code{eq.x.rhs}.
#'   Parameter \code{orientation} is redundant as it only affects the default
#'   for \code{formula} but is included for consistency with
#'   \code{ggplot2::stat_smooth()}.
#'
#'   Methods in \code{\link[lmodel2]{lmodel2}} are all computed always except
#'   for RMA that requires a numeric argument to at least one of \code{range.y}
#'   or \code{range.x}. The results for specific methods are extracted a
#'   posteriori from the model fit object. When a function is passed as argument
#'   to \code{method}, the method can be passed in a list to \code{method.args}
#'   as member \code{method}. More easily, the name of the function can be
#'   passed as a character string together with the \code{lmodel2}-supported
#'   method.
#'
#'   R option \code{OutDec} is obeyed based on its value at the time the plot
#'   is rendered, i.e., displayed or printed. Set \code{options(OutDec = ",")}
#'   for languages like Spanish or French.
#'
#' @details This stat can be used to automatically annotate a plot with \eqn{R^2},
#'   \eqn{P}-value, \eqn{n} and/or the fitted model equation. It supports linear major axis
#'   (MA), standard major axis (SMA) and ranged major axis (RMA) regression by
#'   means of function \code{\link[lmodel2]{lmodel2}}. Please see the
#'   documentation, including the vignette of package 'lmodel2' for details.
#'   The parameters in \code{stat_ma_eq()} follow the same naming as in function
#'   \code{lmodel2()}.
#'
#'   It is important to keep in mind that although the fitted line does not
#'   depend on whether the \eqn{x} or \eqn{y} appears on the rhs of the model
#'   formula, the numeric estimates for the parameters do depend on this.
#'
#'   A ggplot statistic receives as \code{data} a data frame that is not the one
#'   passed as argument by the user, but instead a data frame with the variables
#'   mapped to aesthetics. \code{stat_ma_eq()} mimics how \code{stat_smooth()}
#'   works, except that only linear regression can be fitted. Similarly to these
#'   statistics the model fits respect grouping, so the scales used for \code{x}
#'   and \code{y} should both be continuous scales rather than discrete.
#'
#'   The minimum number of observations with distinct values can be set through
#'   parameter \code{n.min}. The default \code{n.min = 2L} is the smallest
#'   possible value. However, model fits with very few observations are of
#'   little interest and using a larger number for \code{n.min} than the default
#'   is usually wise.
#'
#' @section Warning!: For the formatted equation to be valid, the fitted model
#'   must be a polynomial, with or without intercept. If defined using
#'   \code{poly()} the argument \code{raw = TRUE} must be passed. If defined
#'   manually as powers of \code{x}, \strong{the terms must be in order of
#'   increasing powers, with no missing intermediate power term.} Please, see
#'   examples below. Currently, no check on the model is used to validate that
#'   it is a polynomial, so failing to comply with this requirement results in
#'   the silent output of an erroneous formatted equation.
#'
#' @section Aesthetics: \code{stat_ma_eq} understands \code{x} and \code{y}, to
#'   be referenced in the \code{formula} while the \code{weight} aesthetic is
#'   ignored. Both \code{x} and \code{y} must be mapped to \code{numeric}
#'   variables. In addition, the aesthetics understood by the geom
#'   (\code{"text"} is the default) are understood and grouping respected.
#'
#'   \emph{Transformation of \code{x} or \code{y} within the model formula
#'   is not supported by \code{stat_ma_eq()}. In this case, transformations
#'   should never be applied in the model formula, but instead in the mapping
#'   of the variables within \code{aes}.}
#'
#' @return A data frame, with a single row and columns as described under
#'   \strong{Computed variables}. In cases when the number of observations is
#'   less than \code{n.min} a data frame with no rows or columns is returned
#'   rendered as an empty/invisible plot layer.
#'
#' @section Computed variables:
#' If output.type different from \code{"numeric"} the returned tibble contains
#' columns listed below. If the fitted model does not contain a given value,
#' the label is set to \code{character(0L)}.
#' \describe{
#'   \item{x,npcx}{x position}
#'   \item{y,npcy}{y position}
#'   \item{eq.label}{equation for the fitted polynomial as a character string to be parsed}
#'   \item{rr.label}{\eqn{R^2} of the fitted model as a character string to be parsed}
#'   \item{p.value.label}{P-value if available, depends on \code{method}.}
#'   \item{theta.label}{Angle in degrees between the two OLS lines for lines estimated from \code{y ~ x} and \code{x ~ y} linear model (\code{lm}) fits.}
#'   \item{n.label}{Number of observations used in the fit.}
#'   \item{grp.label}{Set according to mapping in \code{aes}.}
#'   \item{method.label}{Set according \code{method} used.}
#'   \item{r.squared, theta, p.value, n}{numeric values, from the model fit object}}
#'
#' If output.type is \code{"numeric"} the returned tibble contains columns
#' listed below. If the model fit function used does not return a value,
#' the variable is set to \code{NA_real_}.
#' \describe{
#'   \item{x,npcx}{x position}
#'   \item{y,npcy}{y position}
#'   \item{coef.ls}{list containing the "coefficients" matrix from the summary of the fit object}
#'   \item{r.squared, theta, p.value, n}{numeric values, from the model fit object}
#'   \item{grp.label}{Set according to mapping in \code{aes}.}
#'   \item{b_0.constant}{TRUE is polynomial is forced through the origin}
#'   \item{b_i}{One or two columns with the coefficient estimates}}
#'
#' To explore the computed values returned for a given input we suggest the use
#' of \code{\link[gginnards]{geom_debug}} as shown in the last examples below.
#'
#' @seealso The major axis regression model is fitted with function
#'   \code{\link[lmodel2]{lmodel2}}, please consult its documentation. Statistic
#'   \code{stat_ma_eq()} can return different ready formatted labels depending
#'   on the argument passed to \code{output.type}. If ordinary least squares
#'   polynomial regression is desired, then \code{\link{stat_poly_eq}}. If
#'   quantile-fitted polynomial regression is desired,
#'   \code{\link{stat_quant_eq}} should be used. For other types of models such
#'   as non-linear models, statistics \code{\link{stat_fit_glance}} and
#'   \code{\link{stat_fit_tidy}} should be used and the code for construction of
#'   character strings from numeric values and their mapping to aesthetic
#'   \code{label} explicitly supplied in the call.
#'
#' @family ggplot statistics for major axis regression
#'
#' @examples
#' # generate artificial data
#' set.seed(98723)
#' my.data <- data.frame(x = rnorm(100) + (0:99) / 10 - 5,
#'                       y = rnorm(100) + (0:99) / 10 - 5,
#'                       group = c("A", "B"))
#'
#' # using defaults (major axis regression)
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line() +
#'   stat_ma_eq()
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line() +
#'   stat_ma_eq(mapping = use_label("eq"))
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line() +
#'   stat_ma_eq(mapping = use_label("eq"), decreasing = TRUE)
#'
#' # use_label() can assemble and map a combined label
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(method = "MA") +
#'   stat_ma_eq(mapping = use_label(c("eq", "R2", "P")))
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(method = "MA") +
#'   stat_ma_eq(mapping = use_label(c("R2", "P", "theta", "method")))
#'
#' # using ranged major axis regression
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(method = "RMA",
#'                range.y = "interval",
#'                range.x = "interval") +
#'   stat_ma_eq(mapping = use_label(c("eq", "R2", "P")),
#'              method = "RMA",
#'              range.y = "interval",
#'              range.x = "interval")
#'
#' # No permutation-based test
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(method = "MA") +
#'   stat_ma_eq(mapping = use_label(c("eq", "R2")),
#'              method = "MA",
#'              nperm = 0)
#'
#' # explicit formula "x explained by y"
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(formula = x ~ y) +
#'   stat_ma_eq(formula = x ~ y,
#'              mapping = use_label(c("eq", "R2", "P")))
#'
#' # modifying both variables within aes()
#' ggplot(my.data, aes(log(x + 10), log(y + 10))) +
#'   geom_point() +
#'   stat_poly_line() +
#'   stat_poly_eq(mapping = use_label("eq"),
#'                eq.x.rhs = "~~log(x+10)",
#'                eq.with.lhs = "log(y+10)~~`=`~~")
#'
#' # grouping
#' ggplot(my.data, aes(x, y, color = group)) +
#'   geom_point() +
#'   stat_ma_line() +
#'   stat_ma_eq()
#'
#' # labelling equations
#' ggplot(my.data,
#'        aes(x, y,  shape = group, linetype = group, grp.label = group)) +
#'   geom_point() +
#'   stat_ma_line(color = "black") +
#'   stat_ma_eq(mapping = use_label(c("grp", "eq", "R2"))) +
#'   theme_classic()
#'
#' # Inspecting the returned data using geom_debug()
#' # This provides a quick way of finding out the names of the variables that
#' # are available for mapping to aesthetics with after_stat().
#'
#' gginnards.installed <- requireNamespace("gginnards", quietly = TRUE)
#'
#' if (gginnards.installed)
#'   library(gginnards)
#'
#' # default is output.type = "expression"
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_ma_eq(geom = "debug")
#'
#' \dontrun{
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_ma_eq(mapping = aes(label = after_stat(eq.label)),
#'                geom = "debug",
#'                output.type = "markdown")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_ma_eq(geom = "debug", output.type = "text")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_ma_eq(geom = "debug", output.type = "numeric")
#' }
#'
#' @export
#'
stat_ma_eq <- function(mapping = NULL, data = NULL,
                       geom = "text_npc",
                       position = "identity",
                       ...,
                       formula = NULL,
                       method = "lmodel2:MA",
                       method.args = list(),
                       n.min = 2L,
                       range.y = NULL,
                       range.x = NULL,
                       nperm = 99,
                       eq.with.lhs = TRUE,
                       eq.x.rhs = NULL,
                       small.r = FALSE,
                       small.p = FALSE,
                       coef.digits = 3,
                       coef.keep.zeros = TRUE,
                       decreasing = FALSE,
                       rr.digits = 2,
                       theta.digits = 2,
                       p.digits = max(1, ceiling(log10(nperm))),
                       label.x = "left",
                       label.y = "top",
                       hstep = 0,
                       vstep = NULL,
                       output.type = NULL,
                       na.rm = FALSE,
                       orientation = NA,
                       parse = NULL,
                       show.legend = FALSE,
                       inherit.aes = TRUE) {
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
    if (grepl("x", as.character(formula)[2])) {
      orientation <- "y"
    } else if (grepl("y", as.character(formula)[2])) {
      orientation <- "x"
    } else {
      stop("The model formula should use 'x' and 'y' as variables")
    }
  }

  if (is.null(output.type)) {
    if (geom %in% c("richtext", "textbox")) {
      output.type <- "markdown"
    } else {
      output.type <- "expression"
    }
  }
  if (is.null(parse)) {
    parse <- output.type == "expression"
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
    stat = StatMaEq,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params =
      rlang::list2(method = method,
                   method.args = method.args,
                   formula = formula,
                   n.min = n.min,
                   range.y = range.y,
                   range.x = range.x,
                   nperm = nperm,
                   eq.with.lhs = eq.with.lhs,
                   eq.x.rhs = eq.x.rhs,
                   small.r = small.r,
                   small.p = small.p,
                   coef.digits = coef.digits,
                   coef.keep.zeros = coef.keep.zeros,
                   decreasing = decreasing,
                   rr.digits = rr.digits,
                   theta.digits = theta.digits,
                   p.digits = p.digits,
                   label.x = label.x,
                   label.y = label.y,
                   hstep = hstep,
                   vstep = ifelse(is.null(vstep),
                                  ifelse(grepl("label", geom),
                                         0.10,
                                         0.05),
                                  vstep),
                   npc.used = grepl("_npc", geom),
                   output.type = output.type,
                   na.rm = na.rm,
                   orientation = orientation,
                   parse = parse,
                   ...)
  )
}

# Defined here to avoid a note in check --as-cran as the import from 'polynom'
# is not seen when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
ma_eq_compute_group_fun <- function(data,
                                    scales,
                                    method,
                                    method.args,
                                    formula,
                                    n.min,
                                    range.y,
                                    range.x,
                                    nperm,
                                    eq.with.lhs,
                                    eq.x.rhs,
                                    small.r,
                                    small.p,
                                    coef.digits,
                                    coef.keep.zeros,
                                    decreasing,
                                    rr.digits,
                                    theta.digits,
                                    p.digits,
                                    label.x,
                                    label.y,
                                    hstep,
                                    vstep,
                                    npc.used,
                                    output.type,
                                    na.rm,
                                    orientation) {
  force(data)

  if (length(unique(data$x)) < n.min ||
      length(unique(data$y)) < n.min) {
    return(data.frame())
  }

  # parse uses this option, but as for some labels or output types we do not use
  # parse() to avoid dropping of trailing zeros, we need to manage this in our
  # code in this case.
  decimal.mark <- getOption("OutDec", default = ".")
  if (!decimal.mark %in% c(".", ",")) {
    warning("Decimal mark must be one of '.' or ',', not: '", decimal.mark, "'")
    decimal.mark <- "."
  }
#  range.sep <- c("." = ", ", "," = "; ")[decimal.mark]

  output.type <- if (!length(output.type)) {
    "expression"
  } else {
    tolower(output.type)
  }
  stopifnot(output.type %in%
              c("expression", "text", "markdown", "numeric", "latex", "tex", "tikz"))

  if (exists("grp.label", data)) {
    if (length(unique(data[["grp.label"]])) > 1L) {
      warning("Non-unique value in 'data$grp.label' using group index ", data[["group"]][1], " as label.")
      grp.label <- as.character(data[["group"]][1])
    } else {
      grp.label <- data[["grp.label"]][1]
    }
  } else {
    # if nothing mapped to grp.label we use group index as label
    grp.label <- as.character(data[["group"]][1])
  }

  if (is.integer(data$group)) {
    group.idx <- abs(data$group[1])
  } else if (is.character(data$group) &&
             grepl("^(-1|[0-9]+).*$", data$group[1])) {
    # likely that 'gganimate' has set the groups for scenes
    # we assume first characters give the original group
    group.idx <- abs(as.numeric(gsub("^(-1|[0-9]+).*$", "\\1", data$group[1])))
  } else {
    group.idx <- NA_integer_
  }

  if (length(label.x) >= group.idx) {
    label.x <- label.x[group.idx]
  } else if (length(label.x) > 0) {
    label.x <- label.x[1]
  }

  if (length(label.y) >= group.idx) {
    label.y <- label.y[group.idx]
  } else if (length(label.y) > 0) {
    label.y <- label.y[1]
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

  if (!inherits(fm, "lmodel2")) {
    stop("Method \"", method.name, "\" did not return a \"lmodel2\" object")
  }
  fm.class <- class(fm)
  # allow model formula selection by the model fit method
  # extract formula from fitted model if possible, but fall back on argument if needed
  formula.ls <- fail_safe_formula(fm, fit.args, verbose = TRUE)

  n <- fm[["n"]]
  coefs <- stats::coefficients(fm, method = fun.method)
  rr <- fm[["rsquare"]]
  theta <- fm[["theta"]]
  idx <- which(fm[["regression.results"]][["Method"]] == fun.method)
  p.value <- fm[["regression.results"]][["P-perm (1-tailed)"]][idx]

  formula <- formula.ls[[1]]
  stopifnot(inherits(formula, what = "formula"))

  formula.rhs.chr <- as.character(formula)[3]
  forced.origin <- grepl("-[[:space:]]*1|+[[:space:]]*0", formula.rhs.chr)
  if (forced.origin) {
      coefs <- c(0, coefs)
  }
  names(coefs) <- paste("b", (1:length(coefs)) - 1, sep = "_")

  if (output.type == "numeric") {
    z <- tibble::tibble(r.squared = rr,
                        theta = theta,
                        p.value = p.value,
                        n = n,
                        rr.label = "",  # needed for default 'label' mapping
                        b_0.constant = forced.origin)
    z <- cbind(z, tibble::as_tibble_row(coefs))
  } else {
    # assemble the equation as a character string
    if (is.null(eq.x.rhs)) {
      eq.x.rhs <- build_eq.x.rhs(output.type = output.type,
                                 orientation = orientation)
    }

    if (is.character(eq.with.lhs)) {
      lhs <- eq.with.lhs
      eq.with.lhs <- TRUE
    } else if (eq.with.lhs) {
      lhs <- build_lhs(output.type = output.type,
                       orientation = orientation)
    } else {
      lhs <- character(0)
    }

    eq.char <- coefs2poly_eq(coefs = coefs,
                             coef.digits = coef.digits,
                             coef.keep.zeros = coef.keep.zeros,
                             decreasing = decreasing,
                             eq.x.rhs = eq.x.rhs,
                             lhs = lhs,
                             output.type = output.type,
                             decimal.mark = decimal.mark)

    # build the other character strings
    z <- data.frame(eq.label = eq.char,
                    rr.label = rr_label(value = rr,
                                        small.r = small.r,
                                        digits = rr.digits,
                                        output.type = output.type,
                                        decimal.mark = decimal.mark),
                    p.value.label = p_value_label(value = p.value,
                                                  subscript = "perm",
                                                  small.p = small.p,
                                                  digits = p.digits,
                                                  output.type = output.type,
                                                  decimal.mark = decimal.mark),
                    theta.label = italic_label(value = theta,
                                               value.name = ifelse(output.type %in% c("latex", "text", "tikz"),
                                                                   "\theta{}", "theta"),
                                               digits = theta.digits,
                                               fixed = TRUE,
                                               output.type = output.type,
                                               decimal.mark = decimal.mark),
                    n.label = italic_label(value = n,
                                           value.name = "n",
                                           digits = 0,
                                           fixed = TRUE,
                                           output.type = output.type,
                                           decimal.mark = decimal.mark),
                    grp.label = grp.label,
                    method.label = paste("\"method: ", method.name, "\"", sep = ""),
                    r.squared = rr,
                    theta = theta,
                    p.value = p.value,
                    n = n)
  }

  z[["fm.method"]] <- method.name
  z[["fm.class"]] <- fm.class[1]
  z[["fm.formula"]] <- formula.ls
  z[["fm.formula.chr"]] <- format(formula.ls)

  # Compute label positions
  if (is.character(label.x)) {
    if (npc.used) {
      margin.npc <- 0.05
    } else {
      # margin set by scale
      margin.npc <- 0
    }
    label.x <- ggpp::compute_npcx(x = label.x, group = group.idx, h.step = hstep,
                                  margin.npc = margin.npc)
    if (!npc.used) {
      # we need to use scale limits as observations are not necessarily plotted
      x.range <- scales$x$range$range
      label.x <- label.x * diff(x.range) + x.range[1]
    }
  }
  if (is.character(label.y)) {
    if (npc.used) {
      margin.npc <- 0.05
    } else {
      # margin set by scale
      margin.npc <- 0
    }
    label.y <- ggpp::compute_npcy(y = label.y, group = group.idx, v.step = vstep,
                                  margin.npc = margin.npc)
    if (!npc.used) {
      # we need to use scale limits as observations are not necessarily plotted
      y.range <- scales$y$range$range
      label.y <- label.y * diff(y.range) + y.range[1]
    }
  }

  if (npc.used) {
    z$npcx <- label.x
    z$x <- NA_real_
    z$npcy <- label.y
    z$y <- NA_real_
  } else {
    z$x <- label.x
    z$npcx <- NA_real_
    z$y <- label.y
    z$npcy <- NA_real_
  }

  z
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatMaEq <-
  ggplot2::ggproto("StatMaEq", ggplot2::Stat,
                   extra_params = c("na.rm", "parse"),
                   compute_group = ma_eq_compute_group_fun,
                   default_aes =
                     ggplot2::aes(npcx = after_stat(npcx),
                                  npcy = after_stat(npcy),
                                  label = after_stat(rr.label),
                                  hjust = "inward", vjust = "inward"),
                   required_aes = c("x", "y"),
                   optional_aes = "grp.label"
  )

