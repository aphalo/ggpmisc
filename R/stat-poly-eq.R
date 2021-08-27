#' Equation, p-value, R^2, AIC or BIC of fitted polynomial
#'
#' \code{stat_poly_eq} fits a polynomial by default with \code{stats::lm()} but
#' alternatively using robust regression. From the fitted model it
#' generates several labels including the equation, p-value, F-value,
#' coefficient of determination (R^2), 'AIC', 'BIC', and number of observations.
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
#' @param method function or character If character, "lm" and "rlm" are
#'   accepted. If a function, it must have formal parameters \code{formula} and
#'   \code{data} and return a model fit object for which \code{summary()} and
#'   \code{coefficients()} are consistent with those for \code{lm} fits.
#' @param method.args named list with additional arguments.
#' @param eq.with.lhs If \code{character} the string is pasted to the front of
#'   the equation label before parsing or a \code{logical} (see note).
#' @param eq.x.rhs \code{character} this string will be used as replacement for
#'   \code{"x"} in the model equation when generating the label before parsing
#'   it.
#' @param small.r,small.p logical Flags to switch use of lower case r and p for
#'   coefficient of determination and p-value.
#' @param coef.digits,f.digits integer Number of significant digits to use for
#'   the fitted coefficients and F-value.
#' @param coef.keep.zeros logical Keep or drop trailing zeros when formatting
#'   the fitted coefficients and F-value.
#' @param rr.digits,p.digits integer Number of digits after the decimal point to
#'   use for R^2 and P-value in labels.
#' @param label.x,label.y \code{numeric} with range 0..1 "normalized parent
#'   coordinates" (npc units) or character if using \code{geom_text_npc()} or
#'   \code{geom_label_npc()}. If using \code{geom_text()} or \code{geom_label()}
#'   numeric in native data units. If too short they will be recycled.
#' @param label.x.npc,label.y.npc \code{numeric} with range 0..1 (npc units)
#'   DEPRECATED, use label.x and label.y instead; together with a geom
#'   using npcx and npcy aesthetics.
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
#' @details This stat can be used to automatically annotate a plot with R^2,
#'   adjusted R^2 or the fitted model equation. It supports linear regression,
#'   robust linear regression and median regression fitted with functions
#'   \code{lm()}, \code{MASS::rlm()} or \code{quanreg::rq()}. The R^2 and
#'   adjusted R^2 annotations can be used with any linear model formula. The
#'   fitted equation label is correctly generated for polynomials or
#'   quasi-polynomials through the origin. Model formulas can use \code{poly()}
#'   or be defined algebraically with terms of powers of increasing magnitude
#'   with no missing intermediate terms, except possibly for the intercept
#'   indicated by "- 1" or "-1" or \code{"+ 0"} in the formula. The validity of
#'   the \code{formula} is not checked in the current implementation, and for
#'   this reason the default aesthetics sets R^2 as label for the annotation.
#'   This stat generates labels as R expressions by default but LaTeX (use TikZ
#'   device), markdown (use package 'ggtext') and plain text are also supported,
#'   as well as numeric values for user-generated text labels. The value of
#'   \code{parse} is set automatically based on \code{output-type}, but if you
#'   assemble labels that need parsing from \code{numeric} output, the default
#'   needs to be overriden. This stat only generates annotation labels, the
#'   predicted values/line need to be added to the plot as a separate layer
#'   using \code{\link{stat_poly_line}} or \code{\link[ggplot2]{stat_smooth}},
#'   so to make sure that the same model formula is used in all steps it is best
#'   to save the formula as an object and supply this object as argument to the
#'   different statistics.
#'
#'   A ggplot statistic receives as \code{data} a data frame that is not the one
#'   passed as argument by the user, but instead a data frame with the variables
#'   mapped to aesthetics. \code{stat_poly_eq()} mimics how \code{stat_smooth()}
#'   works, except that only polynomials can be fitted. Similarly to these
#'   statistics the model fits respect grouping, so the scales used for \code{x}
#'   and \code{y} should both be continuous scales rather than discrete.
#'
#' @references Written as an answer to a question at Stackoverflow.
#'   \url{https://stackoverflow.com/questions/7549694/adding-regression-line-equation-and-r2-on-graph}
#'
#' @section Aesthetics: \code{stat_poly_eq} understands \code{x} and \code{y},
#'   to be referenced in the \code{formula} and \code{weight} passed as argument
#'   to parameter \code{weights}. All three must be mapped to
#'   \code{numeric} variables. In addition, the aesthetics understood by the geom
#'   (\code{"text"} is the default) are understood and grouping respected.
#'
#' @section Computed variables:
#' If output.type different from \code{"numeric"} the returned tibble contains
#' columns listed below. If the model fit function used does not return a value,
#' the label is set to \code{character(0L)}.
#' \describe{
#'   \item{x,npcx}{x position}
#'   \item{y,npcy}{y position}
#'   \item{eq.label}{equation for the fitted polynomial as a character string to be parsed}
#'   \item{rr.label}{\eqn{R^2} of the fitted model as a character string to be parsed}
#'   \item{adj.rr.label}{Adjusted \eqn{R^2} of the fitted model as a character string to be parsed}
#'   \item{f.value.label}{F value and degrees of freedom for the fitted model as a whole.}
#'   \item{p.value.label}{P-value for the F-value above.}
#'   \item{AIC.label}{AIC for the fitted model.}
#'   \item{BIC.label}{BIC for the fitted model.}
#'   \item{n.label}{Number of observations used in the fit.}
#'   \item{grp.label}{Set according to mapping in \code{aes}.}
#'   \item{r.squared, adj.r.squared, p.value, n}{numeric values, from the model fit object}}
#'
#' If output.type is \code{"numeric"} the returned tibble contains columns
#' listed below. If the model fit function used does not return a value,
#' the variable is set to \code{NA_real_}.
#' \describe{
#'   \item{x,npcx}{x position}
#'   \item{y,npcy}{y position}
#'   \item{coef.ls}{list containing the "coefficients" matrix from the summary of the fit object}
#'   \item{r.squared, adj.r.squared, f.value, f.df1, f.df2, p.value, AIC, BIC, n}{numeric values, from the model fit object}
#'   \item{grp.label}{Set according to mapping in \code{aes}.}
#'   \item{b_0.constant}{TRUE is polynomial is forced through the origin}
#'   \item{b_i}{One or columns with the coefficient estimates}}
#'
#' To explore the computed values returned for a given input we suggest the use
#' of \code{\link[gginnards]{geom_debug}} as shown in the last examples below.
#'
#' @seealso This \code{stat_poly_eq} statistic can return ready formatted labels
#'   depending on the argument passed to \code{output.type}. This is possible
#'   because only polynomial models are supported. For quantile regression
#'   \code{\link{stat_quant_eq}} should be used instead of \code{stat_poly_eq}
#'   while for model II or major axis regression \code{\link{stat_ma_eq}} should
#'   be used. For other types of models such as non-linear models, statistics
#'   \code{\link{stat_fit_glance}} and \code{\link{stat_fit_tidy}} should be
#'   used and the code for construction of character strings from
#'   numeric values and their mapping to aesthetic \code{label} needs to be
#'   explicitly supplied by the user.
#'
#' @family ggplot statistics for linear and polynomial regression
#'
#' @examples
#' # generate artificial data
#' set.seed(4321)
#' x <- 1:100
#' y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
#' my.data <- data.frame(x = x, y = y,
#'                       group = c("A", "B"),
#'                       y2 = y * c(0.5,2),
#'                       w = sqrt(x))
#'
#' # give a name to a formula
#' formula <- y ~ poly(x, 3, raw = TRUE)
#'
#' # no weights
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula)
#'
#' # grouping
#' ggplot(my.data, aes(x, y, color = group)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula)
#'
#' # rotation
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula, angle = 90, hjust = 1)
#'
#' # label location
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula, label.y = "bottom", label.x = "right")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula, label.y = 0.1, label.x = 0.9)
#'
#' # using weights
#' ggplot(my.data, aes(x, y, weight = w)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula)
#'
#' # no weights, digits for R square
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula, rr.digits = 4)
#'
#' # user specified label
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(aes(label =  paste(after_stat(rr.label),
#'                                   after_stat(n.label), sep = "*\", \"*")),
#'                formula = formula)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(aes(label =  paste(after_stat(eq.label),
#'                                   after_stat(adj.rr.label), sep = "*\", \"*")),
#'                formula = formula)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(aes(label =  paste(after_stat(f.value.label),
#'                                   after_stat(p.value.label),
#'                                   sep = "*\", \"*")),
#'                formula = formula)
#'
#' # x on y regression
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula, orientation = "y") +
#'   stat_poly_eq(aes(label =  paste(after_stat(eq.label),
#'                                   after_stat(adj.rr.label),
#'                                   sep = "*\", \"*")),
#'                formula = x ~ poly(y, 3, raw = TRUE))
#'
#' # conditional user specified label
#' ggplot(my.data, aes(x, y, color = group)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(aes(label =  ifelse(after_stat(adj.r.squared) > 0.96,
#'                                    paste(after_stat(adj.rr.label),
#'                                          after_stat(eq.label),
#'                                          sep = "*\", \"*"),
#'                                    after_stat(adj.rr.label))),
#'                rr.digits = 3,
#'                formula = formula)
#'
#' # geom = "text"
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(geom = "text", label.x = 100, label.y = 0, hjust = 1,
#'                formula = formula)
#'
#' # using numeric values
#' # Here we use columns b_0 ... b_3 for the coefficient estimates
#' my.format <-
#'   "b[0]~`=`~%.3g*\", \"*b[1]~`=`~%.3g*\", \"*b[2]~`=`~%.3g*\", \"*b[3]~`=`~%.3g"
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula,
#'                output.type = "numeric",
#'                parse = TRUE,
#'                mapping =
#'                 aes(label = sprintf(my.format,
#'                                     after_stat(b_0), after_stat(b_1),
#'                                     after_stat(b_2), after_stat(b_3))))
#'
#' # Inspecting the returned data using geom_debug()
#' if (requireNamespace("gginnards", quietly = TRUE)) {
#'   library(gginnards)
#'
#' # This provides a quick way of finding out the names of the variables that
#' # are available for mapping to aesthetics.
#'
#' # the whole of data
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     geom_smooth(method = "lm", formula = formula) +
#'     stat_poly_eq(formula = formula, geom = "debug")
#'
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     geom_smooth(method = "lm", formula = formula) +
#'     stat_poly_eq(formula = formula, geom = "debug", output.type = "numeric")
#'
#' # names of the variables
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     geom_smooth(method = "lm", formula = formula) +
#'     stat_poly_eq(formula = formula, geom = "debug",
#'                  summary.fun = colnames)
#'
#' # only data$eq.label
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     geom_smooth(method = "lm", formula = formula) +
#'     stat_poly_eq(formula = formula, geom = "debug",
#'                  output.type = "expression",
#'                  summary.fun = function(x) {x[["eq.label"]]})
#'
#' # only data$eq.label
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     geom_smooth(method = "lm", formula = formula) +
#'     stat_poly_eq(aes(label = after_stat(eq.label)),
#'                  formula = formula, geom = "debug",
#'                  output.type = "markdown",
#'                  summary.fun = function(x) {x[["eq.label"]]})
#'
#' # only data$eq.label
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     geom_smooth(method = "lm", formula = formula) +
#'     stat_poly_eq(formula = formula, geom = "debug",
#'                  output.type = "latex",
#'                  summary.fun = function(x) {x[["eq.label"]]})
#'
#' # only data$eq.label
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     geom_smooth(method = "lm", formula = formula) +
#'     stat_poly_eq(formula = formula, geom = "debug",
#'                  output.type = "text",
#'                  summary.fun = function(x) {x[["eq.label"]]})
#'
#' # show the content of a list column
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     geom_smooth(method = "lm", formula = formula) +
#'     stat_poly_eq(formula = formula, geom = "debug", output.type = "numeric",
#'                  summary.fun = function(x) {x[["coef.ls"]][[1]]})
#' }
#'
#' @export
#'
stat_poly_eq <- function(mapping = NULL, data = NULL,
                         geom = "text_npc",
                         position = "identity",
                         ...,
                         method = "lm",
                         method.args = list(),
                         formula = NULL,
                         eq.with.lhs = TRUE,
                         eq.x.rhs = NULL,
                         small.r = FALSE,
                         small.p = FALSE,
                         coef.digits = 3,
                         coef.keep.zeros = TRUE,
                         rr.digits = 2,
                         f.digits = 3,
                         p.digits = 3,
                         label.x = "left",
                         label.y = "top",
                         label.x.npc = NULL,
                         label.y.npc = NULL,
                         hstep = 0,
                         vstep = NULL,
                         output.type = NULL,
                         na.rm = FALSE,
                         orientation = NA,
                         parse = NULL,
                         show.legend = FALSE,
                         inherit.aes = TRUE) {
  # backwards compatibility
  if (!is.null(label.x.npc)) {
    stopifnot(grepl("_npc", geom))
    label.x <- label.x.npc
  }
  if (!is.null(label.y.npc)) {
    stopifnot(grepl("_npc", geom))
    label.y <- label.y.npc
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
    stat = StatPolyEq,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(method = method,
                  method.args = method.args,
                  formula = formula,
                  eq.with.lhs = eq.with.lhs,
                  eq.x.rhs = eq.x.rhs,
                  small.r = small.r,
                  small.p = small.p,
                  coef.digits = coef.digits,
                  coef.keep.zeros = coef.keep.zeros,
                  rr.digits = rr.digits,
                  f.digits = f.digits,
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
poly_eq_compute_group_fun <- function(data,
                                      scales,
                                      method,
                                      method.args,
                                      formula,
                                      weight,
                                      eq.with.lhs,
                                      eq.x.rhs,
                                      small.r,
                                      small.p,
                                      coef.digits,
                                      coef.keep.zeros,
                                      rr.digits,
                                      f.digits,
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

  stopifnot(!any(c("formula", "data") %in% names(method.args)))
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

  output.type <- if (!length(output.type)) {
    "expression"
  } else {
    tolower(output.type)
  }
  stopifnot(output.type %in%
              c("expression", "text", "markdown", "numeric", "latex", "tex", "tikz"))

  if (is.null(data$weight)) {
    data$weight <- 1
  }

  if (exists("grp.label", data)) {
    if (length(unique(data[["grp.label"]])) > 1L) {
    warning("Non-unique value in 'data$grp.label' for group.")
      grp.label <- ""
    } else {
      grp.label <- data[["grp.label"]][1]
    }
  } else {
    grp.label <- ""
  }

  group.idx <- abs(data$group[1])
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

  if (orientation == "x") {
    if (length(unique(data$x)) < 2) {
      warning("Not enough data to perform fit for group ",
              group.idx, "; computing mean instead.",
              call. = FALSE)
      formula = y ~ 1
    }
  } else if (orientation == "y") {
    if (length(unique(data$y)) < 2) {
      warning("Not enough data to perform fit for group ",
              group.idx, "; computing mean instead.",
              call. = FALSE)
      formula = x ~ 1
    }
  }

  if (is.function(method)) {
    fun <- method
  } else if (is.character(method)) {
    fun <- switch(method,
                  lm = stats::lm,
                  rlm = MASS::rlm,
                  stop("Method '", method, "' not yet implemented.")
    )
  } else {
    stop("Method '", method, "' not yet implemented.")
  }

  # quantreg contains code with partial matching of names!
  # so we silence selectively only these warnings
  withCallingHandlers({
    mf <- do.call(fun,
                  args = c(list(formula = formula, data = data, weights = quote(weight)),
                           method.args))
    mf.summary <- summary(mf)
  }, warning = function(w) {
    if (startsWith(conditionMessage(w), "partial match of 'coef'") ||
        startsWith(conditionMessage(w), "partial argument match of 'contrasts'"))
      invokeRestart("muffleWarning")
  })

  if ("r.squared" %in% names(mf.summary)) {
    rr <- mf.summary[["r.squared"]]
  } else {
    rr <- NA_real_
  }
  if ("adj.r.squared" %in% names(mf.summary)) {
    adj.rr <- mf.summary[["adj.r.squared"]]
  } else {
    adj.rr <- NA_real_
  }
  AIC <- AIC(mf)
  BIC <- BIC(mf)
  n <- length(mf.summary[["residuals"]])
  if ("fstatistic" %in% names(mf.summary)) {
    f.value <- mf.summary[["fstatistic"]]["value"]
    f.df1 <- mf.summary[["fstatistic"]]["numdf"]
    f.df2 <- mf.summary[["fstatistic"]]["dendf"]
    p.value <- 1 - stats::pf(q = f.value, f.df1, f.df2)
  } else {
    f.value <- f.df1 <- f.df2 <- p.value <- NA_real_
  }
  coefs <- stats::coefficients(mf)

  formula.rhs.chr <- as.character(formula)[3]
  forced.origin <- grepl("-[[:space:]]*1|+[[:space:]]*0", formula.rhs.chr)
  if (forced.origin) {
      coefs <- c(0, coefs)
  }
  names(coefs) <- paste("b", (1:length(coefs)) - 1, sep = "_")

  if (output.type == "numeric") {
    z <- tibble::tibble(coef.ls = list(summary(mf)[["coefficients"]]),
                        r.squared = rr,
                        adj.r.squared = adj.rr,
                        f.value = f.value,
                        f.df1 = f.df1,
                        f.df2 = f.df2,
                        p.value = p.value,
                        AIC = AIC,
                        BIC = BIC,
                        n = n,
                        rr.label = "",  # needed for default 'label' mapping
                        b_0.constant = forced.origin)
    z <- cbind(z, tibble::as_tibble_row(coefs))
  } else {
    # set defaults needed to assemble the equation as a character string
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

    # build equation as a character string from the coefficient estimates
    eq.char <- coefs2poly_eq(coefs = coefs,
                             coef.digits = coef.digits,
                             coef.keep.zeros = coef.keep.zeros,
                             eq.x.rhs = eq.x.rhs,
                             lhs = lhs,
                             output.type = output.type)

    # build the other character strings
    stopifnot(rr.digits > 0)
    if (rr.digits < 2) {
      warning("'rr.digits < 2' Likely information loss!")
    }
    stopifnot(f.digits > 0)
    if (f.digits < 2) {
      warning("'f.digits < 2' Likely information loss!")
    }
    stopifnot(p.digits > 0)
    if (p.digits < 2) {
      warning("'p.digits < 2' Likely information loss!")
    }

    if (output.type == "expression") {
      rr.char <- sprintf("\"%#.*f\"", rr.digits, rr)
      adj.rr.char <- sprintf("\"%#.*f\"", rr.digits, adj.rr)
      AIC.char <- sprintf("\"%.4g\"", AIC)
      BIC.char <- sprintf("\"%.4g\"", BIC)
      f.value.char <- sprintf("\"%#.*g\"", f.digits, f.value)
      f.df1.char <- as.character(f.df1)
      f.df2.char <- as.character(f.df2)
      p.value.char <- sprintf("\"%#.*g\"", p.digits, p.value)
    } else {
      rr.char <- sprintf("%#.*f", rr.digits, rr)
      adj.rr.char <- sprintf("%#.*f", rr.digits, adj.rr)
      AIC.char <- sprintf("%.4g", AIC)
      BIC.char <- sprintf("%.4g", BIC)
      f.value.char <- sprintf("%#.*g", f.digits, f.value)
      f.df1.char <- as.character(f.df1)
      f.df2.char <- as.character(f.df2)
      p.value.char <- sprintf("%#.*g", p.digits, p.value)
    }

    # build the data frames to return
    if (output.type == "expression") {
      z <- tibble::tibble(eq.label = eq.char,
                          rr.label =
                            # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
                            ifelse(is.na(rr), character(0L),
                                   paste(ifelse(small.r, "italic(r)^2", "italic(R)^2"),
                                         ifelse(rr < 10^(-rr.digits) & rr != 0,
                                                sprintf("\"%.*f\"", rr.digits, 10^(-rr.digits)),
                                                rr.char),
                                         sep = ifelse(rr < 10^(-rr.digits) & rr != 0,
                                                      "~`<`~",
                                                      "~`=`~"))),
                          adj.rr.label =
                            ifelse(is.na(adj.rr), character(0L),
                                   paste(ifelse(small.r, "italic(r)[adj]^2", "italic(R)[adj]^2"),
                                         ifelse(adj.rr < 10^(-rr.digits) & adj.rr != 0,
                                                sprintf("\"%.*f\"", rr.digits, 10^(-rr.digits)),
                                                adj.rr.char),
                                         sep = ifelse(adj.rr < 10^(-rr.digits) & adj.rr != 0,
                                                      "~`<`~",
                                                      "~`=`~"))),
                          AIC.label =
                            ifelse(is.na(AIC), character(0L),
                                   paste("AIC", AIC.char, sep = "~`=`~")),
                          BIC.label =
                            ifelse(is.na(BIC), character(0L),
                                   paste("BIC", BIC.char, sep = "~`=`~")),
                          f.value.label =
                            ifelse(is.na(f.value), character(0L),
                                   paste("italic(F)[", f.df1.char,
                                         "*\",\"*", f.df2.char,
                                         "]~`=`~", f.value.char, sep = "")),
                          p.value.label =
                            ifelse(is.na(p.value), character(0L),
                                   paste(ifelse(small.p, "italic(p)",  "italic(P)"),
                                         ifelse(p.value < 10^(-p.digits),
                                                sprintf("\"%.*f\"", p.digits, 10^(-p.digits)),
                                                p.value.char),
                                         sep = ifelse(p.value < 10^(-p.digits),
                                                      "~`<`~",
                                                      "~`=`~"))),
                          n.label = paste("italic(n)~`=`~\"", n, "\"", sep = ""),
                          grp.label = grp.label,
                          r.squared = rr,
                          adj.r.squared = adj.rr,
                          p.value = p.value,
                          n = n)
    } else if (output.type %in% c("latex", "tex", "text", "tikz")) {
      z <- tibble::tibble(eq.label = eq.char,
                          rr.label =
                            # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
                            ifelse(is.na(rr), character(0L),
                                   paste(ifelse(small.r, "r^2", "R^2"),
                                         ifelse(rr < 10^(-rr.digits), as.character(10^(-rr.digits)), rr.char),
                                         sep = ifelse(rr < 10^(-rr.digits), " < ", " = "))),
                          adj.rr.label =
                            ifelse(is.na(adj.rr), character(0L),
                                   paste(ifelse(small.r, "r_{adj}^2", "R_{adj}^2"),
                                         ifelse(adj.rr < 10^(-rr.digits), as.character(10^(-rr.digits)), adj.rr.char),
                                         sep = ifelse(adj.rr < 10^(-rr.digits), " < ", " = "))),
                          AIC.label =
                            ifelse(is.na(AIC), character(0L),
                                   paste("AIC", AIC.char, sep = " = ")),
                          BIC.label =
                            ifelse(is.na(BIC), character(0L),
                                   paste("BIC", BIC.char, sep = " = ")),
                          f.value.label =
                            ifelse(is.na(f.value), character(0L),
                                   paste("F_{", f.df1.char, ",", f.df2.char,
                                         "} = ", f.value.char, sep = "")),
                          p.value.label =
                            ifelse(is.na(p.value), character(0L),
                                   paste(ifelse(small.p, "p",  "P"),
                                         ifelse(p.value < 10^(-p.digits), as.character(10^(-p.digits)), p.value.char),
                                         sep = ifelse(p.value < 10^(-p.digits), " < ", " = "))),
                          n.label = paste("n = ", n, sep = ""),
                          grp.label = grp.label,
                          r.squared = rr,
                          adj.r.squared = adj.rr,
                          p.value = p.value,
                          n = n)
    } else if (output.type == "markdown") {
      z <- tibble::tibble(eq.label = eq.char,
                          rr.label =
                            # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
                            ifelse(is.na(rr), character(0L),
                                   paste(ifelse(small.r, "_r_<sup>2</sup>", "_R_<sup>2</sup>"),
                                         ifelse(rr < 10^(-rr.digits), as.character(10^(-rr.digits)), rr.char),
                                         sep = ifelse(rr < 10^(-rr.digits), " < ", " = "))),
                          adj.rr.label =
                            ifelse(is.na(adj.rr), character(0L),
                                   paste(ifelse(small.r, "_r_<sup>2</sup><sub>adj</sub>", "_R_<sup>2</sup><sub>adj</sub>"),
                                         ifelse(adj.rr < 10^(-rr.digits), as.character(10^(-rr.digits)), adj.rr.char),
                                         sep = ifelse(adj.rr < 10^(-rr.digits), " < ", " = "))),
                          AIC.label =
                            ifelse(is.na(AIC), character(0L),
                                   paste("AIC", AIC.char, sep = " = ")),
                          BIC.label =
                            ifelse(is.na(BIC), character(0L),
                                   paste("BIC", BIC.char, sep = " = ")),
                          f.value.label =
                            ifelse(is.na(f.value), character(0L),
                                   paste("_F_<sub>", f.df1.char, ",", f.df2.char,
                                         "</sub> = ", f.value.char, sep = "")),
                          p.value.label =
                            ifelse(is.na(p.value), character(0L),
                                   paste(ifelse(small.p, "_p_", "_P_"),
                                         ifelse(p.value < 10^(-p.digits), as.character(10^(-p.digits)), p.value.char),
                                         sep = ifelse(p.value < 10^(-p.digits), " < ", " = "))),
                          n.label = paste("_n_ = ", n, sep = ""),
                          grp.label = grp.label,
                          r.squared = rr,
                          adj.r.squared = adj.rr,
                          p.value = p.value,
                          n = n)
    } else {
      warning("Unknown 'output.type' argument: ", output.type)
    }
  }

  # Compute label positions
  if (is.character(label.x)) {
    if (npc.used) {
      margin.npc <- 0.05
    } else {
      # margin set by scale
      margin.npc <- 0
    }
    label.x <- compute_npcx(x = label.x, group = group.idx, h.step = hstep,
                            margin.npc = margin.npc)
    if (!npc.used) {
      x.expanse <- abs(diff(range(data$x)))
      x.min <- min(data$x)
      label.x <- label.x * x.expanse + x.min
    }
  }
  if (is.character(label.y)) {
    if (npc.used) {
      margin.npc <- 0.05
    } else {
      # margin set by scale
      margin.npc <- 0
    }
    label.y <- compute_npcy(y = label.y, group = group.idx, v.step = vstep,
                            margin.npc = margin.npc)
    if (!npc.used) {
      y.expanse <- abs(diff(range(data$y)))
      y.min <- min(data$y)
      label.y <- label.y * y.expanse + y.min
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
StatPolyEq <-
  ggplot2::ggproto("StatPolyEq", ggplot2::Stat,
                   extra_params = c("na.rm", "parse"),
                   compute_group = poly_eq_compute_group_fun,
                   default_aes =
                     ggplot2::aes(npcx = after_stat(npcx),
                                  npcy = after_stat(npcy),
                                  label = after_stat(rr.label),
                                  hjust = "inward", vjust = "inward",
                                  weight = 1),
                   required_aes = c("x", "y")
  )

### Utility functions shared between stat_poly_eq() and stat_quant_eq()
# when stable will be exported
#
build_eq.x.rhs <- function(output.type = "expression",
                           orientation = "x") {
  if (orientation == "x") {
    if (output.type == "expression") {
      "~italic(x)"
    } else if (output.type == "markdown") {
      "_x_"
    } else{
      " x"
    }
  } else if (orientation == "y") {
    if (output.type == "expression") {
      "~italic(y)"
    } else if (output.type == "markdown") {
      "_y_"
    } else{
      " y"
    }
  }
}

build_lhs <- function(output.type = "expression",
                      orientation = "x") {
  if (orientation == "x") {
    if (output.type == "expression") {
      "italic(y)~`=`~"
    } else if (output.type %in% c("latex", "tex", "tikz", "text")) {
       "y = "
    } else if (output.type == "markdown") {
      "_y_ = "
    }
  } else if (orientation == "y") {
    if (output.type == "expression") {
      "italic(x)~`=`~"
    } else if (output.type %in% c("latex", "tex", "tikz", "text")) {
      "x = "
    } else if (output.type == "markdown") {
      "_x_ = "
    }
  }
}

coefs2poly_eq <- function(coefs,
                          coef.digits = 3L,
                          coef.keep.zeros = TRUE,
                          eq.x.rhs = "x",
                          lhs = "y~`=`~",
                          output.type = "expression") {
  # build equation as a character string from the coefficient estimates
  stopifnot(coef.digits > 0)
  if (coef.digits < 3) {
    warning("'coef.digits < 3' Likely information loss!")
  }
  eq.char <- as.character(polynom::as.polynomial(coefs),
                          digits = coef.digits,
                          keep.zeros = coef.keep.zeros)
  if (output.type == "markdown") {
    eq.char <- gsub("e([+-]?)[0]([1-9]*)", "&times;10<sup>\\1\\2</sup>", eq.char)
    eq.char <- gsub("[:^]([0-9]*)", "<sup>\\1</sup>", eq.char)
    eq.char <- gsub("*", "&nbsp;", eq.char, fixed = TRUE)
    eq.char <- gsub(" ", "", eq.char, fixed = TRUE)
  } else {
    eq.char <- gsub("e([+-]?[0-9]*)", "%*%10^{\\1}", eq.char)
    # muliplication symbol
    if (output.type %in% c("latex", "tikz")) {
      eq.char <- gsub("%*%", "\\times{}", eq.char, fixed = TRUE)
      eq.char <- gsub("*", "", eq.char, fixed = TRUE)
    }else if (output.type == "text") {
      eq.char <- gsub("[*][:space:]", " ", eq.char, fixed = TRUE)
      eq.char <- gsub("%*%", " * ", eq.char, fixed = TRUE)
    }
  }

  if (eq.x.rhs != "x") {
    eq.char <- gsub("x", eq.x.rhs, eq.char, fixed = TRUE)
  }
  if (length(lhs)) {
    eq.char <- paste(lhs, eq.char, sep = "")
  }

  eq.char
}

# based on idea in answer by slamballais to Stackoverflow question
# at https://stackoverflow.com/questions/67942485/
#
# This is an edit of the code in package 'polynom' so that trailing zeros are
# retained during the conversion
#
as.character.polynomial <- function (x,
                                     decreasing = FALSE,
                                     digits = 3,
                                     keep.zeros = TRUE) {
  if (keep.zeros) {
    p <- sprintf("%#.*g", digits, x)
  } else {
    p <- sprintf("%.*g", digits, x)
  }
  lp <- length(p) - 1
  names(p) <- 0:lp
  p <- p[as.numeric(p) != 0]
  if (length(p) == 0)
    return("0")
  if (decreasing)
    p <- rev(p)
  signs <- ifelse(as.numeric(p) < 0, "- ", "+ ")
  signs[1] <- if (signs[1] == "- ") "-" else ""
  np <- names(p)
  pow <- paste("x^", np, sep = "")
  pow[np == "0"] <- ""
  pow[np == "1"] <- "x"
  stars <- rep.int("*", length(p))
  stars[p == "" | pow == ""] <- ""
  p <- gsub("^-", "", p)
  paste0(signs, p, stars, pow, collapse = " ")
}

#' author:
#'
#' @noRd
#'
# polynomial2character <- function (x, decreasing = FALSE, digits = 2, nsmall = 2) {
#   p <- format(unclass(x), digits = digits, nsmall = nsmall)
#   lp <- length(p) - 1
#   names(p) <- 0:lp
#   p <- p[as.numeric(p) != 0]
#   if (length(p) == 0)
#     return("0")
#   if (decreasing)
#     p <- rev(p)
#   signs <- ifelse(as.numeric(p) < 0, "- ", "+")
#   signs[1] <- if (signs[1] == "- ") "-" else ""
#   np <- names(p)
#   pow <- paste("x^", np, sep = "")
#   pow[np == "0"] <- ""
#   pow[np == "1"] <- "x"
#   stars <- rep.int("*", length(p))
#   stars[p == "" | pow == ""] <- ""
#   paste0(signs, p, stars, pow, collapse = " ")
# }
