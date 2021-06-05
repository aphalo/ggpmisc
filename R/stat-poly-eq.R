#' Equation, p-value, R^2, AIC or BIC of fitted polynomial
#'
#' \code{stat_poly_eq} fits a polynomial by default with \code{stats::lm()} but
#' alternatively using robust or quantile regression. From the fitted model it
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
#' @param method function or character If character, "lm", "rlm" and
#'   "rq" are accepted. If a function, it must have formal parameters
#'   \code{formula} and \code{data} and return a model fit object for which
#'   \code{summary()} and \code{coefficients()} are consistent with those for
#'   \code{lm} fits.
#' @param method.args named list with additional arguments.
#' @param eq.with.lhs If \code{character} the string is pasted to the front of
#'   the equation label before parsing or a \code{logical} (see note).
#' @param eq.x.rhs \code{character} this string will be used as replacement for
#'   \code{"x"} in the model equation when generating the label before parsing
#'   it.
#' @param coef.digits,f.digits integer Number of significant digits to use for
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
#'
#' @note For backward compatibility a logical is accepted as argument for
#'   \code{eq.with.lhs}, giving the same output than the current default
#'   character value. By default "x" is retained as independent variable as this
#'   is the name of the aesthetic. However, it can be substituted by providing a
#'   suitable replacement character string through \code{eq.x.rhs}.
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
#'   indicated by "- 1" or "-1" in the formula. The validity of the
#'   \code{formula} is not checked in the current implementation, and for this
#'   reason the default aesthetics sets R^2 as label for the annotation. This
#'   stat generates labels as R expressions by default (set \code{parse = TRUE})
#'   but LaTeX (use TikZ device) and markdown (use package 'ggtext')
#'   are also supported, as well as numeric values for user-generated text
#'   labels. The predicted values need to be separately added to the plot with
#'   \code{stat_smooth()} or \code{stat_quantile()}, so to make sure that the
#'   same model formula is used in both plot layers it is best to save the
#'   formula as an object and supply this object as argument to the different
#'   statistics.
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
#' columns:
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
#' If output.type is \code{"numeric"} the returned tibble contains columns:
#' \describe{
#'   \item{x,npcx}{x position}
#'   \item{y,npcy}{y position}
#'   \item{coef.ls}{list containing the "coefficients" matrix from the summary of the fit object}
#'   \item{r.squared, adj.r.squared, f.value, f.df1, f.df2, p.value, AIC, BIC, n}{numeric values, from the model fit object}
#'   \item{grp.label}{Set according to mapping in \code{aes}.}}
#'
#' To explore the computed values returned for a given input we suggest the use
#' of \code{\link[gginnards]{geom_debug}} as shown in the last examples below.
#'
#' @section Parsing may be required: if using the computed labels with
#'   \code{output.type = "expression"}, then \code{parse = TRUE} is needed,
#'   while if using \code{output.type = "LaTeX"} \code{parse = FALSE} is needed.
#'
#' @seealso This \code{stat_poly_eq} statistic can return ready formatted labels
#'   depending on the argument passed to \code{output.type}. This is possible
#'   because only polynomial models are supported. For other types of models,
#'   statistics \code{\link{stat_fit_glance}},  \code{\link{stat_fit_tidy}} and
#'   \code{\link{stat_fit_glance}} should be used instead and the code for
#'   construction of character strings from numeric values and their mapping to
#'   aesthetic \code{label} needs to be explicitly supplied in the call.
#'
#' @family statistics for linear model fits
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
#'   stat_poly_eq(formula = formula, parse = TRUE)
#'
#' # grouping
#' ggplot(my.data, aes(x, y, color = group)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula, parse = TRUE)
#'
#' # rotation
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula, parse = TRUE, angle = 90,
#'                hjust = 1)
#'
#' # label location
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula, parse = TRUE,
#'                label.y = "bottom", label.x = "right")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula, parse = TRUE,
#'                label.y = 0.1, label.x = 0.9)
#'
#' # using weights
#' ggplot(my.data, aes(x, y, weight = w)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula, parse = TRUE)
#'
#' # no weights, digits for R square
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula, rr.digits = 4, parse = TRUE)
#'
#' # user specified label
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(aes(label =  paste(stat(rr.label),
#'                                   stat(n.label), sep = "*\", \"*")),
#'                formula = formula, parse = TRUE)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(aes(label =  paste(stat(eq.label),
#'                                   stat(adj.rr.label), sep = "*\", \"*")),
#'                formula = formula, parse = TRUE)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(aes(label =  paste(stat(f.value.label),
#'                                   stat(p.value.label),
#'                                   sep = "*\", \"*")),
#'                formula = formula, parse = TRUE)
#'
#' # user specified label and digits
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(aes(label =  paste(stat(eq.label),
#'                                   stat(adj.rr.label),
#'                                   sep = "*\", \"*")),
#'                formula = formula, rr.digits = 3, coef.digits = 4,
#'                parse = TRUE)
#'
#' # conditional user specified label
#' ggplot(my.data, aes(x, y, color = group)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(aes(label =  ifelse(stat(adj.r.squared) > 0.96,
#'                                    paste(stat(adj.rr.label),
#'                                          stat(eq.label),
#'                                          sep = "*\", \"*"),
#'                                    stat(adj.rr.label))),
#'                rr.digits = 3,
#'                formula = formula,
#'                parse = TRUE)
#'
#' # geom = "text"
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(geom = "text", label.x = 100, label.y = 0, hjust = 1,
#'                formula = formula, parse = TRUE)
#'
#' # using numeric values
#' # Here we use column "Estimate" from the matrix.
#' # Other available columns are "Std. Error", "t value" and "Pr(>|t|)".
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
#'                                     stat(coef.ls)[[1]][[1, "Estimate"]],
#'                                     stat(coef.ls)[[1]][[2, "Estimate"]],
#'                                     stat(coef.ls)[[1]][[3, "Estimate"]],
#'                                     stat(coef.ls)[[1]][[4, "Estimate"]])
#'                                     )
#'                    )
#'
#' # Examples using geom_debug() to show computed values
#' #
#' # This provides a quick way of finding out which variables are available for
#' # use in mapping of aesthetics when using other geoms as in the examples
#' # above.
#'
#' library(gginnards)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula, geom = "debug")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(aes(label = stat(eq.label)),
#'                formula = formula, geom = "debug",
#'                output.type = "markdown")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula, geom = "debug", output.type = "text")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula, geom = "debug", output.type = "numeric")
#'
#' # show the content of a list column
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula, geom = "debug", output.type = "numeric",
#'                summary.fun = function(x) {x[["coef.ls"]][[1]]})
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
                         coef.digits = 3,
                         rr.digits = 2,
                         f.digits = 3,
                         p.digits = 3,
                         label.x = "left",
                         label.y = "top",
                         label.x.npc = NULL,
                         label.y.npc = NULL,
                         hstep = 0,
                         vstep = NULL,
                         output.type = "expression",
                         na.rm = FALSE,
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
                  coef.digits = coef.digits,
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
                                      coef.digits,
                                      rr.digits,
                                      f.digits,
                                      p.digits,
                                      label.x,
                                      label.y,
                                      hstep,
                                      vstep,
                                      npc.used,
                                      output.type,
                                      na.rm) {
  force(data)
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

  if (is.null(eq.x.rhs)) {
    if (output.type == "expression") {
      eq.x.rhs <- "~italic(x)"
    } else if (output.type == "markdown") {
      eq.x.rhs <- "_x_"
    } else{
      eq.x.rhs <- " x"
    }
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

  if (length(unique(data$x)) < 2) {
    warning("Not enough data to perform fit for group ",
            group.idx, "; computing mean instead.",
            call. = FALSE)
    formula = y ~ 1
  }

  stopifnot(!any(c("formula", "data") %in% names(method.args)))
  if (is.function(method)) {
    fun <- method
  } else if (is.character(method)) {
    if (method == "rq" && length(method.args) == 0) {
      method.args <- list(tau = 0.5)
    }
    fun <- switch(method,
                  lm = stats::lm,
                  rlm = MASS::rlm,
                  rq = quantreg::rq,
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

  if (output.type == "numeric") {
    z <- tibble::tibble(coef.ls = list(summary(mf)[["coefficients"]]),
                        coefs = list(stats::coefficients(mf)),
                        r.squared = rr,
                        adj.r.squared = adj.rr,
                        f.value = f.value,
                        f.df1 = f.df1,
                        f.df2 = f.df2,
                        p.value = p.value,
                        AIC = AIC,
                        BIC = BIC,
                        n = n,
                        rr.label = "") # needed for default 'label' mapping
  } else {
    coefs <- stats::coef(mf)
    formula.rhs.chr <- as.character(formula)[3]
    if (grepl("-1", formula.rhs.chr) || grepl("- 1", formula.rhs.chr)) {
      coefs <- c(0, coefs)
    }

    stopifnot(coef.digits > 0)
    if (coef.digits < 3) {
      warning("'coef.digits < 3' Likely information loss!")
    }
    eq.char <- as.character(signif(polynom::as.polynomial(coefs), coef.digits))
    # as character drops 1
    eq.char <-
      gsub("+ x",
           paste("+ 1.", stringr::str_dup("0", coef.digits - 1L), "*x",
                 sep = ""),
           eq.char, fixed = TRUE)
    eq.char <- gsub("e([+-]?[0-9]*)", "%*%10^{\\1}", eq.char)
    if (output.type %in% c("latex", "tex", "tikz", "markdown")) {
      eq.char <- gsub("*", " ", eq.char, fixed = TRUE)
    }
    if (is.character(eq.with.lhs)) {
      lhs <- eq.with.lhs
      eq.with.lhs <- TRUE
    } else if (eq.with.lhs) {
      if (output.type == "expression") {
        lhs <- "italic(y)~`=`~"
      } else if (output.type %in% c("latex", "tex", "tikz", "text")) {
        lhs <- "y = "
      } else if (output.type == "markdown") {
        lhs <- "_y_ = "
      }
    }
    if (eq.with.lhs) {
      eq.char <- paste(lhs, eq.char, sep = "")
    }

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
    rr.char <- as.character(round(rr, digits = rr.digits))
    adj.rr.char <- as.character(round(adj.rr, digits = rr.digits))
    AIC.char <- sprintf("%.4g", AIC)
    BIC.char <- sprintf("%.4g", BIC)
    f.value.char <- as.character(signif(f.value, digits = f.digits))
    f.df1.char <- as.character(f.df1)
    f.df2.char <- as.character(f.df2)
    p.value.char <- as.character(round(p.value, digits = p.digits))
    if (output.type == "expression") {
      z <- tibble::tibble(eq.label = gsub("x", eq.x.rhs, eq.char, fixed = TRUE),
                          rr.label =
                            # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
                            ifelse(is.na(rr), character(0L),
                                   paste("italic(R)^2",
                                         ifelse(rr < 10^(-rr.digits) & rr != 0,
                                                as.character(10^(-rr.digits)),
                                                rr.char),
                                         sep = ifelse(rr < 10^(-rr.digits) & rr != 0,
                                                      "~`<`~",
                                                      "~`=`~"))),
                          adj.rr.label =
                            ifelse(is.na(adj.rr), character(0L),
                                   paste("italic(R)[adj]^2",
                                         ifelse(adj.rr < 10^(-rr.digits) & adj.rr != 0,
                                                as.character(10^(-rr.digits)),
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
                                   paste("italic(P)",
                                         ifelse(p.value < 10^(-p.digits),
                                                as.character(10^(-p.digits)),
                                                p.value.char),
                                         sep = ifelse(p.value < 10^(-p.digits),
                                                      "~`<`~",
                                                      "~`=`~"))),
                          n.label = paste("italic(n)~`=`~", n, sep = ""),
                          grp.label = grp.label,
                          r.squared = rr,
                          adj.r.squared = adj.rr,
                          p.value = p.value,
                          n = n)
    } else if (output.type %in% c("latex", "tex", "text", "tikz")) {
      z <- tibble::tibble(eq.label =
                            gsub("x", eq.x.rhs, eq.char, fixed = TRUE),
                          rr.label =
                            # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
                            ifelse(is.na(rr), character(0L),
                                   paste("R^2",
                                         ifelse(rr < 10^(-rr.digits), as.character(10^(-rr.digits)), rr.char),
                                         sep = ifelse(rr < 10^(-rr.digits), " < ", " = "))),
                          adj.rr.label =
                            ifelse(is.na(adj.rr), character(0L),
                                   paste("R_{adj}^2",
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
                                   paste("P",
                                         ifelse(p.value < 10^(-p.digits), as.character(10^(-p.digits)), p.value.char),
                                         sep = ifelse(p.value < 10^(-p.digits), " < ", " = "))),
                          n.label = paste("n = ", n, sep = ""),
                          grp.label = grp.label,
                          r.squared = rr,
                          adj.r.squared = adj.rr,
                          p.value = p.value,
                          n = n)
    } else if (output.type == "markdown") {
      z <- tibble::tibble(eq.label =
                            gsub("x", eq.x.rhs, eq.char, fixed = TRUE),
                          rr.label =
                            # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
                            ifelse(is.na(rr), character(0L),
                                   paste("_R_<sup>2</sup>",
                                         ifelse(rr < 10^(-rr.digits), as.character(10^(-rr.digits)), rr.char),
                                         sep = ifelse(rr < 10^(-rr.digits), " < ", " = "))),
                          adj.rr.label =
                            ifelse(is.na(adj.rr), character(0L),
                                   paste("_R_<sup>2</sup><sub>adj</sub>",
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
                                   paste("_P_",
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
                   compute_group = poly_eq_compute_group_fun,
                   default_aes =
                     ggplot2::aes(npcx = stat(npcx),
                                  npcy = stat(npcy),
                                  label = stat(rr.label),
                                  hjust = "inward", vjust = "inward",
                                  weight = 1),
                   required_aes = c("x", "y")
  )
