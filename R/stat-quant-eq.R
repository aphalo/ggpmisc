#' Equation, p-value, R^2, AIC or BIC from quantile regression
#'
#' \code{stat_quant_eq} fits a polynomial model by quantile regression and
#' generates several labels including the equation, p-value, coefficient of
#' determination (R^2), 'AIC' and 'BIC'.
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
#' @param formula a formula object. Using aesthetic names instead of
#'   original variable names.
#' @param quantiles numeric vector Values in 0..1 indicating the quantiles.
#' @param eq.with.lhs If \code{character} the string is pasted to the front of
#'   the equation label before parsing or a \code{logical} (see note).
#' @param eq.x.rhs \code{character} this string will be used as replacement for
#'   \code{"x"} in the model equation when generating the label before parsing
#'   it.
#' @param coef.digits,rho.digits integer Number of significant digits to use for
#'   the fitted coefficients and rho in labels.
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
#'   adjusted R^2 or the fitted model equation. It supports only linear models
#'   fitted with function \code{lm()}. The R^2 and adjusted R^2 annotations can
#'   be used with any linear model formula. The fitted equation label is
#'   correctly generated for polynomials or quasi-polynomials through the
#'   origin. Model formulas can use \code{poly()} or be defined algebraically
#'   with terms of powers of increasing magnitude with no missing intermediate
#'   terms, except possibly for the intercept indicated by "- 1" or "-1" in the
#'   formula. The validity of the \code{formula} is not checked in the current
#'   implementation, and for this reason the default aesthetics sets R^2 as
#'   label for the annotation.  This stat generates labels as R expressions by
#'   default but LaTeX (use TikZ device) and markdown (use package 'ggtext') are
#'   also supported, as well as numeric values for user-generated text labels.
#'   The value of \code{parse} is set automatically based on \code{output-type},
#'   but if you assemble labels that need parsing from \code{numeric} output,
#'   the default needs to be overriden. This stat only generates labels, the
#'   predicted values need to be separately added to the plot, so to make sure
#'   that the same model formula is used in all steps it is best to save the
#'   formula as an object and supply this object as argument to the different
#'   statistics.
#'
#'   A ggplot statistic receives as data a data frame that is not the one passed
#'   as argument by the user, but instead a data frame with the variables mapped
#'   to aesthetics. stat_quant_eq() mimics how stat_smooth() works, except that
#'   only polynomials can be fitted. In other words, it respects the grammar of
#'   graphics. This helps ensure that the model is fitted to the same data as
#'   plotted in other layers.
#'
#' @references Written as an answer to a question by Mark Neal at Stackoverflow.
#'   \url{https://stackoverflow.com/questions/65695409/is-there-a-neat-approach-to-label-a-ggplot-plot-with-the-equation-and-other-stat}
#'
#' @section Aesthetics: \code{stat_quant_eq} understands \code{x} and \code{y},
#'   to be referenced in the \code{formula} and \code{weight} passed as argument
#'   to parameter \code{weights} of \code{lm()}. All three must be mapped to
#'   \code{numeric} variables. In addition, the aesthetics undertood by the geom
#'   used (\code{"text"} by default) are understood and grouping respected.
#'
#' @section Computed variables:
#' If output.type different from \code{"numeric"} the returned tibble contains
#' columns:
#' \describe{
#'   \item{x,npcx}{x position}
#'   \item{y,npcy}{y position}
#'   \item{coef.ls, r.squared, adj.r.squared, AIC, BIC}{as numric values extracted from fit object}
#'   \item{eq.label}{equation for the fitted polynomial as a character string to be parsed}
#'   \item{rho.label}{\eqn{rho} of the fitted model as a character string to be parsed}
#'   \item{AIC.label}{AIC for the fitted model.}
#'   \item{n.label}{Number of observations used in the fit.}
#'   \item{rq.method}{character, method used.}
#'   \item{rho, n}{numeric values extracted or computed from fit object.}
#'   \item{hjust, vjust}{Set to "inward" to override the default of the "text" geom.}}
#'
#' If output.type is \code{"numeric"} the returned tibble contains columns:
#' \describe{
#'   \item{x,npcx}{x position}
#'   \item{y,npcy}{y position}
#'   \item{coef.ls}{list containing the "coefficients" matrix from the summary of the fit object}
#'   \item{rho, AIC, n}{numeric values extracted or computed from fit object}
#'   \item{rq.method}{character, method used.}
#'   \item{hjust, vjust}{Set to "inward" to override the default of the "text" geom.}}
#'
#' To explore the computed values returned for a given input we suggest the use
#' of \code{\link[gginnards]{geom_debug}} as shown in the example below.
#'
#' @seealso This \code{stat_quant_eq} statistic can return ready formatted labels
#'   depending on the argument passed to \code{output.type}. This is possible
#'   because only polynomial models are supported. For other types of models,
#'   statistics \code{\link{stat_fit_glance}},  \code{\link{stat_fit_tidy}} and
#'   \code{\link{stat_fit_glance}} should be used instead and the code for
#'   construction of character strings from numeric values and their mapping to
#'   aesthetic \code{label} needs to be explicitly supplied in the call.
#'
#' @note Support for the \code{angle} aesthetic is not automatic and requires
#'   that the user passes as argument suitable numeric values to override the
#'   defaults.
#'
#' @family ggplot statistics for model fits
#'
#' @import quantreg
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
#' # using defaults
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_quantile() +
#'   stat_quant_eq()
#'
#' # same formula as default
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_quantile(formula = y ~ x) +
#'   stat_quant_eq(formula = y ~ x)
#'
#' # explicit formula "x explained by y"
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#' #   geom_quantile() +
#'   stat_quant_eq(formula = x ~ y)
#'
#' # give a name to a formula
#' formula <- y ~ poly(x, 3, raw = TRUE)
#'
#' # no weights
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_quantile(formula = formula) +
#'   stat_quant_eq(formula = formula)
#'
#' # angle
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_quantile(formula = formula) +
#'   stat_quant_eq(formula = formula, angle = 90, hstep = 0.05, vstep = 0,
#'                 label.y = 0.9, hjust = 1)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_quantile(formula = formula) +
#'   stat_quant_eq(formula = formula, angle = 90,
#'                 hstep = 0.05, vstep = 0, hjust = 0,
#'                 label.y = 0.5)
#'
#' # user set quantiles
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_quantile(formula = formula, quantiles = 0.5) +
#'   stat_quant_eq(formula = formula, quantiles = 0.5)
#'
#' # grouping
#' ggplot(my.data, aes(x, y, color = group)) +
#'   geom_point() +
#'   geom_quantile(formula = formula) +
#'   stat_quant_eq(formula = formula)
#'
#' ggplot(my.data, aes(x, y, color = group)) +
#'   geom_point() +
#'   geom_quantile(formula = formula) +
#'   stat_quant_eq(formula = formula, angle = 90,
#'                 hstep = 0.05, vstep = 0, hjust = 0,
#'                 label.y = 0.5)
#'
#' # labelling equations
#' ggplot(my.data, aes(x, y,  shape = group, linetype = group,
#'        grp.label = group)) +
#'   geom_point() +
#'   geom_quantile(formula = formula, color = "black") +
#'   stat_quant_eq(aes(label = paste(stat(grp.label), stat(eq.label), sep = "*\": \"*")),
#'                 formula = formula) +
#'   theme_classic()
#'
#' # setting non-default quantiles
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_quantile(formula = formula,
#'                 quantiles = c(0.1, 0.5, 0.9)) +
#'   stat_quant_eq(formula = formula, parse = TRUE,
#'                quantiles = c(0.1, 0.5, 0.9))
#'
#' # Location of equations
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_quantile(formula = formula) +
#'   stat_quant_eq(formula = formula, label.y = "bottom", label.x = "right")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_quantile(formula = formula) +
#'   stat_quant_eq(formula = formula, label.y = 0.03, label.x = 0.95, vstep = 0.04)
#'
#' # using weights
#' ggplot(my.data, aes(x, y, weight = w)) +
#'   geom_point() +
#'   geom_quantile(formula = formula) +
#'   stat_quant_eq(formula = formula)
#'
#' # no weights, quantile set to upper boundary
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_quantile(formula = formula, quantiles = 1) +
#'   stat_quant_eq(formula = formula, quantiles = 1)
#'
#' # user specified label
#' ggplot(my.data, aes(x, y, color = group, grp.label = group)) +
#'   geom_point() +
#'   geom_quantile(method = "rq", formula = formula,
#'                 quantiles = c(0.05, 0.5, 0.95)) +
#'   stat_quant_eq(aes(label = paste(stat(grp.label), "*\": \"*",
#'                                    stat(eq.label), sep = "")),
#'                 quantiles = c(0.05, 0.5, 0.95),
#'                 formula = formula)
#'
#' # geom = "text"
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_quantile(method = "rq", formula = formula, quantiles = 0.5) +
#'   stat_quant_eq(label.x = "left", label.y = "top",
#'                 formula = formula)
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
#'   stat_quant_eq(formula = formula, geom = "debug")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_eq(aes(label = stat(eq.label)),
#'                formula = formula, geom = "debug",
#'                output.type = "markdown")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_eq(formula = formula, geom = "debug", output.type = "text")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_eq(formula = formula, geom = "debug", output.type = "numeric")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_eq(formula = formula, quantiles = c(0.25, 0.5, 0.75),
#'                 geom = "debug", output.type = "text")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_eq(formula = formula, quantiles = c(0.25, 0.5, 0.75),
#'                 geom = "debug", output.type = "numeric")
#'
#' # show the content of a list column
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_eq(formula = formula, geom = "debug", output.type = "numeric",
#'                summary.fun = function(x) {x[["coef.ls"]][[1]]})
#'
#' @export
#'
stat_quant_eq <- function(mapping = NULL, data = NULL,
                         geom = "text_npc",
                         position = "identity",
                         ...,
                         formula = NULL,
                         quantiles = c(0.25, 0.5, 0.75),
                         eq.with.lhs = TRUE,
                         eq.x.rhs = NULL,
                         coef.digits = 3,
                         rho.digits = 2,
                         label.x = "left", label.y = "top",
                         label.x.npc = NULL, label.y.npc = NULL,
                         hstep = 0,
                         vstep = NULL,
                         output.type = "expression",
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
  if (is.null(parse)) {
    parse <- output.type == "expression"
  }
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatQuantEq,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(formula = formula,
                  quantiles = quantiles,
                  eq.with.lhs = eq.with.lhs,
                  eq.x.rhs = eq.x.rhs,
                  coef.digits = coef.digits,
                  rho.digits = rho.digits,
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
quant_eq_compute_group_fun <- function(data,
                                       scales,
                                       formula,
                                       quantiles,
                                       weight,
                                       eq.with.lhs,
                                       eq.x.rhs,
                                       coef.digits,
                                       rho.digits,
                                       label.x,
                                       label.y,
                                       hstep,
                                       vstep,
                                       npc.used,
                                       output.type,
                                       na.rm,
                                       orientation) {
  force(data)
  num.quantiles <- length(quantiles)
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

  if (is.null(data[["weight"]])) {
    data[["weight"]] <- 1
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

  group.idx <- abs(data[["group"]][1])
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

  rq.args <- list(quote(formula),
                  tau = quantiles,
                  data = quote(data),
                  weights = quote(weight))

  # quantreg contains code with partial matching of names!
  # so we silence selectively only these warnings
  withCallingHandlers({
    mf <- do.call(quantreg::rq, rq.args)
    mf.summary <- summary(mf)
  }, warning = function(w) {
    if (startsWith(conditionMessage(w), "partial match of 'coef'") ||
        startsWith(conditionMessage(w), "partial argument match of 'contrasts'"))
      invokeRestart("muffleWarning")
  })

  if (class(mf.summary)[1L] == "summary.rq") {
    mf.summary <- list(mf.summary)
  }
  names(mf.summary) <- as.character(quantiles)

  AIC <- AIC(mf)
  n <- length(mf.summary[[1]][["residuals"]])
  rho <- mf[["rho"]]
  rq.method <- mf[["method"]]
  coefs <- mf[["coefficients"]]
  # ensure that coefs is consistent
  if (is.vector(coefs)) {
    coefs <- as.matrix(coefs)
    colnames(coefs) <- paste("tau=", mf[["tau"]])
  }

  formula.rhs.chr <- as.character(formula)[3]
  if (grepl("-[[:space:]]*1|+[[:space:]]*0", formula.rhs.chr)) {
    coefs <- rbind(rep(0, ncol(coefs)), coefs)
  }

  coefs.ls <- asplit(coefs, 2)

  z <- tibble::tibble()

  for (i in seq_along(quantiles)) {

  }
  if (output.type == "numeric") {
    z <- tibble::tibble(coef.ls = coefs.ls,
                        quantiles = quantiles,
                        rq.method = rq.method,
                        AIC = AIC,
                        rho = rho,
                        n = n,
                        eq.label = "") # needed for default 'label' mapping
  } else {
    # set defaults needed to assemble the equation as a character string
    if (is.null(eq.x.rhs)) {
      if (orientation == "x") {
        if (output.type == "expression") {
          eq.x.rhs <- "~italic(x)"
        } else if (output.type == "markdown") {
          eq.x.rhs <- "_x_"
        } else{
          eq.x.rhs <- " x"
        }
      } else if (orientation == "y") {
        if (output.type == "expression") {
          eq.x.rhs <- "~italic(y)"
        } else if (output.type == "markdown") {
          eq.x.rhs <- "_y_"
        } else{
          eq.x.rhs <- " y"
        }
      }
    }

    if (is.character(eq.with.lhs)) {
      lhs <- eq.with.lhs
      eq.with.lhs <- TRUE
    } else if (eq.with.lhs) {
      if (orientation == "x") {
        if (output.type == "expression") {
          lhs <- "italic(y)~`=`~"
        } else if (output.type %in% c("latex", "tex", "tikz", "text")) {
          lhs <- "y = "
        } else if (output.type == "markdown") {
          lhs <- "_y_ = "
        }
      } else if (orientation == "y") {
        if (output.type == "expression") {
          lhs <- "italic(x)~`=`~"
        } else if (output.type %in% c("latex", "tex", "tikz", "text")) {
          lhs <- "x = "
        } else if (output.type == "markdown") {
          lhs <- "_x_ = "
        }
      }
    }

    # build labels
    stopifnot(coef.digits > 0)
    if (coef.digits < 3) {
      warning("'coef.digits < 3' Likely information loss!")
    }

    polys.ls <- polynom::polylist(signif(coefs, coef.digits))

    eq.char <- AIC.char <- rho.char <- character(num.quantiles)
    for (q in seq_along(quantiles)) {
      eq.char[q] <- as.character(signif(polynom::as.polynomial(coefs.ls[[q]]),
                                        coef.digits))
      eq.char[q] <-
        gsub("+ x",
             paste("+ 1.", stringr::str_dup("0", coef.digits - 1L), "*x",
                   sep = ""),
             eq.char[q], fixed = TRUE)
      eq.char[q] <- gsub("e([+-]?[0-9]*)", "%*%10^{\\1}", eq.char[q])
      if (output.type %in% c("latex", "tex", "tikz", "markdown")) {
        eq.char[q] <- gsub("*", " ", eq.char[q], fixed = TRUE)
      }

      eq.char[q] <- gsub("x", eq.x.rhs, eq.char[q], fixed = TRUE)
      if (eq.with.lhs) {
        eq.char[q] <- paste(lhs, eq.char[q], sep = "")
      }

      AIC.char[q] <- sprintf("%.4g", AIC[q])
      rho.char[q] <- sprintf("%.3g", rho[q])
    }

    # build data frames to return
    if (output.type == "expression") {
      z <- tibble::tibble(eq.label = eq.char,
                          AIC.label = paste("AIC", AIC.char, sep = "~`=`~"),
                          rho.label = paste("rho", AIC.char, sep = "~`=`~"),
                          n.label = paste("italic(n)~`=`~", n, sep = ""),
                          grp.label = if (any(grp.label != ""))
                                         paste(grp.label,
                                            sprintf("italic(q)~`=`~%.2f", quantiles),
                                            sep = "*\", \"*")
                                      else
                                        sprintf("italic(q)~`=`~%.2f", quantiles),
                          rq.method = rq.method,
                          quantiles = quantiles,
                          n = n)
    } else if (output.type %in% c("latex", "tex", "text", "tikz")) {
      z <- tibble::tibble(eq.label = eq.char,
                          AIC.label = paste("AIC", AIC.char, sep = " = "),
                          rho.label = paste("rho", AIC.char, sep = " = "),
                          n.label = paste("n = ", n, sep = ""),
                          grp.label = paste(grp.label,
                                            sprintf("q = %.2f", quantiles)),
                          rq.method = rq.method,
                          quantiles = quantiles,
                          n = n)
    } else if (output.type == "markdown") {
      z <- tibble::tibble(eq.label = eq.char,
                          AIC.label = paste("AIC", AIC.char, sep = " = "),
                          rho.label = paste("rho", AIC.char, sep = " = "),
                          n.label = paste("_n_ = ", n, sep = ""),
                          grp.label = paste(grp.label,
                                            sprintf("q = %.2f", quantiles)),
                          rq.method = rq.method,
                          quantiles = quantiles,
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
    label.x <-
      ggpp::compute_npcx(x = label.x, group = group.idx, h.step = hstep,
                         margin.npc = margin.npc, each.len = num.quantiles)
    if (!npc.used) {
      x.expanse <- abs(diff(range(data[["x"]])))
      x.min <- min(data[["x"]])
      label.x <- label.x * x.expanse + x.min
    }
  } else if (is.numeric(label.x) && length(label.x == 1L)) {
    if (!npc.used) {
      x.expanse <- abs(diff(range(data[["x"]])))
      x.min <- min(data[["x"]])
      x <- (label.x - x.min) / x.expanse
    } else {
      x <- label.x
    }
    group <- abs(group.idx)
    expanded.group <- integer()
    for (i in seq_along(group)) {
      temp <- seq(from = 1, by = 1, length.out = num.quantiles) +
        (group[i] - 1) * num.quantiles
      expanded.group <- c(expanded.group, temp)
    }
    if (any(expanded.group > 0L) && hstep != 0) {
      x <- x + (expanded.group - 1) * hstep * ifelse(x < 0.5, 1, -1)
    }
    x <- ifelse(x > 1, 1, x)
    x <- ifelse(x < 0, 0, x)
    if (!npc.used) {
      label.x <- x * x.expanse + x.min
    } else {
      label.x <- x
    }
  }
  if (is.character(label.y)) {
    if (npc.used) {
      margin.npc <- 0.05
    } else {
      # margin set by scale
      margin.npc <- 0
    }
    label.y <-
      ggpp::compute_npcy(y = label.y, group = group.idx, v.step = vstep,
                         margin.npc = margin.npc, each.len = num.quantiles)
    if (!npc.used) {
      y.expanse <- abs(diff(range(data[["y"]])))
      y.min <- min(data[["y"]])
      label.y <- label.y * y.expanse + y.min
    }
  } else if (is.numeric(label.y) && length(label.y == 1L)) {
    if (!npc.used) {
      y.expanse <- abs(diff(range(data[["y"]])))
      y.min <- min(data[["y"]])
      y <- (label.y - y.min) / y.expanse
    } else {
      y <- label.y
    }
    group <- abs(group.idx)
    expanded.group <- integer()
    for (i in seq_along(group)) {
      temp <- seq(from = 1, by = 1, length.out = num.quantiles) +
        (group[i] - 1) * num.quantiles
      expanded.group <- c(expanded.group, temp)
    }
    if (any(expanded.group > 0L) && vstep != 0) {
      y <- y + (expanded.group - 1) * vstep * ifelse(y < 0.5, 1, -1)
    }
    y <- ifelse(y > 1, 1, y)
    y <- ifelse(y < 0, 0, y)
    if (!npc.used) {
      label.y <- y * y.expanse + y.min
    } else {
      label.y <- y
    }
  }

  if (npc.used) {
    z[["npcx"]] <- label.x
    z[["x"]] <- NA_real_
    z[["npcy"]] <- label.y
    z[["y"]] <- NA_real_
  } else {
    z[["x"]] <- label.x
    z[["npcx"]] <- NA_real_
    z[["y"]] <- label.y
    z[["npcy"]] <- NA_real_
  }

  z
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatQuantEq <-
  ggplot2::ggproto("StatQuantEq", ggplot2::Stat,
                   extra_params = c("na.rm", "parse"),
                   compute_group = quant_eq_compute_group_fun,
                   default_aes =
                     ggplot2::aes(npcx = after_stat(npcx),
                                  npcy = after_stat(npcy),
                                  label = after_stat(eq.label),
                                  hjust = "inward",
                                  vjust = "inward",
                                  weight = 1,
                                  quantiles = after_stat(quantiles)),
                   required_aes = c("x", "y")
  )

