#' Equation, p-value, R^2, AIC or BIC of fitted polynomial
#'
#' \code{stat_poly_eq} fits a polynomial and generates several labels including
#' the equation and/or p-value, coefficient of determination (R^2), 'AIC' or
#' 'BIC'.
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
#' @param eq.with.lhs If \code{character} the string is pasted to the front of
#'   the equation label before parsing or a \code{logical} (see note).
#' @param eq.x.rhs \code{character} this string will be used as replacement for
#'   \code{"x"} in the model equation when generating the label before parsing
#'   it.
#' @param coef.digits,rr.digits integer Number of significant digits to use in
#'   for the vector of fitted coefficients and for $R^2$ labels.
#' @param label.x,label.y \code{numeric} with range 0..1 "normalized parent
#'   coordinates" (npc units) or character if using \code{geom_text_npc()} or
#'   \code{geom_label_npc()}. If using \code{geom_text()} or \code{geom_label()}
#'   numeric in native data units. If too short they will be recycled.
#' @param label.x.npc,label.y.npc \code{numeric} with range 0..1 (npc units)
#'   DEPRECATED, use label.x and label.y instead; together with a geom
#'   using npcx and npcy aesthetics.
#' @param hstep,vstep numeric in npc units, the horizontal and vertical step
#'   used between labels for different groups.
#' @param output.type character One of "expression", "LaTeX" or "text",
#'   or "numeric".
#'
#' @note For backward compatibility a logical is accepted as argument for
#'   \code{eq.with.lhs}, giving the same output than the current default
#'   character value. By default "x" is retained as independent variable as this
#'   is the name of the aesthetic. However, it can be substituted by providing a
#'   suitable replacement character string through \code{eq.x.rhs}.
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
#'   label for the annotation. This stat only generates labels, the predicted
#'   values need to be separately added to the plot, so to make sure that the
#'   same model formula is used in all steps it is best to save the formula as
#'   an object and supply this object as argument to the different statistics.
#'
#'   A ggplot statistic receives as data a data frame that is not the one passed
#'   as argument by the user, but instead a data frame with the variables mapped
#'   to aesthetics. stat_poly_eq() mimics how stat_smooth() works, except that
#'   only polynomials can be fitted. In other words, it respects the grammar of
#'   graphics. This helps ensure that the model is fitted to the same data as
#'   plotted in other layers.
#'
#' @references Written as an answer to a question at Stackoverflow.
#'   \url{https://stackoverflow.com/questions/7549694/adding-regression-line-equation-and-r2-on-graph}
#'
#'
#' @section Aesthetics: \code{stat_poly_eq} understands \code{x} and \code{y},
#'   to be referenced in the \code{formula} and \code{weight} passed as argument
#'   to parameter \code{weights} of \code{lm()}. All three must be mapped to
#'   \code{numeric} variables. In addition, the aesthetics undertood by the geom
#'   used (\code{"text"} by default) are understood and grouping respected.
#'
#' @section Computed variables:
#' If output.type different from \code{"numeric"} the returned tibble contains columns:
#' \describe{
#'   \item{x,npcx}{x position}
#'   \item{y,npcy}{y position}
#'   \item{coef.ls, r.squared, adj.r.squared, AIC, BIC}{as numric values extracted from fit object}
#'   \item{eq.label}{equation for the fitted polynomial as a character string to be parsed}
#'   \item{rr.label}{\eqn{R^2} of the fitted model as a character string to be parsed}
#'   \item{adj.rr.label}{Adjusted \eqn{R^2} of the fitted model as a character string to be parsed}
#'   \item{AIC.label}{AIC for the fitted model.}
#'   \item{BIC.label}{BIC for the fitted model.}
#'   \item{hjust, vjust}{Set to "inward" to override the default of the "text" geom.}}
#'
#' If output.type is \code{"numeric"} the returned tibble contains columns:
#' \describe{
#'   \item{x,npcx}{x position}
#'   \item{y,npcy}{y position}
#'   \item{coef.ls}{list containing the "coefficients" matrix from the summary of the fit object}
#'   \item{r.squared, adj.r.squared, AIC, BIC}{numric values extracted from fit object}
#'   \item{hjust, vjust}{Set to "inward" to override the default of the "text" geom.}}
#'
#' To explore the computed values returned for a given input we sugegst the use
#' of \code{\link[gginnards]{geom_debug}} as shown in the example below.
#'
#' @section Parsing may be required: if using the computed labels with
#'   \code{output.type = "expression"}, then \code{parse = TRUE} is needed,
#'   while if using \code{output.type = "LaTeX"} \code{parse = FALSE} is needed.
#'
#' @seealso This \code{stat_poly_eq} statistic can return ready formatted labels
#'   depending on the argument passed to \code{output.type}. This is possible
#'   because only polynomial models are supported. For other types of models,
#'   statistics \code{\link{stat_fit_glance}},  \code{\link{stat_fit_tidy}} and
#'   \code{\link{stat_fit_glance}} should be used instead and the mapping of
#'   aesthetic \code{label} explicitly supplied in the call.
#'
#' @family statistics for linear model fits
#'
#' @examples
#' library(gginnards)
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
#' # as above but using geom_debug()
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula,
#'                geom = "debug")
#'
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
#'   stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "~~~~")),
#'                formula = formula, parse = TRUE)
#'
#' # user specified label and digits
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "~~~~")),
#'                formula = formula, rr.digits = 3, coef.digits = 2, parse = TRUE)
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
#'                mapping = aes(label = sprintf(my.format,
#'                                              stat(coef.ls)[[1]][[1, "Estimate"]],
#'                                              stat(coef.ls)[[1]][[2, "Estimate"]],
#'                                              stat(coef.ls)[[1]][[3, "Estimate"]],
#'                                              stat(coef.ls)[[1]][[4, "Estimate"]])
#'                                              )
#'                              )
#'
#' @export
#'
stat_poly_eq <- function(mapping = NULL, data = NULL,
                         geom = "text_npc",
                         position = "identity",
                         ...,
                         formula = NULL,
                         eq.with.lhs = "italic(y)~`=`~",
                         eq.x.rhs = NULL,
                         coef.digits = 3,
                         rr.digits = 2,
                         label.x = "left", label.y = "top",
                         label.x.npc = NULL, label.y.npc = NULL,
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
    params = list(formula = formula,
                  eq.with.lhs = eq.with.lhs,
                  eq.x.rhs = eq.x.rhs,
                  coef.digits = coef.digits,
                  rr.digits = rr.digits,
                  label.x = label.x,
                  label.y = label.y,
                  hstep = hstep,
                  vstep = ifelse(is.null(vstep),
                                 ifelse(grepl("label", geom),
                                        0.125,
                                        0.075),
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
                                      formula = NULL,
                                      weight = 1,
                                      eq.with.lhs = "italic(y)~`=`~",
                                      eq.x.rhs = NULL,
                                      coef.digits = 3,
                                      rr.digits = 2,
                                      label.x = "left",
                                      label.y = "top",
                                      hstep = 0,
                                      vstep = 0.075,
                                      npc.used = TRUE,
                                      output.type = "expression",
                                      na.rm = FALSE) {
  force(data)
  if (length(unique(data$x)) < 2) {
    # Not enough data to perform fit
    return(tibble::new_tibble())
  }

  if (is.null(data$weight)) data$weight <- 1

  output.type = tolower(output.type)
  if (is.null(eq.x.rhs)) {
    if (output.type == "expression") {
      eq.x.rhs <- "~italic(x)"
    } else {
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

  lm.args <- list(quote(formula), data = quote(data), weights = quote(weight))
  mf <- do.call(stats::lm, lm.args)

  rr <- summary(mf)$r.squared
  adj.rr <- summary(mf)$adj.r.squared
  AIC <- AIC(mf)
  BIC <- BIC(mf)

  if (output.type == "numeric") {
    z <- tibble::tibble(coef.ls = list(summary(mf)[["coefficients"]]),
                        r.squared = rr,
                        adj.r.squared = adj.rr,
                        AIC = AIC,
                        BIC = BIC)
  } else {
    coefs <- stats::coef(mf)
    formula.rhs.chr <- as.character(formula)[3]
    if (grepl("-1", formula.rhs.chr) || grepl("- 1", formula.rhs.chr)) {
      coefs <- c(0, coefs)
    }

    eq.char <- as.character(signif(polynom::as.polynomial(coefs), coef.digits))
    # as character drops 1
    eq.char <- gsub("+ x", paste("+ 1.", stringr::str_dup("0", coef.digits - 1L),
                                 "*x", sep = ""),
                    eq.char, fixed = TRUE)
    eq.char <- gsub("e([+-]?[0-9]*)", "%*%10^{\\1}", eq.char)
    if (output.type %in% c("latex", "tex", "tikz")) {
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
      }
    }
    if (eq.with.lhs) {
      eq.char <- paste(lhs, eq.char, sep = "")
    }
    rr.char <- format(rr, digits = rr.digits)
    adj.rr.char <- format(adj.rr, digits = rr.digits)
    AIC.char <- sprintf("%.4g", AIC)
    BIC.char <- sprintf("%.4g", BIC)
    if (output.type == "expression") {
      z <- tibble::tibble(eq.label = gsub("x", eq.x.rhs, eq.char, fixed = TRUE),
                                        rr.label = paste("italic(R)^2", rr.char, sep = "~`=`~"),
                                        adj.rr.label = paste("italic(R)[adj]^2",
                                                             adj.rr.char, sep = "~`=`~"),
                                        AIC.label = paste("AIC", AIC.char, sep = "~`=`~"),
                                        BIC.label = paste("BIC", BIC.char, sep = "~`=`~"))
    } else if (output.type %in% c("latex", "tex", "text")) {
      z <-tibble::tibble(eq.label = gsub("x", eq.x.rhs, eq.char, fixed = TRUE),
                                        rr.label = paste("R^2", rr.char, sep = " = "),
                                        adj.rr.label = paste("R_{adj}^2",
                                                             adj.rr.char, sep = " = "),
                                        AIC.label = paste("AIC", AIC.char, sep = " = "),
                                        BIC.label = paste("BIC", BIC.char, sep = " = "))
    } else if (!output.type %in% c("numeric")) {
      warning("Unknown 'output.type' argument: ", output.type)
    }
  }

  if (npc.used) {
    margin.npc <- 0.05
  } else {
    # margin set by scale
    margin.npc <- 0
  }
  if (is.character(label.x)) {
    label.x <- compute_npcx(x = label.x, group = group.idx, h.step = hstep, margin.npc = margin.npc)
    if (!npc.used) {
      x.expanse <- abs(diff(range(data$x)))
      x.min <- min(data$x)
      label.x <- label.x * x.expanse + x.min
    }
  }
  if (is.character(label.y)) {
    label.y <- compute_npcy(y = label.y, group = group.idx, v.step = vstep, margin.npc = margin.npc)
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
