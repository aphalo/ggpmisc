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
#' @param method character "MA", "SMA" , "RMA" and "OLS".
#' @param nperm integer Number of permutation used to estimate significance.
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
#'   P_value, n and/or the fitted model equation. It supports linear major axis
#'   (MA), standard major axis (SMA) and ranged major axis (RMA) regression by
#'   means of function \code{\link[lmodel2]{lmodel2}}. Please see the
#'   documentation, including the vignette of package 'lmodel2' for details.
#'   The parameters in \code{stat_ma_eq()} follow the same naming as in function
#'   \code{lmodel2()}.
#'
#'   A ggplot statistic receives as \code{data} a data frame that is not the one
#'   passed as argument by the user, but instead a data frame with the variables
#'   mapped to aesthetics. \code{stat_ma_eq()} mimics how \code{stat_smooth()}
#'   works, except that only linear regression can be fitted. Similarly to these
#'   statistics the model fits respect grouping, so the scales used for \code{x}
#'   and \code{y} should both be continuous scales rather than discrete.
#'
#' @section Aesthetics: \code{stat_ma_eq} understands \code{x} and \code{y}, to
#'   be referenced in the \code{formula} while the \code{weight} aesthetic is
#'   ignored. Both \code{x} and \code{y} must be mapped to \code{numeric}
#'   variables. In addition, the aesthetics understood by the geom
#'   (\code{"text"} is the default) are understood and grouping respected.
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
#'   \item{p.value.label}{P-value for the F-value above.}
#'   \item{n.label}{Number of observations used in the fit.}
#'   \item{grp.label}{Set according to mapping in \code{aes}.}
#'   \item{r.squared, p.value, n}{numeric values, from the model fit object}}
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
#'   \item{b_i}{One or two columns with the coefficient estimates}}
#'
#' To explore the computed values returned for a given input we suggest the use
#' of \code{\link[gginnards]{geom_debug}} as shown in the last examples below.
#'
#' @seealso This \code{stat_ma_eq} statistic can return ready formatted labels
#'   depending on the argument passed to \code{output.type}. If other than
#'   linear major axis regression is desired, then \code{\link{stat_poly_eq}} or
#'   \code{\link{stat_quant_eq}} should be used instead of \code{stat_ma_eq}.
#'   For other types of models such as non-linear models, statistics
#'   \code{\link{stat_fit_glance}} and \code{\link{stat_fit_tidy}} should be
#'   used and the code for construction of character strings from numeric values
#'   and their mapping to aesthetic \code{label} needs to be explicitly supplied
#'   in the call.
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
#' # using major axis regression
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(method = "MA") +
#'   stat_ma_eq(method = "MA")
#'
#' # using standard major axis regression
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(method = "SMA") +
#'   stat_ma_eq(method = "SMA")
#'
#' # using ranged major axis regression
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(method = "RMA", range.y = "interval", range.x = "interval") +
#'   stat_ma_eq(method = "RMA", range.y = "interval", range.x = "interval")
#'
#' # using defaults
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line() +
#'   stat_ma_eq()
#'
#' # same formula as default
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(formula = y ~ x) +
#'   stat_ma_eq(formula = y ~ x)
#'
#' # explicit formula "x explained by y"
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line(formula = x ~ y) +
#'   stat_ma_eq(formula = x ~ y)
#'
#' # angle
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line() +
#'   stat_ma_eq(angle = 90, hstep = 0.05, vstep = 0,
#'              label.y = 0.98, hjust = 1)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line() +
#'   stat_ma_eq(angle = 90,
#'              hstep = 0.05, vstep = 0, hjust = 0,
#'              label.y = 0.5)
#'
#' # grouping
#' ggplot(my.data, aes(x, y, color = group)) +
#'   geom_point() +
#'   stat_ma_line() +
#'   stat_ma_eq()
#'
#' ggplot(my.data, aes(x, y, color = group)) +
#'   geom_point() +
#'   stat_ma_line() +
#'   stat_ma_eq(angle = 90,
#'              hstep = 0.05, vstep = 0, hjust = 0,
#'              size = 5, label.y = 2/3)
#'
#' # labelling equations
#' ggplot(my.data, aes(x, y,  shape = group, linetype = group,
#'        grp.label = group)) +
#'   geom_point() +
#'   stat_ma_line(color = "black") +
#'   stat_ma_eq(aes(label = paste(after_stat(grp.label),
#'                                after_stat(eq.label),
#'                                sep = "*\": \"*"))) +
#'   theme_classic()
#'
#' # Location of equations
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line() +
#'   stat_ma_eq(label.y = "bottom", label.x = "right")
#'
#' ggplot(my.data, aes(x, y, color = group)) +
#'   geom_point() +
#'   stat_ma_line() +
#'   stat_ma_eq(label.y = 0.03, label.x = 0.95, vstep = 0.1)
#'
#' # geom = "text"
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_ma_line() +
#'   stat_ma_eq(label.x = "left", label.y = "top")
#'
#' # Inspecting the returned data using geom_debug()
#' if (requireNamespace("gginnards", quietly = TRUE)) {
#'   library(gginnards)
#'
#' # This provides a quick way of finding out the names of the variables that
#' # are available for mapping to aesthetics.
#'
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_ma_eq(geom = "debug")
#'
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_ma_eq(aes(label = after_stat(eq.label)),
#'                geom = "debug",
#'                output.type = "markdown")
#'
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_ma_eq(geom = "debug", output.type = "text")
#'
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_ma_eq(geom = "debug", output.type = "numeric")
#'
#' }
#'
#'
#' @export
#'
stat_ma_eq <- function(mapping = NULL, data = NULL,
                       geom = "text_npc",
                       position = "identity",
                       ...,
                       method = "MA",
                       formula = NULL,
                       range.y = NULL,
                       range.x = NULL,
                       nperm = 99,
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
                       hstep = 0,
                       vstep = NULL,
                       output.type = NULL,
                       na.rm = FALSE,
                       orientation = NA,
                       parse = NULL,
                       show.legend = FALSE,
                       inherit.aes = TRUE) {
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
    stat = StatMaEq,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(method = method,
                  formula = formula,
                  range.y = range.y,
                  range.x = range.x,
                  nperm = nperm,
                  eq.with.lhs = eq.with.lhs,
                  eq.x.rhs = eq.x.rhs,
                  small.r = small.r,
                  small.p = small.p,
                  coef.digits = coef.digits,
                  coef.keep.zeros = coef.keep.zeros,
                  rr.digits = rr.digits,
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
                                    range.y,
                                    range.x,
                                    nperm,
                                    weight,
                                    eq.with.lhs,
                                    eq.x.rhs,
                                    small.r,
                                    small.p,
                                    coef.digits,
                                    coef.keep.zeros,
                                    rr.digits,
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

  n <- mf[["n"]]
  coefs <- stats::coefficients(mf, method = method)
  rr <- mf[["rsquare"]]
  idx <- which(mf[["regression.results"]][["Method"]] == method)
  p.value <- mf[["regression.results"]]["P-perm (1-tailed)", idx]

  formula.rhs.chr <- as.character(formula)[3]
  forced.origin <- grepl("-[[:space:]]*1|+[[:space:]]*0", formula.rhs.chr)
  if (forced.origin) {
      coefs <- c(0, coefs)
  }
  names(coefs) <- paste("b", (1:length(coefs)) - 1, sep = "_")

  if (output.type == "numeric") {
    z <- tibble::tibble(r.squared = rr,
                        p.value = p.value,
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
    stopifnot(p.digits > 0)
    if (p.digits < 2) {
      warning("'p.digits < 2' Likely information loss!")
    }

    if (output.type == "expression") {
      rr.char <- sprintf("\"%#.*f\"", rr.digits, rr)
      p.value.char <- sprintf("\"%#.*g\"", p.digits, p.value)
    } else {
      rr.char <- sprintf("%#.*f", rr.digits, rr)
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
                          p.value.label =
                            ifelse(is.na(p.value), character(0L),
                                   paste(ifelse(small.p, "p",  "P"),
                                         ifelse(p.value < 10^(-p.digits), as.character(10^(-p.digits)), p.value.char),
                                         sep = ifelse(p.value < 10^(-p.digits), " < ", " = "))),
                          n.label = paste("n = ", n, sep = ""),
                          grp.label = grp.label,
                          r.squared = rr,
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
                          p.value.label =
                            ifelse(is.na(p.value), character(0L),
                                   paste(ifelse(small.p, "_p_", "_P_"),
                                         ifelse(p.value < 10^(-p.digits), as.character(10^(-p.digits)), p.value.char),
                                         sep = ifelse(p.value < 10^(-p.digits), " < ", " = "))),
                          n.label = paste("_n_ = ", n, sep = ""),
                          grp.label = grp.label,
                          r.squared = rr,
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
    label.x <- ggpp::compute_npcx(x = label.x, group = group.idx, h.step = hstep,
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
    label.y <- ggpp::compute_npcy(y = label.y, group = group.idx, v.step = vstep,
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
StatMaEq <-
  ggplot2::ggproto("StatMaEq", ggplot2::Stat,
                   extra_params = c("na.rm", "parse"),
                   compute_group = ma_eq_compute_group_fun,
                   default_aes =
                     ggplot2::aes(npcx = after_stat(npcx),
                                  npcy = after_stat(npcy),
                                  label = after_stat(rr.label),
                                  hjust = "inward", vjust = "inward",
                                  weight = 1),
                   required_aes = c("x", "y")
  )

