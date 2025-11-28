#' Equation, rho, AIC and BIC from quantile regression
#'
#' \code{stat_quant_eq} fits a polynomial model by quantile regression and
#' generates several labels including the equation, rho, 'AIC' and 'BIC'.
#'
#' This statistic interprets the argument passed to \code{formula} differently
#' than \code{\link[ggplot2]{stat_quantile}} accepting \code{y} as well as
#' \code{x} as explanatory variable, matching \code{stat_quant_line()}.
#'
#' When two variables are subject to mutual constrains, it is useful to consider
#' both of them as explanatory and interpret the relationship based on them. So,
#' from version 0.4.1 'ggpmisc' makes it possible to easily implement the
#' approach described by Cardoso (2019) under the name of "Double quantile
#' regression".
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}}. Only needs to be
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
#' @param method function or character If character, "rq" or the name of a model
#'   fit function are accepted, possibly followed by the fit function's
#'   \code{method} argument separated by a colon (e.g. \code{"rq:br"}). If a
#'   function different to \code{rq()}, it must accept arguments named
#'   \code{formula}, \code{data}, \code{weights}, \code{tau} and \code{method}
#'   and return a model fit object of class \code{rq} or \code{rqs}.
#' @param method.args named list with additional arguments passed to \code{rq()}
#'   or to a function passed as argument to \code{method}.
#' @param n.min integer Minimum number of observations needed for fitting a
#'   the model.
#' @param fit.seed RNG seed argument passed to \code{\link[base:Random]{set.seed}()}.
#'   Defaults to \code{NA}, which means that \code{set.seed()} will not be
#'   called.
#' @param eq.with.lhs If \code{character} the string is pasted to the front of
#'   the equation label before parsing or a \code{logical} (see note).
#' @param eq.x.rhs \code{character} this string will be used as replacement for
#'   \code{"x"} in the model equation when generating the label before parsing
#'   it.
#' @param coef.digits,rho.digits integer Number of significant digits to use for
#'   the fitted coefficients and rho in labels.
#' @param coef.keep.zeros logical Keep or drop trailing zeros when formatting
#'   the fitted coefficients and F-value.
#' @param decreasing logical It specifies the order of the terms in the
#'   returned character string; in increasing (default) or decreasing powers.
#' @param label.x,label.y \code{numeric} with range 0..1 "normalized parent
#'   coordinates" (npc units) or character if using \code{geom_text_npc()} or
#'   \code{geom_label_npc()}. If using \code{geom_text()} or \code{geom_label()}
#'   numeric in native data units. If too short they will be recycled.
#' @param hstep,vstep numeric in npc units, the horizontal and vertical step
#'   used between labels for different groups.
#' @param output.type character One of \code{"expression"}, \code{"LaTeX"},
#'   \code{"text"}, \code{"markdown"} or \code{"numeric"}. In most cases,
#'   instead of using this statistics to obtain numeric values, it is better to
#'   use \code{stat_fit_tidy()}.
#' @param orientation character Either \code{"x"} or \code{"y"} controlling the
#'   default for \code{formula}.
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
#'   R option \code{OutDec} is obeyed based on its value at the time the plot
#'   is rendered, i.e., displayed or printed. Set \code{options(OutDec = ",")}
#'   for languages like Spanish or French.
#'
#' @details This stat can be used to automatically annotate a plot with rho or
#'   the fitted model equation. The model fitting is done using package
#'  'quantreg', please, consult its documentation for the
#'   details. It supports only linear models fitted with function \code{rq()},
#'   passing \code{method = "br"} to it, should work well with up to several
#'   thousand observations. The rho, AIC, BIC and n annotations can be used with
#'   any linear model formula. The fitted equation label is correctly generated
#'   for polynomials or quasi-polynomials through the origin. Model formulas can
#'   use \code{poly()} or be defined algebraically with terms of powers of
#'   increasing magnitude with no missing intermediate terms, except possibly
#'   for the intercept indicated by \code{"- 1"} or \code{"-1"} or \code{"+ 0"}
#'   in the formula. The validity of the \code{formula} is not checked in the
#'   current implementation. The default aesthetics sets rho as label for the
#'   annotation.  This stat generates labels as R expressions by default but
#'   LaTeX (use TikZ device), markdown (use package 'ggtext') and plain text are
#'   also supported, as well as numeric values for user-generated text labels.
#'   The value of \code{parse} is set automatically based on \code{output-type},
#'   but if you assemble labels that need parsing from \code{numeric} output,
#'   the default needs to be overridden. This stat only generates annotation
#'   labels, the predicted values/line need to be added to the plot as a
#'   separate layer using \code{\link{stat_quant_line}},
#'   \code{\link{stat_quant_band}} or \code{\link[ggplot2]{stat_quantile}}, so
#'   to make sure that the same model formula is used in all steps it is best to
#'   save the formula as an object and supply this object as argument to the
#'   different statistics.
#'
#'   A ggplot statistic receives as data a data frame that is not the one passed
#'   as argument by the user, but instead a data frame with the variables mapped
#'   to aesthetics. \code{stat_quant_eq()} mimics how \code{stat_smooth()}
#'   works, except that only polynomials can be fitted. In other words, it
#'   respects the grammar of graphics. This helps ensure that the model is
#'   fitted to the same data as plotted in other layers.
#'
#'   Function \code{\link[quantreg]{rq}} does not support singular fits, in
#'   contrast to \code{lm}.
#'
#'   The minimum number of observations with distinct values in the explanatory
#'   variable can be set through parameter \code{n.min}. The default \code{n.min
#'   = 3L} is the smallest usable value. However, model fits with very few
#'   observations are of little interest and using larger values of \code{n.min}
#'   than the default is usually wise.
#'
#' @section User-defined methods: User-defined functions can be passed as
#'   argument to \code{method}. The requirements are 1) that the signature is
#'   similar to that of functions from package 'quantreg' and 2) that the value
#'   returned by the function is an object belonging to class \code{"rq"}, class
#'   \code{"rqs"}, or an atomic \code{NA} value.
#'
#'   The \code{formula} and \code{tau} used to build the equation and quantile
#'   labels aer extracted from the returned \code{"rq"} or \code{"rqs"} object
#'   and can safely differ from the argument passed to parameter \code{formula}
#'   in the call to \code{stat_poly_eq()}. Thus, user-defined methods can
#'   implement both model selection or conditional skipping of labelling.
#'
#' @references Written as an answer to question 65695409 by Mark Neal at
#'   Stackoverflow.
#'
#' @section Warning!: For the formatted equations to be valid, the fitted model
#'   must be a polynomial, with or without intercept. If defined using
#'   \code{poly()} the argument \code{raw = TRUE} must be passed. If defined
#'   manually as powers of \code{x}, \strong{the terms must be in order of
#'   increasing powers, with no missing intermediate power term.} Please, see
#'   examples below. A check on the model is used to validate that it is a
#'   polynomial, in most cases a warning is issued. Failing to comply with this
#'   requirement results in the return of \code{NA} as the formatted equation.
#'
#' @section Aesthetics: \code{stat_quant_eq()} understands \code{x} and \code{y},
#'   to be referenced in the \code{formula} and \code{weight} passed as argument
#'   to parameter \code{weights} of \code{rq()}. All three must be mapped to
#'   \code{numeric} variables. In addition, the aesthetics understood by the
#'   geom used (\code{"text"} by default) are understood and grouping respected.
#'
#'   \emph{If the model formula includes a transformation of \code{x}, a
#'   matching argument should be passed to parameter \code{eq.x.rhs}
#'   as its default value \code{"x"} will not reflect the applied
#'   transformation. In plots, transformation should never be applied to the
#'   left hand side of the model formula, but instead in the mapping of the
#'   variable within \code{aes}, as otherwise plotted observations and fitted
#'   curve will not match. In this case it may be necessary to also pass
#'   a matching argument to parameter \code{eq.with.lhs}.}
#'
#' @return A data frame, with one row per quantile and columns as described
#'   under \strong{Computed variables}. In cases when the number of observations
#'   is less than \code{n.min} a data frame with no rows or columns is returned
#'   rendered as an empty/invisible plot layer.
#'
#' @section Computed variables:
#' If output.type different from \code{"numeric"} the returned tibble contains
#' columns below in addition to a modified version of the original \code{group}:
#' \describe{
#'   \item{x,npcx}{x position}
#'   \item{y,npcy}{y position}
#'   \item{eq.label}{equation for the fitted polynomial as a character string to be parsed}
#'   \item{r.label, and one of cor.label, rho.label, or tau.label}{\eqn{rho} of the fitted model as a character string to be parsed}
#'   \item{AIC.label}{AIC for the fitted model.}
#'   \item{n.label}{Number of observations used in the fit.}
#'   \item{method.label}{Set according \code{method} used.}
#'   \item{rq.method}{character, method used.}
#'   \item{rho, n}{numeric values extracted or computed from fit object.}
#'   \item{hjust, vjust}{Set to "inward" to override the default of the "text" geom.}
#'   \item{quantile}{Numeric value of the quantile used for the fit}
#'   \item{quantile.f}{Factor with a level for each quantile}
#'   }
#'
#' If output.type is \code{"numeric"} the returned tibble contains columns
#'  in addition to a modified version of the original \code{group}:
#' \describe{
#'   \item{x,npcx}{x position}
#'   \item{y,npcy}{y position}
#'   \item{coef.ls}{list containing the "coefficients" matrix from the summary of the fit object}
#'   \item{rho, AIC, n}{numeric values extracted or computed from fit object}
#'   \item{rq.method}{character, method used.}
#'   \item{hjust, vjust}{Set to "inward" to override the default of the "text" geom.}
#'   \item{quantile}{Indicating the quantile  used for the fit}
#'   \item{quantile.f}{Factor with a level for each quantile}
#'   \item{b_0.constant}{TRUE is polynomial is forced through the origin}
#'   \item{b_i}{One or columns with the coefficient estimates}}
#'
#' To explore the computed values returned for a given input we suggest the use
#' of \code{\link[gginnards]{geom_debug}} as shown in the example below.
#'
#' @seealso The quantile fit is done with function \code{\link[quantreg]{rq}},
#'   please consult its documentation. This \code{stat_quant_eq} statistic can
#'   return ready formatted labels depending on the argument passed to
#'   \code{output.type}. This is possible because only polynomial models are
#'   supported. For other types of models, statistics
#'   \code{\link{stat_fit_glance}},  \code{\link{stat_fit_tidy}} and
#'   \code{\link{stat_fit_glance}} should be used instead and the code for
#'   construction of character strings from numeric values and their mapping to
#'   aesthetic \code{label} needs to be explicitly supplied in the call.
#'
#' @note Support for the \code{angle} aesthetic is not automatic and requires
#'   that the user passes as argument suitable numeric values to override the
#'   defaults for label positions.
#'
#' @family ggplot statistics for quantile regression
#'
#' @import quantreg
#'
#' @examples
#' # generate artificial data
#' set.seed(4321)
#' x <- 1:100
#' y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
#' y <- y / max(y)
#' my.data <- data.frame(x = x, y = y,
#'                       group = c("A", "B"),
#'                       y2 = y * c(1, 2) + max(y) * c(0, 0.1),
#'                       w = sqrt(x))
#'
#' # using defaults
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_line() +
#'   stat_quant_eq()
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_line() +
#'   stat_quant_eq(mapping = use_label("eq"))
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_line() +
#'   stat_quant_eq(mapping = use_label("eq"), decreasing = TRUE)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_line() +
#'   stat_quant_eq(mapping = use_label("eq", "method"))
#'
#' # same formula as default
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_line(formula = y ~ x) +
#'   stat_quant_eq(formula = y ~ x)
#'
#' # explicit formula "x explained by y"
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_line(formula = x ~ y) +
#'   stat_quant_eq(formula = x ~ y)
#'
#' # using color
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_line(mapping = aes(color = after_stat(quantile.f))) +
#'   stat_quant_eq(mapping = aes(color = after_stat(quantile.f))) +
#'   labs(color = "Quantiles")
#'
#' # location and colour
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_line(mapping = aes(color = after_stat(quantile.f))) +
#'   stat_quant_eq(mapping = aes(color = after_stat(quantile.f)),
#'                 label.y = "bottom", label.x = "right") +
#'   labs(color = "Quantiles")
#'
#' # give a name to a formula
#' formula <- y ~ poly(x, 3, raw = TRUE)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_line(formula = formula, linewidth = 0.5) +
#'   stat_quant_eq(formula = formula)
#'
#' # angle
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_line(formula = formula, linewidth = 0.5) +
#'   stat_quant_eq(formula = formula, angle = 90, hstep = 0.04, vstep = 0,
#'                 label.y = 0.02, hjust = 0) +
#'   expand_limits(x = -15) # make space for equations
#'
#' # user set quantiles
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_line(formula = formula, quantiles = 0.5) +
#'   stat_quant_eq(formula = formula, quantiles = 0.5)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_band(formula = formula,
#'                   quantiles = c(0.1, 0.5, 0.9)) +
#'   stat_quant_eq(formula = formula, parse = TRUE,
#'                 quantiles = c(0.1, 0.5, 0.9))
#'
#' # grouping
#' ggplot(my.data, aes(x, y2, color = group)) +
#'   geom_point() +
#'   stat_quant_line(formula = formula, linewidth = 0.5) +
#'   stat_quant_eq(formula = formula)
#'
#' ggplot(my.data, aes(x, y2, color = group)) +
#'   geom_point() +
#'   stat_quant_band(formula = formula, linewidth = 0.75) +
#'   stat_quant_eq(formula = formula) +
#'   theme_bw()
#'
#' # labelling equations
#' ggplot(my.data, aes(x, y2,  shape = group, linetype = group,
#'        grp.label = group)) +
#'   geom_point() +
#'   stat_quant_band(formula = formula, color = "black", linewidth = 0.75) +
#'   stat_quant_eq(mapping = use_label("grp", "eq", sep = "*\": \"*"),
#'                 formula = formula) +
#'   expand_limits(y = 3) +
#'   theme_classic()
#'
#' # modifying the explanatory variable within the model formula
#' # modifying the response variable within aes()
#' formula.trans <- y ~ I(x^2)
#' ggplot(my.data, aes(x, y + 1)) +
#'   geom_point() +
#'   stat_quant_line(formula = formula.trans) +
#'   stat_quant_eq(mapping = use_label("eq"),
#'                formula = formula.trans,
#'                eq.x.rhs = "~x^2",
#'                eq.with.lhs = "y + 1~~`=`~~")
#'
#' # using weights
#' ggplot(my.data, aes(x, y, weight = w)) +
#'   geom_point() +
#'   stat_quant_line(formula = formula, linewidth = 0.5) +
#'   stat_quant_eq(formula = formula)
#'
#' # no weights, quantile set to upper boundary
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_line(formula = formula, quantiles = 0.95) +
#'   stat_quant_eq(formula = formula, quantiles = 0.95)
#'
#' # manually assemble and map a specific label using paste() and aes()
#' ggplot(my.data, aes(x, y2, color = group, grp.label = group)) +
#'   geom_point() +
#'   stat_quant_line(method = "rq", formula = formula,
#'                   quantiles = c(0.05, 0.5, 0.95),
#'                   linewidth = 0.5) +
#'   stat_quant_eq(mapping = aes(label = paste(after_stat(grp.label), "*\": \"*",
#'                                             after_stat(eq.label), sep = "")),
#'                 quantiles = c(0.05, 0.5, 0.95),
#'                 formula = formula, size = 3)
#'
#' # manually assemble and map a specific label using sprintf() and aes()
#' ggplot(my.data, aes(x, y2, color = group, grp.label = group)) +
#'   geom_point() +
#'   stat_quant_band(method = "rq", formula = formula,
#'                   quantiles = c(0.05, 0.5, 0.95)) +
#'   stat_quant_eq(mapping = aes(label = sprintf("%s*\": \"*%s",
#'                                               after_stat(grp.label),
#'                                               after_stat(eq.label))),
#'                 quantiles = c(0.05, 0.5, 0.95),
#'                 formula = formula, size = 3)
#'
#' # geom = "text"
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_line(formula = formula, quantiles = 0.5) +
#'   stat_quant_eq(label.x = "left", label.y = "top",
#'                 formula = formula,
#'                 quantiles = 0.5)
#'
#' # Inspecting the returned data using geom_debug()
#' # This provides a quick way of finding out the names of the variables that
#' # are available for mapping to aesthetics using after_stat().
#'
#' gginnards.installed <- requireNamespace("gginnards", quietly = TRUE)
#'
#' if (gginnards.installed)
#'   library(gginnards)
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_quant_eq(formula = formula, geom = "debug")
#'
#' \dontrun{
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_quant_eq(mapping = aes(label = after_stat(eq.label)),
#'                   formula = formula, geom = "debug",
#'                   output.type = "markdown")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_quant_eq(formula = formula, geom = "debug", output.type = "text")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_quant_eq(formula = formula, geom = "debug", output.type = "numeric")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_quant_eq(formula = formula, quantiles = c(0.25, 0.5, 0.75),
#'                   geom = "debug", output.type = "text")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_quant_eq(formula = formula, quantiles = c(0.25, 0.5, 0.75),
#'                   geom = "debug", output.type = "numeric")
#' }
#'
#' @export
#'
stat_quant_eq <- function(mapping = NULL,
                          data = NULL,
                          geom = "text_npc",
                          position = "identity",
                          ...,
                          formula = NULL,
                          quantiles = c(0.25, 0.5, 0.75),
                          method = "rq:br",
                          method.args = list(),
                          n.min = 3L,
                          fit.seed = NA,
                          eq.with.lhs = TRUE,
                          eq.x.rhs = NULL,
                          coef.digits = 3,
                          coef.keep.zeros = TRUE,
                          decreasing = getOption("ggpmisc.decreasing.poly.eq", FALSE),
                          rho.digits = 4,
                          label.x = "left", label.y = "top",
                          hstep = 0,
                          vstep = NULL,
                          output.type = NULL,
                          na.rm = FALSE,
                          orientation = NA,
                          parse = NULL,
                          show.legend = FALSE,
                          inherit.aes = TRUE) {

  stopifnot("Args 'formula' and/or 'data' in 'method.args'" =
              !any(c("formula", "data") %in% names(method.args)))

  # we make a character string name for the method
  if (is.character(method)) {
    method <- trimws(method, which = "both")
    method.name <- method
  } else if (is.function(method)) {
    method.name <- deparse(substitute(method))
    if (grepl("^function[ ]*[(]", method.name[1])) {
      method.name <- "function"
    }
  } else {
    method.name <- "missing"
  }

  if (grepl("^lm$|^lm[:]|^rlm$|^rlm[:]|^gls$|^gls[:]", method.name)) {
    stop("Methods 'lm', 'rlm' and 'gls' not supported, please use 'stat_poly_eq()'.")
  } else if (grepl("^lmodel2$|^lmodel2[:]", method.name)) {
    stop("Method 'lmodel2' not supported, please use 'stat_ma_eq()'.")
  }

  if (method.name == "rqss") {
    default.formula <- y ~ qss(x)
  } else {
    default.formula <- y ~ x
  }
  temp <- guess_orientation(orientation = orientation,
                            formula = formula,
                            default.formula = default.formula,
                            formula.on.x = FALSE)
  orientation <- temp[["orientation"]]
  formula <-  temp[["formula"]]

  if (is.null(output.type)) {
    if (geom %in% c("richtext", "textbox", "marquee")) {
      output.type <- "markdown"
    } else {
      output.type <- "expression"
    }
  }
  if (is.null(parse)) {
    parse <- output.type == "expression"
  }

  # is the model formula that of complete and increasing polynomial?
  mk.eq.label <- output.type != "numeric" && check_poly_formula(formula, orientation)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatQuantEq,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params =
      rlang::list2(formula = formula,
                   quantiles = quantiles,
                   method = method,
                   method.name = method.name,
                   method.args = method.args,
                   n.min = n.min,
                   fit.seed = fit.seed,
                   eq.with.lhs = eq.with.lhs,
                   eq.x.rhs = eq.x.rhs,
                   mk.eq.label = mk.eq.label,
                   coef.digits = coef.digits,
                   coef.keep.zeros = coef.keep.zeros,
                   decreasing = decreasing,
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
                                       formula = y ~ x,
                                       quantiles = c(0.25, 0.5, 0.75),
                                       method,
                                       method.name,
                                       method.args = list(),
                                       n.min = 3L,
                                       fit.seed = NA,
                                       weight = 1,
                                       eq.with.lhs = TRUE,
                                       eq.x.rhs = NULL,
                                       mk.eq.label = TRUE,
                                       coef.digits = 3,
                                       coef.keep.zeros = TRUE,
                                       decreasing = FALSE,
                                       rho.digits = 4,
                                       label.x = "left",
                                       label.y = "top",
                                       hstep = 0,
                                       vstep = 0.10,
                                       npc.used = TRUE,
                                       output.type = "expression",
                                       na.rm = FALSE,
                                       orientation = "x") {
  force(data)
  force(method)

  # parse obeys this option, but as for some labels or output types we do not
  # use parse() to avoid dropping of trailing zeros, we need to manage this in
  # our code in this case.
  decimal.mark <- getOption("OutDec", default = ".")
  if (!decimal.mark %in% c(".", ",")) {
    warning("Decimal mark must be one of '.' or ',', not: '", decimal.mark, "'")
    decimal.mark <- "."
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
      warning("Non-unique value in 'data$grp.label' using group index ", data[["group"]][1], " as label.")
      grp.label <- as.character(data[["group"]][1])
    } else {
      grp.label <- data[["grp.label"]][1]
    }
  } else {
    # if nothing mapped to grp.label we use ""
    grp.label <- ""
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

  # make sure quantiles are ordered
  quantiles <- sort(quantiles)

  fm.ls <- quant_helper_fun(data = data,
                            formula = formula,
                            quantiles = quantiles,
                            fit.by.quantile = FALSE,
                            method = method,
                            method.name = method.name,
                            method.args = method.args,
                            n.min = n.min,
                            fit.seed = fit.seed,
                            weight = weight,
                            na.rm = na.rm,
                            orientation = orientation)
  fm <- fm.ls[["fm1"]]
  fun.args <- fm.ls[["fun.args1"]]

  # allow model formula and tau selection by method functions
  if (!length(fm) || (is.atomic(fm) && is.na(fm))) {
#    warning("Model fit failure!")
    return(data.frame())
  } else if (inherits(fm, "rq") || inherits(fm, "rqs")) {
    # allow model formula selection by the model fit method
    # extract formula from fitted model if possible, but fall back on argument if needed
    formula.ls <- fail_safe_formula(fm, fun.args, verbose = TRUE)
    quantiles <- fm[["tau"]]
  } else {
    stop("Fitted model object does not inherit from class \"rq\" or \"rqs\" as expected")
  }

  # quantreg contains code with partial matching of names!
  # so we silence selectively only these warnings
  withCallingHandlers({
    fm.summary <- summary(fm)
  }, warning = function(w) {
    if (startsWith(conditionMessage(w), "partial match of") ||
        startsWith(conditionMessage(w), "partial argument match of")) {
      invokeRestart("muffleWarning")
    }
  })

  fm.class <- class(fm)

  # class of returned summary value depends on length of quantiles vector
  if (!inherits(fm.summary, "summary.rqs")) {
    fm.summary <- list(fm.summary)
  }
  names(fm.summary) <- as.character(quantiles)

  AIC <- AIC(fm)
  n <- length(fm.summary[[1]][["residuals"]])
  rho <- fm[["rho"]]
  rq.method <- fm[["method"]]
  coefs.mt <- fm[["coefficients"]] # a matrix if length(quantiles) > 1
  # ensure that coefs.mt is consistent
  if (is.vector(coefs.mt)) {
    coefs.mt <- as.matrix(coefs.mt)
    colnames(coefs.mt) <- paste("tau=", fm[["tau"]], sep = "")
  }

  formula <- formula.ls[[1]]
  stopifnot(inherits(formula, what = "formula"))

  formula.rhs.chr <- as.character(formula)[3]
  forced.origin <- grepl("-[[:space:]]*1|+[[:space:]]*0", formula.rhs.chr)
  if (forced.origin) {
    coefs.mt <- rbind(rep(0, ncol(coefs.mt)), coefs.mt)
  }
  coefs.ls <- asplit(coefs.mt, 2)
  # located here so that names in coef.ls remain the same as in version 0.4.0
  rownames(coefs.mt) <- paste("b", (1:nrow(coefs.mt)) - 1, sep = "_")

  # factor with nicely formatted labels
  num.quantiles <- length(quantiles)
  quant.digits <- ifelse(min(quantiles) < 0.01 || max(quantiles) > 0.99, 3, 2)
  quant.levels <- sort(unique(quantiles), decreasing = TRUE)
  quant.labels <- sprintf_dm("%#.*f", quant.digits, quant.levels,
                             decimal.mark = decimal.mark)
  quantiles.f <- factor(quantiles,
                        levels = quant.levels,
                        labels = quant.labels)

  z <- tibble::tibble()
  if (output.type == "numeric") {
    z <- tibble::tibble(coef.ls = coefs.ls,
                        quantile = quantiles,
                        quantile.f = quantiles.f,
                        rq.method = rq.method,
                        AIC = AIC,
                        rho = rho,
                        n = n,
                        eq.label = "", # needed for default 'label' mapping
                        b_0.constant = forced.origin)
    z <- cbind(z, tibble::as_tibble(t(coefs.mt)))
  } else {
    if (mk.eq.label) {
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
    }
    # build labels
    stopifnot(coef.digits > 0)
    if (coef.digits < 3) {
      warning("'coef.digits < 3' Likely information loss!")
    }

    qtl.char <- n.char <- eq.char <- AIC.char <- rho.char <- character(num.quantiles)
    for (q in seq_along(quantiles)) {
      if (mk.eq.label) {
        # build equation as a character string from the coefficient estimates
        eq.char[q] <- coefs2poly_eq(coefs = coefs.ls[[q]],
                                    coef.digits = coef.digits,
                                    coef.keep.zeros = coef.keep.zeros,
                                    decreasing = decreasing,
                                    eq.x.rhs = eq.x.rhs,
                                    lhs = lhs,
                                    output.type = output.type,
                                    decimal.mark = decimal.mark)
      } else {
        eq.char[q] <- NA_character_
      }
      # build other label that vary with quantiles
      AIC.char[q] <- plain_label(value = AIC[q],
                                 value.name = "AIC",
                                 digits = 4,
                                 fixed = FALSE,
                                 output.type = output.type,
                                 decimal.mark = decimal.mark)
      rho.char[q] <- r_label(value = rho[q],
                             method = "spearman",
                             digits = rho.digits,
                             fixed = FALSE,
                             output.type = output.type,
                             decimal.mark = decimal.mark)
      n.char[q] <- italic_label(value = n,
                                value.name = "n",
                                digits = 0,
                                fixed = TRUE,
                                output.type = output.type,
                                decimal.mark = decimal.mark)
      qtl.char[q] <- italic_label(value = quantiles[q],
                                  value.name = "q",
                                  digits = 2,
                                  fixed = TRUE,
                                  output.type = output.type,
                                  decimal.mark = decimal.mark)
    }

    # build data frames to return
    z <- data.frame(eq.label = eq.char,
                    AIC.label = AIC.char,
                    rho.label = rho.char,
                    n.label = n.char,
                    grp.label = grp.label,
                    qtl.label = qtl.char,
                    method.label = paste("\"method: ", method.name, "\"", sep = ""),
                    rho = rho,
                    rq.method = rq.method,
                    quantile = quantiles,
                    quantile.f = quantiles.f,
                    n = n)
  }

  # add members common to numeric and other output types
  z[["fm.method"]] <- method.name
  z[["fm.class"]] <- fm.class[1]
  z[["fm.formula"]] <- formula.ls
  z[["fm.formula.chr"]] <- format(formula.ls)

  # Compute label positions
  # we need to use scale limits as observations are not necessarily plotted
  x.range <- scales$x$range$range
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
      x.range <- scales$x$range$range
      label.x <- label.x * diff(x.range) + x.range[1]
    }
  } else if (is.numeric(label.x) && length(label.x == 1L)) {
    if (!npc.used) {
      x <- (label.x - x.range[1]) / diff(x.range)
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
      label.x <- x * diff(x.range) + x.range[1]
    } else {
      label.x <- x
    }
  }

  # we need to use scale limits as observations are not necessarily plotted
  y.range <- scales$y$range$range
  if (is.character(label.y)) {
    rev.y.pos <- length(label.y) == 1L && label.y != "bottom"
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
      label.y <- label.y * diff(y.range) + y.range[1]
    }
  } else if (is.numeric(label.y) && length(label.y == 1L)) {
    rev.y.pos <- length(label.y) == 1L && label.y >= 0.5
    if (!npc.used) {
      y.range <- scales$y$range$range
      y <- (label.y - y.range[1]) / diff(y.range)
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
      label.y <- y * diff(y.range) + y.range[1]
    } else {
      label.y <- y
    }
  }

  if (npc.used) {
    z[["npcx"]] <- label.x
    z[["x"]] <- NA_real_
    z[["npcy"]] <- if (rev.y.pos) rev(label.y) else label.y
    z[["y"]] <- NA_real_
  } else {
    z[["x"]] <- label.x
    z[["npcx"]] <- NA_real_
    z[["y"]] <- if (rev.y.pos) rev(label.y) else label.y
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
                                  vjust = "inward"),
                   dropped_aes = "weight",
                   required_aes = c("x", "y"),
                   optional_aes = "grp.label"
  )

