#' Quantile regression predictions and annotations
#'
#' Statistics \code{stat_quant_line()}, \code{stat_quant_band()} and
#' \code{stat_quant_eq()} fit models by quantile regression. While
#' \code{stat_quant_line()} and \code{stat_quant_band()} add prediction lines and
#' bands, \code{stat_quant_eq()} adds textual labels to a plot.
#'
#' @details While \code{stat_poly_line()} and \code{stat_poly_eq()} fit
#'   a single model per plot layer, \code{stat_quant_line()}, \code{stat_quant_band()}
#'   and \code{stat_quant_eq()} can fit multiple models sharing the same
#'   \code{method} and \code{formula} but differing in their
#'   probability. These probabilities are passed a vector argument to parameter
#'   \code{quantiles}.
#'
#'   \code{stat_quant_line} fits one or more quantile regressions and obtains
#'   predictions similarly to \code{\link[ggplot2]{stat_quantile}()} from
#'   'ggplot2', but in addition it computes confidence regions for the
#'   prediction lines. By default each quantile is plotted as a line, with a
#'   confidence band when \code{se = TRUE}.
#'
#'   \code{stat_quant_band()} fits quantile regressions and obtains predictions
#'   identically to \code{stat_quant_line()}. \code{stat_quant_band()} fits 2 or
#'   3 quantiles in the same plot layer and displays the area between the
#'   predicted regression lines for the extreme quantiles as a band.
#'
#'   \code{stat_quant_eq()} fits quantile regressions and generates a set of
#'   labels for each regression line fitted. By default the labels are formatted
#'    as R's \code{\link[grDevices]{plotmath}} expressions, \eqn{\LaTeX} and
#'    markdown are also supported.
#'
#'   \code{stat_quant_eq()}, \code{stat_quant_line()} and
#'   \code{stat_quant_band()} support both \code{"rq"} and \code{"rqss"} as
#'   \code{method}. In the case of \code{"rqss"} the model formula makes
#'   normally use of \code{qss()} to formulate the spline and its constraints.
#'   User defined functions are supported as \code{method} as long as they
#'   accept arguments named \code{formula}, \code{data}, \code{weights},
#'   \code{tau} and \code{method} and return a model fit object of class
#'   \code{rq}, \code{rqs} or \code{rqss}. Such user-defined functions can
#'   implement model selection and/or method selection, or conditionally skip
#'   model fitting on a per data group basis.
#'
#'   The minimum number of observations with distinct values in the explanatory
#'   variable can be set through parameter \code{n.min}. The default \code{n.min
#'   = 10L} is a bare minimum for quantile regression. Model fits with such a
#'   small number of observations are of little interest and using larger values
#'   of \code{n.min} than the default is wise.
#'
#'   There are interesting uses for \emph{double quantile regression}, i.e., a
#'   pair of quantile regressions on \code{x} and \code{y} on the same data. For
#'   example, when two variables are subject to mutual constrains, it is useful
#'   to consider both of them as explanatory and interpret the relationship
#'   based on them considered as limiting. 'ggpmisc' (>= 0.4.1) supports
#'   \code{orientation} making it easy implement the approach described by
#'   Cardoso (2019) under the name of "Double quantile regression".
#'
#' @inheritParams stat_poly_eq
#' @param quantiles numeric vector Values in 0..1 indicating the quantiles.
#' @param method function or character If character, "rq", "rqss" or the name of
#'   a model fit function are accepted, possibly followed by the fit function's
#'   \code{method} argument separated by a colon (e.g. \code{"rq:br"}). If a
#'   function different to \code{rq()}, it must accept arguments named
#'   \code{formula}, \code{data}, \code{weights}, \code{tau} and \code{method}
#'   and return a model fit object of class \code{rq}, \code{rqs} or
#'   \code{rqss}.
#' @param method.args named list with additional arguments passed to
#'   \code{rq()}, \code{rqss()} or to another function passed as argument to
#'   \code{method}.
#' @param se logical Passed to \code{quantreg::predict.rq()}.
#' @param level numeric in range [0..1] Passed to \code{quantreg::predict.rq()}.
#' @param type character Passed to \code{quantreg::predict.rq()}.
#' @param interval character Passed to \code{quantreg::predict.rq()}.
#'
#' @param coef.digits,rho.digits integer Number of significant digits to use for
#'   the fitted coefficients and rho in labels.
#'
#' @aesthetics StatQuantLine
#' @aesthetics StatQuantBand
#' @aesthetics StatQuantEq
#'
#' @inheritSection check_output_type Output types
#'
#' @inheritSection stat_poly_eq Model equation label
#'
#' @inheritSection stat_poly_eq Position of labels
#'
#' @inheritSection stat_poly_eq Model formula and model fitting
#'
#' @inheritSection stat_poly_eq Model fit methods supported
#'
#' @return \code{stat_quant_eq()} returns a data frame, with one row per
#'   quantile and columns as described below, while \code{stat_quant_line()}
#'   and \code{stat_quant_band()} return a data frame, with \code{n} rows per
#'   quantile and columns as described below. If the number of observations
#'   is less than \code{n.min} or if the model fit method returns \code{NA} or
#'   \code{NULL}, a data frame with no rows or columns is returned, resulting
#'   in an empty/invisible plot layer.
#'
#' @section Variables returned by \code{stat_quant_eq()}:
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
#' To explore the computed values returned for a given input we suggest the use
#' of \code{\link[gginnards]{geom_debug}} as shown in the example below.
#'
#' @section Variables returned by \code{stat_quant_line()}:
#'
#'   \describe{
#'   \item{y \strong{or} x}{predicted value}
#'   \item{ymin \strong{or} xmin}{lower confidence limit around the fitted line}
#'   \item{ymax \strong{or} xmax}{upper confidence limit around the fitted line}
#'   }
#'
#'   If \code{fm.values = TRUE} is passed then one column with the number of
#'   observations \code{n} used for each fit is also included, with the same
#'   value in each row within a group. This is wasteful and disabled by default,
#'   but provides a simple and robust approach to achieve effects like colouring
#'   or hiding of the model fit line based on the number of observations.
#'
#' @section Variables returned by \code{stat_quant_band()}:
#'
#'   \describe{
#'   \item{y \strong{or} x}{Regression prediction for the middle quantile, if three quantiles are passed as argument}
#'   \item{ymin \strong{or} xmin}{Regression prediction for the smallest quantile}
#'   \item{ymax \strong{or} xmax}{Regression prediction for the largest quantile}
#'   }
#'
#'   If \code{fm.values = TRUE} is passed then one column with the number of
#'   observations \code{n} used for each fit is also included, with the same
#'   value in each row within a group. This is wasteful and disabled by default,
#'   but provides a simple and robust approach to achieve effects like colouring
#'   or hiding of the model fit line based on the number of observations.
#'
#' @references
#' Cardoso, G. C. (2019) Double quantile regression accurately assesses
#'   distance to boundary trade-off. Methods in ecology and evolution,
#'   10(8), 1322-1331.
#'
#' @seealso \code{\link[quantreg]{rq}}, \code{\link[quantreg]{rqss}} and
#'   \code{\link[quantreg]{qss}}.
#'
#' @family 'ggpmisc' statistics for model fits
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
#' # Predictions as lines
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_line()
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_line(quantiles = 0.5, se = TRUE)
#'
#' # Predictions as band
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_band()
#'
#' # y as explanatory variable (orientation = y)
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_band(formula = x ~ y)
#'
#' # Using splines
#' library(quantreg)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quant_line(method = "rqss",
#'                   formula = y ~ qss(x, constraint = "D"),
#'                   quantiles = 0.5, se = FALSE)
#'
#' # Adding annotations
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
#'                 label.y = 0.02, hjust = 0, size = 3) +
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
#' # Inspecting the returned data using geom_debug_group()
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
#'     stat_quant_line(geom = "debug_group")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     stat_quant_band(geom = "debug_group")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_quant_eq(formula = formula, geom = "debug_group")
#'
#' \dontrun{
#' if (gginnards.installed)
#'   ggplot(mpg, aes(displ, hwy)) +
#'     stat_quant_line(geom = "debug_group", fm.values = TRUE)
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     stat_quant_band(geom = "debug_group", fm.values = TRUE)
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_quant_eq(mapping = aes(label = after_stat(eq.label)),
#'                   formula = formula, geom = "debug_group",
#'                   output.type = "markdown")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_quant_eq(formula = formula, geom = "debug_group", output.type = "text")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_quant_eq(formula = formula, geom = "debug_group", output.type = "numeric")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_quant_eq(formula = formula, quantiles = c(0.25, 0.5, 0.75),
#'                   geom = "debug_group", output.type = "text")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_quant_eq(formula = formula, quantiles = c(0.25, 0.5, 0.75),
#'                   geom = "debug_group", output.type = "numeric")
#' }
#'
#' @export
#'
stat_quant_eq <- function(mapping = NULL,
                          data = NULL,
                          geom = "text_npc",
                          position = "identity",
                          ...,
                          orientation = NA,
                          formula = NULL,
                          quantiles = c(0.25, 0.5, 0.75),
                          method = "rq:br",
                          method.args = list(),
                          n.min = 10L,
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

  if (grepl("^lm$|^lm[:]|^rlm$|^rlm[:]|^gls$|^gls[:]|^sma$|^ma$", method.name)) {
    stop("Methods 'lm', 'rlm', 'gls', 'ma' and 'sma' not supported, please use 'stat_poly_eq()'.")
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

  output.type <-
    check_output_type(output.type = output.type, geom = geom)

  if (is.null(parse)) {
    parse <- output.type == "expression"
  }

  # is the model formula that of complete and increasing polynomial?
  mk.eq.label <- output.type != "numeric" &&
    check_poly_formula(formula,
                       orientation,
                       check.transf.lhs = !is.character(eq.with.lhs),
                       check.transf.rhs = !is.character(eq.x.rhs))

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

  rlang::check_installed("quantreg", reason = "to use stat_quant_eq()")

  # parse obeys this option, but as for some labels or output types we do not
  # use parse() to avoid dropping of trailing zeros, we need to manage this in
  # our code in this case.
  decimal.mark <- getOption("OutDec", default = ".")
  if (!decimal.mark %in% c(".", ",")) {
    warning("Decimal mark must be one of '.' or ',', not: '", decimal.mark, "'")
    decimal.mark <- "."
  }

  if (is.null(data[["weight"]])) {
    data[["weight"]] <- 1
  }

  if (exists("grp.label", data)) {
    if (length(unique(data[["grp.label"]])) > 1L) {
      warning("Non-unique value in 'data$grp.label' using group index ",
              data[["group"]][1], " as label.")
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
                   extra_params = c("na.rm", "parse", "orientation"),
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

