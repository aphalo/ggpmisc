#' Equation, p-value, \eqn{R^2}, AIC and BIC of fitted polynomial
#'
#' \code{stat_poly_eq} fits a polynomial, by default with \code{stats::lm()},
#' but alternatively using robust, resistant or generalized least squares. Major
#' axis regression and segmented linear regression are also supported. Using the
#' fitted model it generates several labels including the fitted model equation,
#' p-value, F-value, coefficient of determination (R^2) and its confidence
#' interval, 'AIC', 'BIC', number of observations and method name, if available.
#'
#' @inheritParams stat_poly_line
#'
#' @param eq.with.lhs If \code{character} the string is pasted to the front of
#'   the equation label before parsing or a \code{logical} (see note).
#' @param eq.x.rhs \code{character} this string will be used as replacement for
#'   \code{"x"} in the model equation when generating the label before parsing
#'   it.
#' @param small.r,small.p logical Flags to switch use of lower case r and p for
#'   coefficient of determination and p-value.
#' @param rsquared.conf.level numeric Confidence level for the returned
#'   confidence interval. Set to NA to skip CI computation.
#' @param CI.brackets character vector of length 2. The opening and closing
#'   brackets used for the CI label.
#' @param coef.digits,f.digits integer Number of significant digits to use for
#'   the fitted coefficients and F-value.
#' @param coef.keep.zeros logical Keep or drop trailing zeros when formatting
#'   the fitted coefficients and F-value.
#' @param decreasing logical It specifies the order of the terms in the
#'   returned character string; in increasing (default) or decreasing powers.
#' @param rr.digits,p.digits integer Number of digits after the decimal point to
#'   use for \eqn{R^2} and P-value in labels. If \code{Inf}, use exponential
#'   notation with three decimal places.
#' @param label.x,label.y \code{numeric} with range 0..1 "normalized parent
#'   coordinates" (npc units) or character if using \code{geom_text_npc()} or
#'   \code{geom_label_npc()}. If using \code{geom_text()} or \code{geom_label()}
#'   numeric in native data units. If too short they will be recycled.
#' @param hstep,vstep numeric in npc units, the horizontal and vertical step
#'   used between labels for different groups.
#' @param output.type character One of "expression", "LaTeX", "text",
#'   "markdown" or "numeric".
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
#' @details This statistic can be used to automatically annotate a plot with
#'   \eqn{R^2}, adjusted \eqn{R^2}, the fitted model equation, \eqn{P}, and
#'   other parameters from a fitted model. It supports linear regression and
#'   polynomial fits with \code{\link[stats]{lm}()}, segmented linear regression
#'   with package 'segmented' and major axis and standardized major axis
#'   regression with package 'smatr', robust and resistant regression with
#'   packages 'MASS' and 'robustbase'. The list is not exhaustive, and depends
#'   on the availability of methods for the model fit objects. Lack of methods
#'   or explicit support results in individual parameters and matching labels
#'   being set to NA. As some model fitting results can depend on the RNG,
#'   \code{fit.seed} if different to \code{NA} is used as argument in a call to
#'   \code{\link[base:Random]{set.seed}()} immediately ahead of model fitting.
#'
#'   While strings for \eqn{R^2}, adjusted \eqn{R^2}, \eqn{F}, and \eqn{P}
#'   annotations are returned for all valid linear models, A character string
#'   for the fitted model is returned only for polynomials (see below). When
#'   not generated automatically, the equation can still be assembled by the
#'   user within the call to \code{\link[ggplot2]{aes}()}. In addition, a label
#'   for the confidence interval of \eqn{R^2}, based on values computed with
#'   function \code{\link[confintr]{ci_rsquared}} from package 'confintr' is
#'   returned when possible.
#'
#'   Model formulas can use \code{poly()} or be defined algebraically including
#'   the intercept indicated by \code{+1}, \code{-1}, \code{+0} or implicit. If
#'   defined using \code{poly()} the argument \code{raw = TRUE} must be passed.
#'   The \code{model formula} is checked, and if not recognized as a polynomial
#'   with no missing terms and terms ordered by increasing powers, no equation
#'   label is generated. Thus, as the value returned for \code{eq.label} can be
#'   \code{NA}, the default aesthetic mapping to \emph{label} is \eqn{R^2}.
#'
#'   By default, the character strings are generated as suitable for parsing
#'   into R's plotmath expressions. However, LaTeX (use TikZ device in R),
#'   markdown (use package 'ggtext') and plain text are also supported, as well
#'   as returning numeric values for user-generated text labels. The argument of
#'   \code{parse} is set automatically based on \code{output-type}, but if you
#'   assemble labels that need parsing from \code{numeric} output, the default
#'   needs to be overridden.
#'
#'   This statistic only generates annotation labels, the predicted values/line
#'   need to be added to the plot as a separate layer using
#'   \code{\link{stat_poly_line}} (or \code{\link[ggplot2]{stat_smooth}}).
#'   Passing the same arguments in \code{stat_poly_line()} and in
#'   \code{stat_poly_eq()} to parameters \code{method} and \code{formula}, and
#'   if used also to \code{method.args} ensures that the plotted curve and
#'   equation are consistent. Thus, it is best to save these arguments as named
#'   objects and pass them as arguments to the two statistics.
#'
#'   A ggplot statistic receives as \code{data} a data frame that is not the one
#'   passed as argument by the user, but instead a data frame with the variables
#'   mapped to aesthetics. \code{stat_poly_eq()} mimics how
#'   \code{\link[ggplot2]{stat_smooth}()} works. Thus, the model formula should
#'   be defined based on the names of aesthetics \code{x} and \code{y}, not the
#'   names of the variables in the data. Before fitting the model, data are
#'   split based on groupings created by any other
#'   mappings present in a plot panel: \emph{fitting is done separately for each
#'   group in each plot panel}.
#'
#'   With method \code{"lm"}, singularity results in terms being dropped with a
#'   message if more numerous than can be fitted with a singular (exact) fit. In
#'   this case or if the model results in a perfect fit due to a low number of
#'   observations, estimates for various parameters are \code{NaN} or \code{NA}.
#'   When this is the case the corresponding labels are set to
#'   \code{character(0L)} and thus not visible in the plot. With methods other
#'   than \code{"lm"}, the model fit functions simply fail in case of
#'   singularity, e.g., singular fits are not implemented in
#'   \code{\link[MASS]{rlm}()}.
#'
#'   A requirement for a minimum number of observations with distinct values in
#'   the explanatory variable can be set through parameter \code{n.min}. The
#'   default \code{n.min = 2L} is the smallest suitable for method \code{"lm"}
#'   but too small for method \code{"rlm"} for which \code{n.min = 3L} is
#'   needed. Anyway, model fits with very few observations are of little
#'   interest and using larger values of \code{n.min} than the default is
#'   usually wise. This can be useful as when this threshold is not reached
#'   an empty data frame is returned resulting in an empty plot layer.
#'
#' @section User-defined methods: User-defined functions can be passed as
#'   argument to \code{method}. The requirements are 1) that the signature is
#'   similar to that of function \code{lm()} (with parameters \code{formula},
#'   \code{data}, \code{weights} and any other arguments passed by name through
#'   \code{method.args}) and 2) that the value returned by the function is an
#'   object of a class such as \code{"lm"} for which \code{coefs()} and similar
#'   query methods are available or for empty plot layer output, an atomic
#'   \code{NA} value.
#'
#'   When possible, i.e., nearly allways, the \code{formula} used to build
#'   the equation label is extracted from the returned fitted model object.
#'   Most fitted model objects returned follow the example of \code{lm()} and
#'   include the model formula fitted. Thus, this model formula can safely
#'   differ from the argument passed to parameter \code{formula} in the call
#'   to \code{stat_poly_eq()}.
#'   Thus, user-defined methods can implement any or all of \code{method}
#'   selection, model \code{formula} selection, dynamically adjusted
#'   \code{method.args} and conditional skipping of labelling on a by group
#'   basis.
#'
#' @references Originally written as an answer to question 7549694 at
#'   Stackoverflow but enhanced based on suggestions from users and my own
#'   needs.
#'
#' @section Aesthetics: \code{stat_poly_eq()} understands \code{x} and \code{y},
#'   to be referenced in the \code{formula} and \code{weight} passed as argument
#'   to parameter \code{weights}. All three must be mapped to \code{numeric}
#'   variables. In addition, the aesthetics understood by the geom
#'   (\code{"text"} is the default) are understood and grouping respected.
#'
#'   If the model formula includes a transformation of \code{x}, a
#'   matching argument should be passed to parameter \code{eq.x.rhs}
#'   as its default value \code{"x"} will not reflect the applied
#'   transformation. In plots, transformation should never be applied to the
#'   left hand side of the model formula, but instead in the mapping of the
#'   variable within \code{aes}, as otherwise plotted observations and fitted
#'   curve will not match. In this case it may be necessary to also pass
#'   a matching argument to parameter \code{eq.with.lhs}.
#'
#' @return A data frame, with a single row per group and columns as described
#'   under \strong{Computed variables}. In cases when the number of observations
#'   is less than \code{n.min} a data frame with no rows or columns is returned,
#'   and rendered as an empty/invisible plot layer.
#'
#' @section Computed variables:
#' If the model fit function used does not returns \code{NA} or no value,
#' the label is set to \code{character(0L)}. The position of the columns in
#' the data frame can change between package versions, extract values always
#' by name.
#'
#' For all \code{output.type} arguments the following values are returned.
#' \describe{
#'   \item{x,npcx}{x position}
#'   \item{y,npcy}{y position}
#'   \item{coefs}{fitted coefficients, named numeric vector as a list member}
#'   \item{r.squared, rr.confint.level, rr.confint.low, rr.confint.high, adj.r.squared, f.value, f.df1, f.df2, p.value, AIC, BIC, n, knots, knots.se}{numeric values, from the model fit object}
#'   \item{grp.label}{Set according to mapping in \code{aes}.}
#'   \item{knots}{list containing a numeric vector of knot or "psi" \emph{x}-value for linear splines}
#'   \item{fm.method}{name of method used, character}
#'   \item{fm.class}{most derived class or the fitted model object, character}
#'   \item{fm.formula.chr}{formatted model formula, character}}
#'
#' If \code{output.type} is not \code{"numeric"} the returned tibble contains in
#' addition to those above the columns listed below, each containing a single
#' character string. The markup used depends on the value of \code{output.type}.
#' \describe{
#'   \item{eq.label}{equation for the fitted polynomial as a character string to be parsed or \code{NA}}
#'   \item{rr.label}{\eqn{R^2} of the fitted model as a character string to be parsed}
#'   \item{adj.rr.label}{Adjusted \eqn{R^2} of the fitted model as a character string to be parsed}
#'   \item{rr.confint.label}{Confidence interval for \eqn{R^2} of the fitted model as a character string to be parsed}
#'   \item{f.value.label}{F value and degrees of freedom for the fitted model as a whole.}
#'   \item{p.value.label}{P-value for the F-value above.}
#'   \item{AIC.label}{AIC for the fitted model.}
#'   \item{BIC.label}{BIC for the fitted model.}
#'   \item{n.label}{Number of observations used in the fit.}
#'   \item{knots.label}{The knots or change points in segmented regression.}
#'   \item{grp.label}{Set according to mapping in \code{aes}.}
#'   \item{method.label}{Set according \code{method} used.}}
#'
#' If output.type is \code{"numeric"} the returned tibble contains columns
#' listed below in addition to the base ones. If the model fit function used
#' does not return a value, the variable is set to \code{NA_real_}.
#' \describe{
#'   \item{coef.ls}{list containing the "coefficients" matrix from the summary of the fit object}
#'   \item{b_0.constant}{TRUE is polynomial is forced through the origin}
#'   \item{b_i}{One or more columns with the coefficient estimates}}
#'
#' To explore the computed values returned for a given input we suggest the use
#' of \code{\link[gginnards]{geom_debug}} as shown in the last examples below.
#'
#' @seealso This statistics fits a model with function \code{\link[stats]{lm}()}
#'   as default, several other functions returning objects of class \code{"lm"}
#'   or objects of classes for which the common R fitted-model-object
#'   extraction/query methods are available. Consult the documentation of these
#'   functions for the details and additional arguments that can be passed to
#'   them by name through parameter \code{method.args}. User-defined
#'   model-fitting functions are also supported.
#'
#'   Please, see the articles
#'   \href{https://docs.r4photobiology.info/ggpmisc/}{online documentation}
#'   for additional use examples and guidance.
#'
#'   For quantile regression \code{\link{stat_quant_eq}()} should be used
#'   instead of \code{stat_poly_eq()} while for model II or major axis
#'   regression with package 'lmodel2' \code{\link{stat_ma_eq}()} should be
#'   used. For methods not supportted by these three statistics, such as
#'   non-linear models, statistics \code{\link{stat_fit_glance}()} and
#'   \code{\link{stat_fit_tidy}()} can be used but require the user to create
#'   character strings from numeric values and map them to aesthetic
#'   \code{label}.
#'
#' @family ggplot statistics for linear and polynomial regression
#'
#' @examples
#' # generate artificial data
#' set.seed(4321)
#' x <- 1:100
#' y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
#' y <- y / max(y)
#' my.data <- data.frame(x = x, y = y,
#'                       group = c("A", "B"),
#'                       y2 = y * c(1, 2) + c(0, 0.1),
#'                       w = sqrt(x))
#'
#' # give a name to a formula
#' formula <- y ~ poly(x, 3, raw = TRUE)
#'
#' # using defaults
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_poly_line() +
#'   stat_poly_eq()
#'
#' # no weights
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(formula = formula)
#'
#' # other labels
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(use_label("eq"), formula = formula)
#'
#' # other labels
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(use_label("eq"), formula = formula, decreasing = TRUE)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(use_label("eq", "R2"), formula = formula)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(use_label("R2", "R2.CI", "P", "method"), formula = formula)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(use_label("R2", "F", "P", "n", sep = "*\"; \"*"),
#'                formula = formula)
#'
#' # grouping
#' ggplot(my.data, aes(x, y2, color = group)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(formula = formula)
#'
#' # rotation
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(formula = formula, angle = 90)
#'
#' # label location
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(formula = formula, label.y = "bottom", label.x = "right")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(formula = formula, label.y = 0.1, label.x = 0.9)
#'
#' # modifying the explanatory variable within the model formula
#' # modifying the response variable within aes()
#' # eq.x.rhs and eq.with.lhs defaults must be overridden!!
#' formula.trans <- y ~ I(x^2)
#' ggplot(my.data, aes(x, y + 1)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula.trans) +
#'   stat_poly_eq(use_label("eq"),
#'                formula = formula.trans,
#'                eq.x.rhs = "~x^2",
#'                eq.with.lhs = "y + 1~~`=`~~")
#'
#' # using weights
#' ggplot(my.data, aes(x, y, weight = w)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(formula = formula)
#'
#' # no weights, 4 digits for R square
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(formula = formula, rr.digits = 4)
#'
#' # manually assemble and map a specific label using paste() and aes()
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(aes(label =  paste(after_stat(rr.label),
#'                                   after_stat(n.label), sep = "*\", \"*")),
#'                formula = formula)
#'
#' # manually assemble and map a specific label using sprintf() and aes()
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(aes(label =  sprintf("%s*\" with \"*%s*\" and \"*%s",
#'                                     after_stat(rr.label),
#'                                     after_stat(f.value.label),
#'                                     after_stat(p.value.label))),
#'                formula = formula)
#'
#' # x on y regression
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula, orientation = "y") +
#'   stat_poly_eq(use_label("eq", "adj.R2"),
#'                formula = x ~ poly(y, 3, raw = TRUE))
#'
#' # conditional user specified label
#' ggplot(my.data, aes(x, y2, color = group)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
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
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(geom = "text", label.x = 100, label.y = 0, hjust = 1,
#'                formula = formula)
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
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_poly_line(formula = formula) +
#'     stat_poly_eq(formula = formula,
#'                  geom = "debug")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_poly_line(formula = formula) +
#'     stat_poly_eq(formula = formula,
#'                  geom = "debug",
#'                  output.type = "numeric")
#'
#' # names of the variables
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_poly_line(formula = formula) +
#'     stat_poly_eq(formula = formula,
#'                  geom = "debug",
#'                  dbgfun.data = colnames)
#'
#' # only data$eq.label
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_poly_line(formula = formula) +
#'     stat_poly_eq(formula = formula,
#'                  geom = "debug",
#'                  output.type = "expression",
#'                  dbgfun.data = function(x) {x[["eq.label"]]})
#'
#' # only data$eq.label
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_poly_line(formula = formula) +
#'     stat_poly_eq(formula = formula,
#'                  geom = "debug",
#'                  output.type = "text",
#'                  dbgfun.data = function(x) {x[["eq.label"]]})
#'
#' @export
#'
stat_poly_eq <- function(mapping = NULL,
                         data = NULL,
                         geom = "text_npc",
                         position = "identity",
                         ...,
                         formula = NULL,
                         method = "lm",
                         method.args = list(),
                         n.min = 2L,
                         fit.seed = NA,
                         eq.with.lhs = TRUE,
                         eq.x.rhs = NULL,
                         small.r = getOption("ggpmisc.small.r", default = FALSE),
                         small.p = getOption("ggpmisc.small.p", default = FALSE),
                         CI.brackets = c("[", "]"),
                         rsquared.conf.level = 0.95,
                         coef.digits = 3,
                         coef.keep.zeros = TRUE,
                         decreasing = getOption("ggpmisc.decreasing.poly.eq", FALSE),
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

  if (method.name == "auto") {
    message("Method 'auto' is equivalent to 'lm', splines are not supported.")
    method <- method.name <- "lm"
  } else if (grepl("^rq$|^rq[:]|^rqss$|^rqss[:]", method.name)) {
    stop("Methods 'rq' and 'rqss' not supported, please use 'stat_quant_eq()'.")
  } else if (grepl("^lmodel2$|^lmodel2[:]", method.name)) {
    stop("Method 'lmodel2' not supported, please use 'stat_ma_eq()'.")
  }

  temp <- guess_orientation(orientation = orientation,
                            formula = formula)
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

  # is the model formula that of an increasing polynomial?
  mk.eq.label <- output.type != "numeric" &&
                   check_poly_formula(formula, orientation) && # is 'formula' a polynomial?
                   !any(grepl("lspline", as.character(formula))) # not a linear spline

  if (is.null(rsquared.conf.level) || !is.finite(rsquared.conf.level)) {
    rsquared.conf.level <- 0
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatPolyEq,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params =
      rlang::list2(formula = formula,
                   method = method,
                   method.name = method.name,
                   method.args = method.args,
                   n.min = n.min,
                   fit.seed = fit.seed,
                   eq.with.lhs = eq.with.lhs,
                   eq.x.rhs = eq.x.rhs,
                   mk.eq.label = mk.eq.label,
                   small.r = small.r,
                   small.p = small.p,
                   CI.brackets = CI.brackets,
                   rsquared.conf.level = rsquared.conf.level,
                   coef.digits = coef.digits,
                   coef.keep.zeros = coef.keep.zeros,
                   decreasing = decreasing,
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
                                      method.name,
                                      method.args = list(),
                                      formula = y ~ x,
                                      n.min =2L,
                                      fit.seed = NA,
                                      weight = 1,
                                      eq.with.lhs,
                                      eq.x.rhs = TRUE,
                                      mk.eq.label = TRUE,
                                      small.r = FALSE,
                                      small.p  = FALSE,
                                      CI.brackets = c("[", "]"),
                                      rsquared.conf.level  = 0.95,
                                      coef.digits = 3L,
                                      coef.keep.zeros = TRUE,
                                      decreasing = FALSE,
                                      rr.digits = 2,
                                      f.digits = 3,
                                      p.digits = 3,
                                      label.x = "left",
                                      label.y = "top",
                                      hstep = 0,
                                      vstep = 0.1,
                                      npc.used = TRUE,
                                      output.type = "expression",
                                      na.rm = FALSE,
                                      orientation = "x") {
  force(data)

  # parse obeys this option, but as for some labels or output types we do not
  # use parse() to avoid dropping of trailing zeros, we need to manage this in
  # our code in this case.
  decimal.mark <- getOption("OutDec", default = ".")
  if (!decimal.mark %in% c(".", ",")) {
    warning("Decimal mark must be one of '.' or ',', not: '", decimal.mark, "'")
    decimal.mark <- "."
  }

  if (length(unique(data[[orientation]])) < n.min) {
    return(data.frame())
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
    method <- switch(method,
                     lm = "lm:qr",
                     rlm = "rlm:M",
                     lqs = "lqs:lqs",
                     gls = "gls:REML",
                     method)
    method.name <- method
    method <- strsplit(x = method, split = ":", fixed = TRUE)[[1]]
    if (length(method) > 1L) {
      fun.method <- method[2]
      method <- method[1]
    } else {
      fun.method <- character()
    }
    method <- switch(method,
                     lm = stats::lm,
                     rlm = MASS::rlm,
                     lqs = MASS::lqs,
                     gls = nlme::gls,
                     match.fun(method))
  } else if (is.function(method)) {
    fun.method <- character()
  }

  if (exists("weight", data) && !all(data[["weight"]] == 1)) {
    stopifnot("A mapping to 'weight' and a named argument 'weights' cannot co-exist" =
                !"weights" %in% method.args)
    fun.args <- list(formula = quote(formula),
                     data = quote(data),
                     weights = data[["weight"]])
  } else {
    fun.args <- list(formula = quote(formula),
                     data = quote(data))
  }
  fun.args <- c(fun.args, method.args)
  if (length(fun.method)) {
    fun.args[["method"]] <- fun.method
  }

  # gls() parameter for formula is called model
  if (grepl("gls", method.name)) {
    names(fun.args)[1] <- "model"
  }

  if (!is.na(fit.seed)) {
    set.seed(fit.seed)
  }
  fm <- do.call(method, args = fun.args)
  mk.eq.label <- mk.eq.label && class(fm)[1] != "segmented" # not segmented into a spline?

  # allow skipping of output if returned value from model fit function is missing
  if (!length(fm) || (is.atomic(fm) && is.na(fm))) {
    return(data.frame())
  } else if (!(inherits(fm, "lm") || inherits(fm, "lmrob") ||
               inherits(fm, "gls") || inherits(fm, "lqs") ||
               inherits(fm, "lts") || inherits(fm, "sma"))) {
    warning("Method \"", method.name,
            "\" did not return a ",
            "\"lm\", \"lmrob\", \"lqs\", \"lts\", \"gls\" or \"sma\" ",
            "object, possible failure ahead.")
  }

  fm.class <- class(fm)
  if (fm.class[1] == "sma") {
    # summary.sma prints results and returns NULL
    fm.summary <- NULL
  } else {
    fm.summary <- summary(fm)
  }

  # allow model formula selection by the model fit method
  # extract formula from fitted model if possible, but fall back on argument if needed
  formula.ls <- fail_safe_formula(fm, fun.args, verbose = TRUE)

  if ("fstatistic" %in% names(fm.summary)) {
    f.value <- fm.summary[["fstatistic"]]["value"]
    f.df1 <- fm.summary[["fstatistic"]]["numdf"]
    f.df2 <- fm.summary[["fstatistic"]]["dendf"]
    p.value <- stats::pf(q = f.value, f.df1, f.df2, lower.tail = FALSE)
  } else {
    if (fm.class[1] == "sma") {
      p.value <- unlist(fm[["pval"]])
      f.value <- f.df1 <- f.df2 <- NA_real_
    } else {
      f.value <- f.df1 <- f.df2 <- p.value <- NA_real_
    }
   }
  if ("r.squared" %in% names(fm.summary)) {
    rr <- fm.summary[["r.squared"]]
    if (!all(is.finite(c(f.value, f.df1, f.df2))) ||
        rsquared.conf.level <= 0
        ) {
      rr.confint.low <- rr.confint.high <- NA_real_
    } else {
      # error handler needs to be added as ci_rsquared() will call stop on
      # non-convergence
      # or alternatively implement a non-stop version of ci_rsquared()
      rr.confint <-
        try(confintr::ci_rsquared(x = f.value,
                              df1 = f.df1,
                              df2 = f.df2,
                              probs = ((1 - rsquared.conf.level) / 2) *
                                c(1, -1) + c(0, 1) ),
            silent = TRUE)
      if (inherits(rr.confint, what = "try-error")) {
        warning("CI computation error: ", attr(rr.confint, "condition"))
        rr.confint.low <- rr.confint.high <- NA_real_
      } else {
        rr.confint.low  <- rr.confint[["interval"]][1]
        rr.confint.high <- rr.confint[["interval"]][2]
      }
    }
  } else {
    if (fm.class[1] == "sma") {
      rr <- unlist(fm[["r2"]])
      rr.confint.low <- rr.confint.high <- NA_real_
    } else {
      rr <- rr.confint.low <- rr.confint.high <- NA_real_
    }
  }
  if ("adj.r.squared" %in% names(fm.summary)) {
    adj.rr <- fm.summary[["adj.r.squared"]]
  } else {
    adj.rr <- NA_real_
  }
  AIC <- try(AIC(fm), silent = TRUE)
  if (inherits(AIC, "try-error")) {
    AIC <- NA_real_
  }
  BIC <- try(BIC(fm), silent = TRUE)
  if (inherits(BIC, "try-error")) {
    BIC <- NA_real_
  }
  n <- try(length(stats::residuals(fm)), silent = TRUE)
  if (inherits(n, "try-error")) {
    n <- NA_real_
  }
  coefs <- stats::coefficients(fm)
  stopifnot(is.numeric(coefs))
  coefs.names <- names(coefs)
  if ("psi" %in% names(fm.summary)) { # package segmented
    knots.mat <- fm.summary[["psi"]]
    knots <- knots.mat[ , 2]
    knots.names <- names(knots)
    knots.se <- knots.mat[ , 3]
  } else {
    knots <- NA_real_
    knots.se <- NA_real_
    knots.names <- NA_character_
  }

  formula <- formula.ls[[1L]]
  stopifnot(inherits(formula, what = "formula"))

  formula.rhs.chr <- as.character(formula)[3]
  forced.origin <- grepl("-[[:space:]]*1|+[[:space:]]*0", formula.rhs.chr)
  if (forced.origin) {
    coefs <- c(0, coefs)
  }
  selector <- !is.na(coefs)
  coefs <- coefs[selector]
  if (!all(selector)) {
    message("Terms dropped from model (singularity); n = ", nrow(data), " in group.")
  }
  # z is the object to be returned, i.e., passed to the geometry function
  # it must be a data.frame, and here we use one row per group
  # to pass embedded object like in a list, we embed single member lists
  if (output.type == "numeric") {
    z <- tibble::tibble(
      rr.label = "",  # needed for default 'label' mapping
      coef.ls = list(fm.summary[["coefficients"]]), # a matrix
      b_0.constant = forced.origin
    )
    names(coefs) <- paste("b", (which(selector)) - 1, sep = "_")
    z <- cbind(z, tibble::as_tibble_row(coefs))
  } else {
    if (mk.eq.label) {
      # assemble the fitted polynomial equation as a character string
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
    } else {
      # model formula is not a polynomial or no eq requested
      eq.char <- NA_character_
    }

    # assemble the tibble to return
    z <- tibble::tibble(
      eq.label = eq.char,
      rr.label = rr_label(value = rr,
                          small.r = small.r,
                          digits = rr.digits,
                          output.type = output.type,
                          decimal.mark = decimal.mark),
      adj.rr.label = adj_rr_label(value = adj.rr,
                                  small.r = small.r,
                                  digits = rr.digits,
                                  output.type = output.type,
                                  decimal.mark = decimal.mark),
      rr.confint.label = rr_ci_label(value = c(rr.confint.low, rr.confint.high),
                                     conf.level = rsquared.conf.level,
                                     range.brackets = CI.brackets,
                                     range.sep = NULL,
                                     digits = rr.digits,
                                     output.type = output.type,
                                     decimal.mark = decimal.mark),
      AIC.label = plain_label(value = AIC,
                              value.name = "AIC",
                              digits = 4,
                              output.type = output.type,
                              decimal.mark = decimal.mark),
      BIC.label = plain_label(value = BIC,
                              value.name = "BIC",
                              digits = 4,
                              output.type = output.type,
                              decimal.mark = decimal.mark),
      f.value.label = f_value_label(value = f.value,
                                    df1 = f.df1,
                                    df2 = f.df2,
                                    digits = f.digits,
                                    output.type = output.type,
                                    decimal.mark = decimal.mark),
      p.value.label = p_value_label(value = p.value,
                                    small.p = small.p,
                                    digits = p.digits,
                                    output.type = output.type,
                                    decimal.mark = decimal.mark),
      n.label = italic_label(value = n,
                             value.name = "n",
                             digits = 0,
                             fixed = TRUE,
                             output.type = output.type,
                             decimal.mark = decimal.mark),
      knots.label = italic_label(value = knots,
                                 "x",
                                 digits = 3,
                                 output.type = output.type,
                                 decimal.mark = decimal.mark),
      grp.label = grp.label,
      method.label = paste("\"method: ", method.name, "\"", sep = ""),
    )
  }

  # add members common to all output types
  # as we support user-defined fit functions, and user assembled labels,
  # some of the numeric values can be necessary for conditional composing
  # of labels in all output types.
  zz <- tibble::tibble(
    coefs = list(coefs), # numeric vector
    coefs.names = list(coefs.names), # character vector
    rr.confint.level = rsquared.conf.level,
    rr.confint.low = rr.confint.low,
    rr.confint.high = rr.confint.high,
    f.value = f.value,
    f.df1 = f.df1,
    f.df2 = f.df2,
    r.squared = rr,
    adj.r.squared = adj.rr,
    p.value = p.value,
    AIC = AIC,
    BIC = BIC,
    n = n,
    knots = list(knots),
    knots.se = list(knots.se),
    knots.names = list(knots.names),
    fm.method = method.name,
    fm.class = fm.class[1],
    fm.formula = formula.ls,
    fm.formula.chr = format(formula.ls)
  )
  z <- cbind(z, zz)

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
                   dropped_aes = "weight",
                   required_aes = c("x", "y"),
                   optional_aes = "grp.label"
  )


