#' Fitted model prediction and annotations
#'
#' Statistics \code{stat_poly_line} and \code{\link{stat_poly_eq}()} fit a
#' model, by default with \code{stats::lm()}, but alternatively using other
#' model fit functions. While \code{stat_poly_line} adds a prediction line and
#' band, \code{\link{stat_poly_eq}()} adds textual labels to a plot.
#'
#' @inheritParams fit_models_internal
#' @param data A layer specific dataset, only needed if you want to override the
#'   plot defaults.
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}()}. Only needs to be set at the layer level if you
#'   are overriding the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points on this
#'   layer.
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
#' @param method function or character If character, "lm", "rlm", "lmrob",
#'   "lts", "gls", "ma", "sma", "segreg", "rq" or the name of a model fit
#'   function are accepted, possibly followed by the fit function's
#'   \code{method} argument separated by a colon (e.g. \code{"rlm:M"}). If a
#'   function is different to \code{lm()}, \code{rlm()}, \code{ltsReg()},
#'   \code{gls()}, \code{ma}, \code{sma}, it must have formal parameters named
#'   \code{formula}, \code{data}, and \code{weights}. See Details.
#' @param se Display confidence interval around smooth? (`TRUE` by default only
#'   for fits with \code{lm()} and \code{rlm()}, see `level` to control.)
#' @param fm.values logical Add metadata and parameter estimates extracted from
#'   the fitted model object; \code{FALSE} by default.
#' @param fullrange logical Should the fit prediction span the full
#'   range of the plot, or just the range of the explanatory variable?
#' @param limit.to character or numeric If character one of \code{""},
#'   \code{"x"}, \code{"y"} or \code{"xy"}. Should the fit prediction be
#'   constrained to the range of the variables mapped to \code{x} and/or
#'   \code{y} in each data group? If numeric, the \emph{new data} values to use
#'   for the explanatory variable when computing the predicted line and
#'   confidence band. When set, \code{limit.to} silently overrides
#'   \code{fullrange}!
#' @param level Level of confidence interval to use (0.95 by default).
#' @param n Number of points at which to predict with the fitted model.
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
#' @param output.type character One of "expression", "text", "markdown",
#'   "marquee", "latex", "latex.eqn", "latex.deqn" or "numeric".
#' @param parse logical Passed to the geom. If \code{TRUE}, the labels will be
#'   parsed into expressions and displayed as described in
#'   \code{\link[grDevices]{plotmath}}. Default is \code{TRUE} if
#'   \code{output.type = "expression"} and \code{FALSE} otherwise.
#'
#' @aesthetics StatPolyEq
#' @aesthetics StatPolyLine
#'
#' @details Statistics \code{stat_poly_line()} and \code{\link{stat_poly_eq}()}
#'   fit a model consistently, but return different values.
#'   \code{stat_poly_line()} plots a prediction line and band, similarly to
#'   \code{\link[ggplot2]{stat_smooth}()}
#'   but has different defaults and supports a different set of model fit
#'   functions.
#'   \code{\link{stat_poly_eq}()} adds textual labels for
#'   \eqn{R^2}, adjusted \eqn{R^2}, the fitted model equation, \eqn{P}, and
#'   other parameters from a fitted model to a plot.
#'
#'   Lack of methods or explicit support for extraction of individual parameters
#'   results in the affected estimates and corresponding labels being set to
#'   \code{NA}. Similarly, confidence bands for the prediction line are not
#'   plotted in some cases, while in the case of MA and SMA models, the band
#'   only displays the uncertainty of the slope rather than for both slope plus
#'   intercept. While strings for \eqn{R^2}, adjusted \eqn{R^2}, \eqn{F}, and
#'   \eqn{P} annotations are returned for all valid linear models and many other
#'   types of fitted models, an automatically constructed character string for
#'   the fitted model equation is returned only for polynomials (see below).
#'   However, when not generated automatically, the equation can still be
#'   assembled by the user within the call to \code{\link[ggplot2]{aes}()}. A
#'   label for the confidence interval of \eqn{R^2}, based on values computed
#'   with function \code{\link[confintr]{ci_rsquared}()} from package 'confintr'
#'   is returned when possible.
#'
#'   When possible, i.e., nearly always, the \code{formula} used to build the
#'   equation label is extracted from the returned fitted model object. Most
#'   fitted model objects follow the example of \code{lm()} and include the
#'   formula for the model that has been fitted. Thus, this model formula can
#'   safely differ from the argument passed to parameter \code{formula} in the
#'   call to \code{stat_poly_eq()}.
#'
#'   \emph{The stats are designed to support user-defined methods that
#'   implement any or all of \code{method} selection, model \code{formula}
#'   selection, dynamically adjusted \code{method.args} and conditional skipping
#'   of labelling on a by group basis.}
#'
#'   The minimum number of observations with distinct values in the explanatory
#'   variable can be set through parameter \code{n.min}. The default \code{n.min
#'   = 2L} is the smallest suitable for method \code{"lm"} but too small for
#'   method \code{"rlm"} for which \code{n.min = 3L} is needed. Anyway, model
#'   fits with very few observations are of little interest and using larger
#'   values of \code{n.min} than the default is wise.
#'
#'   As some model fitting approaches depend on the RNG (pseudo-Random Number
#'   Generator), when \code{fit.seed} is not \code{NA} it is used as argument in
#'   a call to \code{\link[base:Random]{set.seed}()} immediately ahead of model
#'   fitting, i.e., once for each group of observations.
#'
#'   Singularity, convergence, etc., are handled by the model fit functions.
#'   With method \code{"lm"}, singularity results in terms being dropped with a
#'   message if more numerous than can be fitted with a singular (exact) fit. In
#'   this case and if the model results in a perfect fit due to low number of
#'   observation, estimates for various parameters are \code{NaN} or \code{NA}.
#'   With methods other than \code{"lm"}, the model fit functions simply fail in
#'   case of singularity, e.g., singular fits are not implemented in
#'   \code{"rlm"}.
#'
#' @inheritSection check_output_type Output types
#'
#' @section Model formula and model fitting:
#'   A ggplot statistic receives as \code{data} a data frame that is not the one
#'   passed as argument by the user, but instead a data frame with the variables
#'   mapped to aesthetics. In \code{stat_poly_eq()} the compute function is
#'   applied by group, each call "seeing" the subset of \code{data} for an
#'   individual group. As supported models are for regression lines,
#'   variables mapped to \code{x} and \code{y} should both be continuous, i.e.,
#'   numeric or date time and model formulas defined using \code{x} and \code{y}
#'   as variable names.
#'
#'   The interpretation of the argument passed to \code{formula} is enhanced
#'   compared to \code{stat_smooth()}. Formulas with \code{x} as explanatory
#'   variable work as in \code{stat_smooth()} but formulas with \code{y} as
#'   explanatory variable are also accepted. \code{orientation} is set
#'   automatically based on which explanatory variable appears in the formula.
#'   Spline-based smoothers are only partially supported.
#'
#' @section Model equation label:
#'   By default the equation label uses as symbols the names of the aesthetics,
#'   \code{x} and \code{y}. However, \code{"x"} and \code{"y"} can be
#'   substituted by providing a replacement character string for the
#'   right-hand-side and left-hand-side through \code{eq.x.rhs} and
#'   \code{eq.with.lhs}, respectively. For backward compatibility a logical is
#'   also accepted as argument for \code{eq.with.lhs}, with \code{FALSE}
#'   suppressing the left-hand-side.
#'
#'   If the model \code{formula} includes a transformation of the explanatory
#'   variable in its right-hand-side (rhs), a matching argument should be passed
#'   to parameter \code{eq.x.rhs} as its default value would result in an
#'   equation label that does not reflect the applied transformation. In most
#'   cases, a transformation should not be applied within the left hand side
#'   (lhs) of the model formula, but instead in the mapping of the response
#'   variable within \code{aes}. In this case it may be necessary to also pass a
#'   matching argument to parameter \code{eq.with.lhs}.
#'
#'   Parameter \code{orientation} is redundant as the orientation can be set
#'   by the \code{formula} but is included for consistency with
#'   \code{ggplot2::stat_smooth()}.
#'
#' @section Position of labels:
#'   When data are grouped by mapping a factor to an aesthetic, e.g.,
#'   \code{colour}, \code{shape} and/or \code{linetype} the model is fitted
#'   separately to each group, and for each group a whole set of labels is
#'   generated. If the argument passed to \code{label.y} is a vector of length
#'   1, this value determines the position of the equation and/or other labels
#'   for the first group, and the positions of the labels for the remaining
#'   groups are generated by adding \code{vspace} based on the group number.
#'   If the argument passed to \code{label.y} is a vector of length > 1, it is
#'   used unchanged, possibly extended by recycling, ignoring \code{vstep}.
#'
#'   If the labels are rotated by 90 degrees then the automatic stepping is
#'   best based on \code{hstep} with \code{vstep = 0}. Similarly as described
#'   above, if \code{label.x} is a vector of length > 1, it is
#'   used unchanged, possibly extended by recycling, ignoring \code{hstep}.
#'
#'   When using facets and with a grouping that does not repeat in each panel,
#'   the automatic positioning in most cases will not be the desired one. Manual
#'   positioning using a vector of length > 1 for \code{label.x} and/or
#'   \code{label.y} is the currently available workaround.
#'
#' @section Range of the prediction line: The range of the prediction line is
#'   controlled by parameters \code{fullrange} and \code{limit.to}.
#'   \code{fullrange} is backwards compatible both with earlier versions of
#'   'ggpmisc' and with \code{stat_smooth()} from 'ggplot2'; an argument passed
#'   to \code{limit.to} overrides \code{fullrange} making it possible to
#'   constrain the range to that of \code{x}, \code{y}, or both simultaneously,
#'   with \code{"x"}, \code{"y"}, or \code{"xy"}, respectively, as argument.
#'   \code{limit.to} also accepts a numeric vector of values to be used as
#'   \code{newdata} when computing the prediction. Limiting the range based on
#'   both aesthetics is the best approach for major axis regression (MA, SMA,
#'   RMA) but can occasionally be useful also with some other methods when
#'   slopes are very steep and error variance in the explanatory variable is
#'   large. A numeric vector can be used to predict the response at specific
#'   values of the explanatory variable. If a single or very few values are
#'   predicted, it can be necessary to override the default \code{geom =
#'   "smooth"} with \code{geom = "pointrange"}.
#'
#' @section Model fit methods supported:
#'   Several model fit functions are supported explicitly (see tables), and some
#'   of their differences smoothed out. Compatibility is checked late, based on
#'   the class of the returned fitted model object. This makes it possible to
#'   use wrapper functions that do model selection or other adjustments to the
#'   fit procedure on a per panel or per group basis. Moreover, if the value
#'   returned as model fit object is \code{NULL} or \code{NA}, plotting is
#'   skipped on a per group within panel basis.
#'
#'   \emph{In the case of fitted model objects of classes not explicitly
#'   supported, an attempt is made to find the usual accessors and/or fitted
#'   object members, and if found, either complete or partial support is
#'   frequently achieved. In this case a message is issued encouraging users to
#'   check the validity of the values extracted as the structure of fitted model
#'   objects belonging to different classes and the values returned by their
#'   accessors can vary, potentially resulting in decoding errors leading to the
#'   return of wrong values for estimates.}
#'
#'   The argument to parameter \code{method} can be either the name of a
#'   function object, possibly using double colon notation in case its package
#'   is not attached, or a character string matching the function name for
#'   functions in the search path. This approach makes it possible to support
#'   model fit functions that are not dependencies of 'ggpmisc'. Either by
#'   attaching the package where the function is defined and passing it by name
#'   or as string, or using double colon notation when passing the name of the
#'   function.
#'
#'   User-defined functions can be passed as argument to parameter \code{method}
#'   as long as they have parameters \code{formula}, \code{data} \code{subset}
#'   and possibly \code{weights}. Additional arguments can be passed to any
#'   method as a named list through parameter \code{method.args}. As in
#'   \code{\link[ggplot2:geom_smooth]{stat_smooth}()} prior \code{weights} are
#'   passed to the model fit functions' \code{weights} (plural!) parameter by
#'   mapping a numeric variable to plot aesthetic \code{weight} (singular!).
#'
#'   Tables 1 lists natively supported model fit functions, with the
#'   caveat that only some 'broom' methods' specializations have been actually
#'   tested with statistics from 'ggpmisc'. In addition, the statistics based
#'   on 'broom' methods require the user to tailor their behaviour by passing
#'   additional arguments in the call and occasionally some detective work to
#'   find out the names of variables in the returned data frame as these names
#'   are set by methods from 'broom'.
#'
#'   \strong{Table 1.} Model fit methods supported by the different statistics
#'   available in package 'ggpmisc'. Column \eqn{f} indicates whether
#'   computations are done by group (G) or by plot panel (P).
#'   \tabular{lcl}{
#'   \strong{Statistic} \tab \eqn{f} \tab \strong{Supported model fit methods} \cr
#'   \code{\link{stat_poly_line}()} \tab G \tab "lm", "rlm", "lts", "sma", "ma", "gls", \emph{others with methods} \code{\link[stats]{predict}()} or \code{\link[stats]{fitted}()} \cr
#'   \code{\link{stat_poly_eq}()}   \tab G \tab "lm", "rlm", "lts", "sma", "ma", "gls",  \emph{others with needed accesors} \cr
#'   \code{\link{stat_quant_line}()} \tab G \tab "rq", "rqss" \cr
#'   \code{\link{stat_quant_band}()} \tab G \tab "rq", "rqss" \cr
#'   \code{\link{stat_quant_eq}()} \tab G \tab "rq", "rqss" \cr
#'   \code{\link{stat_ma_line}()} \tab G \tab "SMA", "MA", "RMA", "OLS" \cr
#'   \code{\link{stat_ma_eq}()} \tab G \tab "SMA", "MA", "RMA", "OLS" \cr
#'   \code{\link{stat_fit_residuals}()} \tab G \tab "lm", "rlm", "lts", "sma", "ma", "gls", "rq", "rqss" \emph{others with method} \code{\link[stats]{residuals}()} \cr
#'   \code{\link{stat_fit_fitted}()} \tab G \tab "lm", "rlm", "lts", "gls", "rq", "rqss" \emph{others with method} \code{\link[stats]{fitted}()} \cr
#'   \code{\link{stat_fit_deviations}()} \tab G \tab "lm", "rlm", "lts", "gls", "rq", "rqss" \emph{others with methods} \code{\link[stats]{fitted}()} and \code{\link[stats]{weights}()} \cr
#'   \code{\link{stat_fit_augment}()} \tab G \tab \emph{any with 'broom' method} \code{\link[broom]{augment}()} \cr
#'   \code{\link{stat_fit_glance}()} \tab G \tab \emph{any with 'broom' method} \code{\link[broom]{glance}()} \cr
#'   \code{\link{stat_fit_tidy}()} \tab G \tab \emph{any with 'broom' method} \code{\link[broom]{tidy}()} \cr
#'   \code{\link{stat_fit_tb}()} \tab P \tab \emph{any with 'broom' method} \code{\link[broom]{tidy}()} \cr
#'   }
#'
#'   The single colon notation is based on parsing
#'   the name and is available when passing the name of the fit method as a
#'   character string. In a string such as "head:tail" the "head" gives the name
#'   of the model fit function and the "tail" gives the argument to pass it's
#'   \code{method} parameter. This is only a convenience, as \code{method.args}
#'   can be also used. In some methods, i.e., splines, the default
#'   \code{formula = y ~ x} needs to be overridden by the user.
#'
#'   Table 2 lists the correspondence of pre-defined \emph{method names}
#'   to model fit method functions. As mentioned above, these are only
#'   a subset of the model fit methods that are expected to work. When using
#'   these names there is no need for users to attach additional packages but
#'   the packages must be available (installed).
#'
#'   \strong{Table 2.} Available predefined method names, the model fit functions
#'   they call, the packages where the functions reside, the class of the
#'   returned fitted model object and the arguments that can be
#'   passed to their \code{method} parameter using single colon notation.
#'   \tabular{llll}{
#'   \strong{Predefined method names} \tab \strong{Model fit methods} \tab \strong{R package} \tab \strong{Object class} \cr
#'   "lm", "lm:qr" \tab \code{\link[stats]{lm}()} \tab 'stats' \tab "lm" \cr
#'   "rlm", "rlm:M", "rlm:MM" \tab \code{\link[MASS]{rlm}()} \tab 'MASS' \tab "rlm" ("lm") \cr
#'   "lts", "ltsReg" \tab \code{\link[robustbase]{ltsReg}()} \tab 'robustbase' \tab "lts" \cr
#'   "ma", "sma", "sma:SMA", "sma:MA", "sma:OLS" \tab \code{\link[smatr]{sma}()} \tab 'smatr' \tab "ma" or "sma" \cr
#'   "gls", "gls:REML", "gls:ML" \tab \code{\link[nlme]{gls}()} \tab 'nlme' \tab "gls" \cr
#'   "rq", "rq:sfn", "rq:sfnc", "rq:lasso" \tab \code{\link[quantreg]{rq}()} \tab 'quantreg' \tab "rq" \cr
#'   "rqss", "rqss:sfn", "rqss:sfnc", "rqss:lasso" \tab \code{\link[quantreg]{rqss}()} \tab 'quantreg' \tab "rqss" \cr
#'   "SMA", "MA", "RMA", "OLS" \tab \code{\link[lmodel2]{lmodel2}()} \tab 'lmodel2' \tab ("list") \cr
#'   }
#'
#' @return \code{stat_poly_eq()} returns a data frame, with a single row per
#'   group and columns as described below. \code{stat_poly_line()} returns a
#'   data frame, with \code{n} rows per group and columns as described below. In
#'   cases when the number of observations is less than \code{n.min} or when the
#'   model fit function returns a single \code{NA} or \code{NULL}, a data frame
#'   with no rows or columns (built by \code{data.frame()}) is returned, and
#'   silently rendered as an empty/invisible plot layer.
#'
#'   When a \code{predict()} method is not available for the fitted model class,
#'   the value returned by calling \code{fitted()}, if available, is replaces it
#'   and the returned data frame with as many rows as observations, instead of
#'   \code{n} rows, is returned with a message.
#'
#' @section Variables returned by `stat_poly_line()`:
#'
#'   Some of the variables can have missing values or depend on
#'   \code{orientation} and/or \code{method}. A message is issued listing
#'   the column names containing non-missing values.
#'
#'   \describe{ \item{y \strong{or} x}{predicted value}
#'   \item{ymin \strong{or} xmin}{lower confidence limit around the fitted line}
#'   \item{ymax \strong{or} xmax}{upper confidence limit around the fitted line}
#'   \item{se}{standard error} }
#'
#'   If \code{fm.values = TRUE} is passed then columns based on the summary of
#'   the model fit are added, with the same value in each row within a group.
#'   This is wasteful and disabled by default, but provides a simple and robust
#'   approach to achieve effects like colouring or hiding of the model fit line
#'   based on \eqn{P}, \eqn{R^2}, \eqn{R_{adj}^2} or the number of
#'   observations in a fit.
#'
#' @section Variables returned by \code{\link{stat_poly_eq}()}:
#'
#'   Computed variables and their names can vary depending on the \code{method}
#'   used to fit a model or the \code{output.type} in use. They can also depend
#'   for a given \code{method} on other arguments passed when fitting a model or
#'   extracting estimates and other computed values. A message is issued listing
#'   the short names for formatted labels as recognized by functions
#'   \code{\link{use_label}()} and \code{\link{f_use_label}()}.
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
#' of \code{\link[gginnards]{geom_debug}()} as shown in the last examples below.
#'
#' @references Originally written as an answer to question 7549694 at
#'   Stackoverflow but enhanced based on suggestions from several users and my
#'   own needs.
#'
#' @seealso Consult the documentation of the model fit functions used
#'   for the details and additional arguments that can be passed to
#'   them by name through parameter \code{method.args}.
#'
#'   Please, see the articles in
#'   \href{https://docs.r4photobiology.info/ggpmisc/}{online-only documentation}
#'   for additional use examples and guidance.
#'
#' @family 'ggpmisc' statistics for model fits
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
#'   stat_poly_eq(aes(label =
#'                  after_stat(paste(rr.label, n.label, sep = "*\", \"*"))),
#'                formula = formula)
#'
#' # manually assemble and map a specific label using sprintf() and aes()
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(aes(label =
#'                  after_stat(
#'                    sprintf("%s*\" with \"*%s*\" and \"*%s",
#'                            rr.label, f.value.label, p.value.label))),
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
#'   stat_poly_eq(aes(label =
#'                      after_stat(ifelse(adj.r.squared > 0.96,
#'                                    paste(adj.rr.label, eq.label,
#'                                          sep = "*\", \"*"),
#'                                    adj.rr.label))),
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
#' # Inspecting the returned data using geom_debug_group()
#' # This provides a quick way of printing the data frame returned.
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
#'                  geom = "debug_group")
#'
#'
#' @export
#'
stat_poly_eq <- function(mapping = NULL,
                         data = NULL,
                         geom = "text_npc",
                         position = "identity",
                         ...,
                         orientation = NA,
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
                            default.formula = y ~ x,
                            formula = formula,
                            formula.on.x = FALSE)
  orientation <- temp[["orientation"]]
  formula <-  temp[["formula"]]

  output.type <-
    check_output_type(output.type = output.type, geom = geom)

  if (is.null(parse)) {
    parse <- output.type == "expression"
  }

  # is the model formula that of an increasing polynomial?
  # is yes it will be parsed and combined with estimates into a character string
  mk.eq.label <- output.type != "numeric" &&
                   check_poly_formula(formula,
                                      orientation,
                                      check.transf.lhs = !is.character(eq.with.lhs),
                                      check.transf.rhs = !is.character(eq.x.rhs))
                   !any(grepl("lspline|bs", as.character(formula))) # not a spline

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

  if (is.null(data$weight)) {
    data$weight <- 1
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

  temp.ls <- fit_models_internal(data = data,
                                 method = method,
                                 method.name = method.name,
                                 method.args = method.args,
                                 n.min = n.min,
                                 formula = formula,
                                 fit.seed = fit.seed,
                                 orientation = orientation,
                                 accept.rq = FALSE)
  if (!length(temp.ls) || !length(temp.ls[["fm"]])) {
    # An empty data.frame results in no plot layer when passed to geoms
    return(data.frame())
  }
  fm <- temp.ls[["fm"]]
  method.name <- temp.ls[["method.name"]] # argument or default which varies
  method.args <- temp.ls[["method.args"]] # argument or default which varies

  fm.class <- class(fm)
  if (fm.class[1] == "sma") {
    # summary.sma prints results and returns NULL
    fm.summary <- NULL
  } else {
    fm.summary <- summary(fm)
  }

  # allow model formula selection by the model fit method
  # extract formula from fitted model object, fall back on argument on failure
  formula.ls <- fail_safe_formula(fm, method.args, verbose = TRUE)

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
  if (forced.origin && !"elevation" %in% coefs.names) { # handle 'smatr'
    coefs <- c(0, coefs)
  }
  selector <- !is.na(coefs)
  coefs <- coefs[selector]
  if (!all(selector)) {
    message("Terms dropped from model (singularity); n = ",
            nrow(data), " in group.")
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

  if (output.type == "numeric") {
    show_colnames(z, stat.name = "stat_poly_eq")
  } else {
    show_labels(z, stat.name = "stat_poly_eq")
  }

  z
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPolyEq <-
  ggplot2::ggproto("StatPolyEq", ggplot2::Stat,
                   extra_params = c("na.rm", "parse", "orientation"),
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


