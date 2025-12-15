#' Labels for pairwise multiple comparisons
#'
#' \code{stat_multcomp} fits a linear model by default with \code{stats::lm()}
#' but alternatively using other model fit functions. The model is passed to
#' function \code{glht()} from package 'multcomp' to fit Tukey, Dunnet or other
#' \strong{pairwise} contrasts and generates labels based on adjusted
#' \emph{P}-values.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset, only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use to display the data.
#' @param position The position adjustment to use for overlapping points on this
#'   layer.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical indicating whether NA values should be stripped before
#'   the computation proceeds.
#' @param formula a formula object. Using aesthetic names \code{x} and \code{y}
#'   instead of original variable names.
#' @param method function or character If character, "lm" (or its equivalent
#'   "aov"), "rlm" or the name of a model fit function are accepted, possibly
#'   followed by the fit function's \code{method} argument separated by a colon
#'   (e.g. \code{"rlm:M"}). If a function different to \code{lm()}, it must
#'   accept as a minimum a model formula through its first parameter, and have
#'   formal parameters named \code{data}, \code{weights}, and \code{method}, and
#'   return a model fit object accepted by function \code{glht()}.
#' @param method.args named list with additional arguments.
#' @param contrasts character vector of length one or a numeric matrix. If
#'    character, one of "Tukey" or "Dunnet". If a matrix, one column per level
#'    of the factor mapped to \code{x} and one row per \strong{pairwise}
#'    contrast.
#' @param p.adjust.method character As the argument for parameter \code{type} of
#'   function \code{adjusted()} passed as argument to parameter \code{test} of
#'   \code{\link[multcomp]{summary.glht}}. Accepted values are "single-step",
#'   "Shaffer", "Westfall", "free", "holm", "hochberg", "hommel", "bonferroni",
#'   "BH", "BY", "fdr", "none".
#' @param fit.seed RNG seed argument passed to \code{\link[base:Random]{set.seed}()}.
#'   Defaults to \code{NA}, which means that \code{set.seed()} will not be
#'   called.
#' @param small.p logical If true, use of lower case \emph{p} instead of capital
#'   \emph{P} as the symbol for \emph{P}-value in labels.
#' @param adj.method.tag numeric, character or function If \code{numeric}, the
#'   length in characters of the abbreviation of the method used to adjust
#'   \emph{p}-values. A value of zero, adds no label and a negative value uses
#'   as starting point for the abbreviation the word "adjusted". If
#'   \code{character} its value is used as subscript. If a \code{function}, the
#'   value used is the value returned by the function when passed
#'   \code{p.adjust.method} as its only argument.
#' @param p.digits integer Number of digits after the decimal point to
#'   use for \eqn{R^2} and \emph{P}-value in labels.
#' @param label.type character One of "bars", "letters" or "LETTERS", selects
#'   how the results of the multiple comparisons are displayed. Only "bars" can
#'   be used together with \code{contrasts = "Dunnet"}.
#' @param fm.cutoff.p.value numeric [0..1] The \emph{P}-value for the main
#'   effect of factor \code{x} in the ANOVA test for the fitted model above
#'   which no pairwise comparisons are computed or labels generated. Be aware
#'   that recent literature tends to recommend to consider which testing
#'   approach is relevant to the problem at hand instead of requiring the
#'   significance of the main effect before applying multiple comparisons'
#'   tests. The default value is 1, imposing no restrictions.
#' @param mc.cutoff.p.value numeric [0..1] The \emph{P}-value for the individual
#'   contrasts above which no labelled bars are generated. Default is 1,
#'   labelling all pairwise contrasts tested.
#' @param mc.critical.p.value numeric The critical \emph{P}-value used for tests
#'   when encoded as letters.
#' @param label.y numeric vector Values in native data units or if
#'   \code{character}, one of "top" or "bottom". Recycled if too short and
#'   truncated if too long.
#' @param vstep numeric in npc units, the vertical displacement step-size
#'   used between labels for different contrasts when \code{label.type = "bars"}.
#' @param output.type character One of "expression", "LaTeX", "text",
#'   "markdown" or "numeric".
#' @param orientation character Either "x" or "y" controlling the default for
#'   \code{formula}. \strong{Support for \code{orientation} is not yet
#'   implemented but is planned.}
#' @param parse logical Passed to the geom. If \code{TRUE}, the labels will be
#'   parsed into expressions and displayed as described in \code{?plotmath}.
#'   Default is \code{TRUE} if \code{output.type = "expression"} and
#'   \code{FALSE} otherwise.
#'
#' @note R option \code{OutDec} is obeyed based on its value at the time the plot
#'   is rendered, i.e., displayed or printed. Set \code{options(OutDec = ",")}
#'   for languages like Spanish or French.
#'
#' @details This statistic can be used to automatically annotate a plot with
#'   \emph{P}-values for \strong{pairwise} multiple comparison tests, based on
#'   Tukey contrasts (all pairwise), Dunnet contrasts (other levels against the
#'   first one) or a subset of all possible pairwise contrasts. See Meier (2022,
#'   Chapter 3) for an accessible explanation of multiple comparisons and
#'   contrasts with package 'multcomp', of which \code{stat_multcomp()} is
#'   mostly a wrapper.
#'
#'   The explanatory variable mapped to the \emph{x} aesthetic must be a factor
#'   as this creates the required grouping. Currently, contrasts that involve
#'   more than two levels of a factor, such as the average of two treatment
#'   levels against a control level are not supported, mainly because they
#'   require a new geometry that I need to design, implement and add to package
#'   'ggpp'.
#'
#'   Two ways of displaying the outcomes are implemented, and are selected by
#'   `"bars"`, `"letters"` or `"LETTERS"` as argument to parameter
#'   `label.type`. `"letters"` and `"LETTERS"` can be used only with Tukey
#'   contrasts, as otherwise the encoding is ambiguous. As too many bars clutter
#'   a plot, the maximum number of factor levels supported for `"bars"` together
#'   with Tukey contrasts is five, while together with Dunnet contrasts or
#'   contrasts defined by a numeric matrix, no limit is imposed.
#'
#'   \code{stat_multcomp()} by default generates character labels ready to be
#'   parsed as R expressions but LaTeX (use TikZ device), markdown (use package
#'   'ggtext') and plain text are also supported, as well as numeric values for
#'   user-generated text labels. The value of \code{parse} is set automatically
#'   based on \code{output.type}, but if you assemble labels that need parsing
#'   from \code{numeric} output, the default needs to be overridden. This
#'   statistic only generates annotation labels and segments connecting the
#'   compared factor levels, or letter labels that discriminate significantly
#'   different groups.
#'
#' @inheritSection check_output_type Output types
#'
#' @section Aesthetics: \code{stat_multcomp()} understands \code{x} and
#'   \code{y}, to be referenced in the \code{formula} and \code{weight} passed
#'   as argument to parameter \code{weights}. A factor must be mapped to
#'   \code{x} and \code{numeric} variables to \code{y}, and, if used, to
#'   \code{weight}. In addition, the aesthetics understood by the geom
#'   (\code{"label_pairwise"} is the default for \code{label.type = "bars"},
#'   \code{"text"} is the default for \code{label.type = "letters"} and for
#'   \code{label.type = "LETTERS"}) are understood and grouping
#'   respected.
#'
#' @return A data frame with one row per comparison for \code{label.type =
#'   "bars"}, or a data frame with one row per factor \code{x} level for
#'   \code{label.type = "letters"} and for \code{label.type = "LETTERS"}.
#'   Variables (= columns) as described under \strong{Computed variables}.
#'
#' @section Computed variables:
#' If \code{output.type = "numeric"} and
#' \code{label.type = "bars"} the returned tibble contains
#' columns listed below. In all cases if the model fit function used does not return a value,
#' the label is set to \code{character(0L)} and the numeric value to \code{NA}.
#' \describe{
#'   \item{x,x.left.tip,x.right.tip}{x position, numeric.}
#'   \item{y}{y position, numeric.}
#'   \item{coefficients}{Delta estimate from pairwise contrasts, numeric.}
#'   \item{contrasts}{Contrasts as two levels' ordinal "numbers" separated by a dash, character.}
#'   \item{tstat}{\emph{t}-statistic estimates for the pairwise contrasts, numeric.}
#'   \item{p.value}{\emph{P}-value for the pairwise contrasts.}
#'   \item{fm.method}{Set according \code{method} used.}
#'   \item{fm.class}{Most derived class of the fitted model object.}
#'   \item{fm.formula}{Formula extracted from the fitted model object if available, or the formula argument.}
#'   \item{fm.formula.chr}{Formula extracted from the fitted model object if available, or the formula argument, formatted as character.}
#'   \item{mc.adjusted}{The method used to adjust the \emph{P}-values.}
#'   \item{mc.contrast}{The type of contrast used for multiple comparisons.}
#'   \item{n}{The total number of observations or rows in data.}
#'   \item{default.label}{text label, always included, but possibly NA.}
#'   }
#'
#' If output.type is not \code{"numeric"} the returned data frame includes in
#' addition the following labels:
#'
#' \describe{
#'   \item{stars.label}{\emph{P}-value for the pairwise contrasts encoded as "starts", character.}
#'   \item{p.value.label}{\emph{P}-value for the pairwise contrasts, character.}
#'   \item{delta.label}{The coefficient or estimate for the difference between compared pairs of levels.}
#'   \item{t.value.label}{\emph{t}-statistic estimates for the pairwise contrasts, character.}
#'   }
#'
#' If \code{label.type = "letters"} or \code{label.type = "LETTERS"} the returned tibble contains
#' columns listed below.
#'
#' \describe{
#'   \item{x,x.left.tip,x.right.tip}{x position, numeric.}
#'   \item{y}{y position, numeric.}
#'   \item{critical.p.value}{\emph{P}-value used in pairwise tests, numeric.}
#'   \item{fm.method}{Set according \code{method} used.}
#'   \item{fm.class}{Most derived class of the fitted model object.}
#'   \item{fm.formula}{Formula extracted from the fitted model object if available, or the formula argument.}
#'   \item{fm.formula.chr}{Formula extracted from the fitted model object if available, or the formula argument, formatted as character.}
#'   \item{mc.adjusted}{The method used to adjust the \emph{P}-values.}
#'   \item{mc.contrast}{The type of contrast used for multiple comparisons.}
#'   \item{n}{The total number of observations or rows in data.}
#'   \item{default.label}{text label, always included, but possibly NA.}
#'   }
#'
#' If output.type is not \code{"numeric"} the returned data frame includes in
#' addition the following labels:
#'
#' \describe{
#'   \item{letters.label}{Letters that distinguish levels based on significance from multiple comparisons test.}
#'   }
#'
#' @section Alternatives: \code{stat_signif()} in package 'ggsignif' is
#'   an earlier and independent implementation of pairwise tests.
#'
#' @seealso This statistic uses the implementation of Tests of General Linear
#'   Hypotheses in function \code{\link[multcomp]{glht}}. See
#'   \code{\link[multcomp]{summary.glht}} and \code{\link[stats]{p.adjust}}
#'   for the supported and tests and the references therein for the theory
#'   behind them.
#'
#' @family ggplot statistics for multiple comparisons
#'
#' @references
#'
#' Meier, Lukas (2022) \emph{ANOVA and Mixed Models: A Short Introduction
#' Using R}. Chapter 3 Contrasts and Multiple Testing. The R Series. Boca Raton:
#' Chapman and Hall/CRC. ISBN: 9780367704209, \doi{10.1201/9781003146216}.
#'
#' @examples
#'
#' p1 <- ggplot(mpg, aes(factor(cyl), hwy)) +
#'   geom_boxplot(width = 0.33)
#'
#' ## labeleld bars
#'
#' p1 +
#'   stat_multcomp()
#'
#' p1 +
#'   stat_multcomp(adj.method.tag = 0)
#'
#' # test against a control, with first level being the control
#' # change order of factor levels in data to set the control group
#' p1 +
#'   stat_multcomp(contrasts = "Dunnet")
#'
#' # arbitrary pairwise contrasts, in arbitrary order
#' p1 +
#'   stat_multcomp(contrasts = rbind(c(0, 0, -1, 1),
#'                                   c(0, -1, 1, 0),
#'                                   c(-1, 1, 0, 0)))
#'
#' # different methods to adjust the contrasts
#' p1 +
#'   stat_multcomp(p.adjust.method = "bonferroni")
#'
#' p1 +
#'   stat_multcomp(p.adjust.method = "holm")
#'
#' p1 +
#'   stat_multcomp(p.adjust.method = "fdr")
#'
#' # no correction, useful only for comparison
#' p1 +
#'   stat_multcomp(p.adjust.method = "none")
#'
#' # sometimes we need to expand the plotting area
#' p1 +
#'   stat_multcomp(geom = "text_pairwise") +
#'   scale_y_continuous(expand = expansion(mult = c(0.05, 0.10)))
#'
#' # position of contrasts' bars (based on scale limits)
#' p1 +
#'   stat_multcomp(label.y = "bottom")
#'
#' p1 +
#'   stat_multcomp(label.y = 11)
#'
#' # use different labels: difference and P-value from hypothesis tests
#' p1 +
#'   stat_multcomp(use_label("Delta", "P"),
#'                 size = 2.75)
#'
#' # control smallest P-value displayed and number of digits
#' p1 +
#'   stat_multcomp(p.digits = 4)
#'
#' # label only significant differences
#' # but test and correct for all pairwise contrasts!
#' p1 +
#'   stat_multcomp(mc.cutoff.p.value = 0.01)
#'
#' ## letters as labels for test results
#'
#' p1 +
#'   stat_multcomp(label.type = "letters")
#'
#' # use capital letters
#' p1 +
#'   stat_multcomp(label.type = "LETTERS")
#'
#' # location
#' p1 +
#'   stat_multcomp(label.type = "letters",
#'                 label.y = "top")
#'
#' p1 +
#'   stat_multcomp(label.type = "letters",
#'                 label.y = 0)
#'
#' # stricter critical p-value than default used for test
#' p1 +
#'   stat_multcomp(label.type = "letters",
#'                 mc.critical.p.value = 0.01)
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
#' p1 +
#'   stat_multcomp(label.type = "bars",
#'                 geom = "debug")
#'
#' if (gginnards.installed)
#' p1 +
#'   stat_multcomp(label.type = "letters",
#'                 geom = "debug")
#'
#' if (gginnards.installed)
#' p1 +
#'   stat_multcomp(label.type = "bars",
#'                 output.type = "numeric",
#'                 geom = "debug")
#'
#' @export
#'
stat_multcomp <- function(mapping = NULL,
                          data = NULL,
                          geom = NULL,
                          position = "identity",
                          ...,
                          formula = NULL,
                          method = "lm",
                          method.args = list(),
                          contrasts = "Tukey",
                          p.adjust.method = NULL,
                          fit.seed = NA,
                          fm.cutoff.p.value = 1,
                          mc.cutoff.p.value = 1,
                          mc.critical.p.value = 0.05,
                          small.p = getOption("ggpmisc.small.p", default = FALSE),
                          adj.method.tag = 4,
                          p.digits = 3,
                          label.type = "bars",
                          label.y = NULL,
                          vstep = NULL,
                          output.type = NULL,
                          na.rm = FALSE,
                          orientation = "x",
                          parse = NULL,
                          show.legend = FALSE,
                          inherit.aes = TRUE) {
  stopifnot("Flipping with 'orientation = y' is not supported" = orientation == "x")
  if (is.character(contrasts)) {
    stopifnot("Character argument to 'contrasts' should be \"Tukey\" or \"Dunnet\"" =
                all(contrasts %in% c("Tukey", "Dunnet")))
  } else if (is.null(p.adjust.method)) {
    # same default as p.adjust()
    p.adjust.method <- "holm"
  }
  force(geom)
  # dynamic defaults
  if (is.null(geom)) {
    if (label.type == "bars") {
      geom <- "label_pairwise" #"text_pairwise"
    } else if (label.type %in% c("letters", "LETTERS", "numeric")) {
      geom <- "text" # "label"
    } else {
      stop("Unrecognized 'label.type = ", label.type, "'.")
    }
  }

  if (label.type %in% c("letters", "LETTERS") &&
      any(contrasts != "Tukey")) {
    stop("'letters' labels are supported only with \"Tukey\" contrasts.")
  }

  if (grepl("_npc", geom)) {
    geom <- gsub("_npc", "", geom)
    warning("\"npc\"-based geometries not supported, using\"", geom, "\" instead.")
  }

  output.type <-
    check_output_type(output.type = output.type, geom = geom)

  if (is.null(parse)) {
    parse <- output.type == "expression"
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatMultcomp,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params =
      rlang::list2(formula = formula,
                   method = method,
                   method.args = method.args,
                   contrasts = contrasts,
                   p.adjust.method = p.adjust.method,
                   fit.seed = fit.seed,
                   small.p = small.p,
                   adj.method.tag = adj.method.tag,
                   p.digits = p.digits,
                   label.type = label.type,
                   label.y = label.y,
                   fm.cutoff.p.value = fm.cutoff.p.value,
                   mc.cutoff.p.value = mc.cutoff.p.value,
                   mc.critical.p.value = mc.critical.p.value,
                   vstep = vstep,
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
multcomp_compute_fun <-
  function(data,
           scales,
           method = "lm",
           method.args = list(),
           contrasts = "Tukey",
           p.adjust.method = "holm",
           formula = y ~ x,
           weight = 1,
           small.p = FALSE,
           adj.method.tag = 4,
           fit.seed = NA,
           p.digits = 3,
           label.type = "bars",
           fm.cutoff.p.value = 1,
           mc.cutoff.p.value = 1,
           mc.critical.p.value = 0.05,
           label.y = NULL,
           vstep = NULL,
           output.type = expression(),
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

    stopifnot(!any(c("formula", "data") %in% names(method.args)))
    # we guess formula from orientation
    if (is.null(formula)) {
      if (is.na(orientation) || orientation == "x") {
        formula = y ~ factor(x)
      } else if (orientation == "y") {
        formula = x ~ factor(y)
      }
    }
    # we guess orientation from formula
    if (is.na(orientation)) {
      orientation <- unname(c(x = "y", y = "x")[as.character(formula)[2]])
    }

    if (orientation == "x") {
      if (length(unique(data[["x"]])) < 2) {
        return(data.frame())
      }
    } else if (orientation == "y") {
      if (length(unique(data[["y"]])) < 2) {
        return(data.frame())
      }
    }

    num.levels <- length(unique(data[[orientation]]))
    if (length(contrasts) == 1 &&
            (contrasts == "Tukey" && num.levels > 5 && label.type == "bars")) {
      warning("Tukey contrasts with bars support at most five groups, not ",
              num.levels, ". Skipping test")
      return(data.frame())
    }

    # default position of labels has to be set after label.type is known
    if (label.type == "bars") {
      if (is.null(vstep)) {
        vstep <- 0.125
      }
      if (is.null(label.y)) {
        label.y <- "top"
      }
    } else if (label.type %in% c("letters", "LETTERS", "numeric")) {
      if (is.null(vstep)) {
        vstep <- 0
      }
      if (is.null(label.y)) {
        label.y <- "bottom"
      }
    }

    if (nrow(data) < 2 * num.levels) {
      stop("Too few observations per factor level. ",
           "Did you map to ", orientation, " a continuous variable instead of a factor?")
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

    if (is.integer(data[["group"]])) {
      group.idx <- abs(data[["group"]][1])
    } else if (is.character(data[["group"]]) &&
               grepl("^(-1|[0-9]+).*$", data[["group"]][1])) {
      # likely that 'gganimate' has set the groups for scenes
      # we assume first characters give the original group
      group.idx <- abs(as.numeric(gsub("^(-1|[0-9]+).*$", "\\1", data[["group"]][1])))
    } else {
      group.idx <- NA_integer_
    }

    # If method was specified as a character string, replace with
    # the corresponding function. Some model fit functions themselves have a
    # method parameter accepting character strings as argument. We support
    # these by splitting strings passed as argument at a colon.
    if (is.character(method)) {
      method <- switch(method,
                       lm = "lm:qr",
                       aov = "aov",
                       rlm = "rlm:M",
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
                       aov = stats::aov,
                       rlm = MASS::rlm,
                       match.fun(method))
    } else if (is.function(method)) {
      fun.method <- character()
      if (is.name(quote(method))) {
        method.name <- as.character(quote(method))
      } else {
        method.name <- "function"
      }
    }

    fun.args <- list(quote(formula),
                     data = quote(data),
                     weights = data[["weight"]])
    fun.args <- c(fun.args, method.args)
    if (length(fun.method)) {
      fun.args[["method"]] <- fun.method
    }

    if (!is.na(fit.seed)) {
      set.seed(fit.seed)
    }
    # some model fit functions can contain code with partial matching of names!
    # so we silence selectively only these warnings
    withCallingHandlers({
      fm <- do.call(method, args = fun.args)
      fm.class <- class(fm)
      if (fm.class[1] == "aov") {
        fm.class <- fm.class[-1]
        class(fm) <- fm.class
      }
      fm.summary <- summary(fm)
    }, warning = function(w) {
      if (startsWith(conditionMessage(w), "partial match of 'coef'") ||
          startsWith(conditionMessage(w), "partial argument match of 'contrasts'"))
        invokeRestart("muffleWarning")
    })

    # allow model formula selection by the model fit method
    # extract formula from fitted model if possible, but fall back on argument if needed
    formula.ls <- fail_safe_formula(fm, fun.args, verbose = TRUE)

    if ("fstatistic" %in% names(fm.summary)) {
      f.value <- fm.summary[["fstatistic"]]["value"]
      f.df1 <- fm.summary[["fstatistic"]]["numdf"]
      f.df2 <- fm.summary[["fstatistic"]]["dendf"]
      p.value <- stats::pf(q = f.value, f.df1, f.df2, lower.tail = FALSE)
    } else {
      f.value <- f.df1 <- f.df2 <- p.value <- NA_real_
      if (fm.cutoff.p.value < 1) {
        warning("F-value and P-value estimates from fitted model not available.")
        return(data.frame())
      }
    }

    if (!is.na(p.value) && p.value > fm.cutoff.p.value) {
      warning(sprintf("P-value for main effect = %.3f > cutoff = %.3f, skipping tests!",
                      p.value, fm.cutoff.p.value))
      return(data.frame())
    } else if (any(is.na(fm[["coefficients"]][-1]))) {
      warning("Problem with model fitting: NAs returned, skipping tests!")
      return(data.frame())
    }

    n <- length(residuals(fm))

    if (is.numeric(contrasts)) {
      if (is.vector(contrasts)) {
        contrasts <- matrix(contrasts, nrow = 1)
      }
      x.left.tip <- x.right.tip <- numeric(nrow(contrasts))
      for (i in 1:nrow(contrasts)) {
        x.tips <- which(contrasts[i, ] != 0)
        if (length(x.tips) != 2) {
          stop("Only pairwise contrasts are currently supported.")
        }
        x.left.tip[i] <- x.tips[1]
        x.right.tip[i] <- x.tips[2]
      }
    } else if (is.character(contrasts)) {
      if (contrasts == "Tukey") {
        x.left.tip <- switch(num.levels,
                             NA_real_,
                             1,
                             c(1, 1, 2),
                             c(1, 1, 1, 2, 2, 3),
                             c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4)
        )
        x.right.tip <- switch(num.levels,
                              NA_real_,
                              2,
                              c(2, 3, 3),
                              c(2, 3, 4, 3, 4, 4),
                              c(2, 3, 4, 5, 3, 4, 5, 4, 5, 5)
        )
      } else if (contrasts == "Dunnet") {
        x.left.tip <- rep(1, num.levels - 1)
        x.right.tip <- 2:num.levels
      }
    }

    # multiple comparisons test
    if (inherits(contrasts, "mcp")) {
      linfct.arg <- contrasts
      contrasts <- "mcp object"
    } else {
      linfct.arg <- multcomp::mcp(`factor(x)` = contrasts)
    }
    fm.glht <-
      multcomp::glht(model = fm,
                     linfct = linfct.arg,
                     rhs = 0)
    if (is.null(p.adjust.method)) {
      # we increase maxpts based on the number of levels to ensure convergence
      # in mvtnorm::pmvnorm
      summary.fm.glht <-
        summary(fm.glht, test = multcomp::adjusted(maxpts = 4e4 * num.levels))
      # needed for labels
      if (contrasts == "Tukey") {
        p.adjust.method <- "HSD"
      } else if (contrasts == "Dunnet"){
        p.adjust.method <- "Dunnet"
      } else {
        p.adjust.method <- "default"
      }
    } else {
      summary.fm.glht <-
        summary(fm.glht, test = multcomp::adjusted(type = p.adjust.method))
    }

    pairwise.p.values <- summary.fm.glht[["test"]][["pvalues"]]
    pairwise.coefficients <- summary.fm.glht[["test"]][["coefficients"]]
    pairwise.tstat <- summary.fm.glht[["test"]][["tstat"]]
    pairwise.contrasts <- gsub(" ", "", names(pairwise.coefficients))

    adj.method.legend <- TRUE
    if (is.character(adj.method.tag)) {
      adj.label <- adj.method.tag[1]
    } else if (is.function(adj.method.tag)) {
      adj.label <- adj.method.tag(p.adjust.method)
    } else if (adj.method.tag < 0) {
      adj.label <- abbreviate("adjusted", -adj.method.tag)
    } else if (adj.method.tag > 0) {
      if (p.adjust.method == "none") {
        adj.label <- "t"
      } else {
        adj.label <- abbreviate(gsub("-", " ", p.adjust.method, fixed=TRUE),
                                adj.method.tag)
      }
    } else {
      adj.label <- "" # character() broken in ggplot2 >= 4.0.0
      # only "LETTERS" and "letters" generate legends, not "bars"
      adj.method.legend <- FALSE
    }

    if (label.type %in% c("bars")) {
      # Labelled bar representation of multiple contrast results.
      #
      # We build a data frame suitable for plotting with geom_text_pairwise()
      # or geom_label_pairwise(). We return multiple results, but map only
      # some.
      #
      z <- tibble::tibble(x = (x.left.tip + x.right.tip) / 2,
                          x.left.tip,
                          x.right.tip,
                          coefficients = pairwise.coefficients,
                          contrasts = pairwise.contrasts,
                          tstat = pairwise.tstat,
                          p.value = pairwise.p.values,
                          fm.method = method.name,
                          fm.class = fm.class[1],
                          fm.formula = formula.ls,
                          fm.formula.chr = format(formula.ls),
                          mc.adjusted = p.adjust.method,
                          mc.contrast = contrasts,
                          n = n,
                          just = "center")

      # Drop unwanted labels
      z <- z[z[["p.value"]] <= mc.cutoff.p.value, ]

      if (nrow(z) == 0) {
        return(data.frame())
      }

      # build character strings
      stopifnot("'p.digits' must be a positive integer > 0" = p.digits > 0)
      if (p.digits < 2) {
        warning("'p.digits < 2' Likely information loss!")
      }

      if (output.type == "expression") {
        stars.char <- paste("\"", stars_pval(z[["p.value"]]), "\"", sep = "")
      } else {
        stars.char <- stars_pval(z[["p.value"]])
      }

      # Build the labels
      # We use a for loop because sep is not vectorized
      # More elegantly, we could handle this in sprintf_dm()
      if (output.type != "numeric") {
        z[["stars.label"]] <- stars.char
        z[["p.value.label"]] <- NA_character_

        for (i in seq_along(z[["p.value"]])) {
          z[["p.value.label"]][i] <- p_value_label(value = z[["p.value"]][i],
                                                   subscript = adj.label,
                                                   small.p = small.p,
                                                   digits = p.digits,
                                                   output.type = output.type,
                                                   decimal.mark = decimal.mark)
          z[["delta.label"]][i] <- italic_label(value = z[["coefficients"]][i],
                                                value.name = "Delta",
                                                digits = 3,
                                                fixed = FALSE,
                                                output.type = output.type,
                                                decimal.mark = decimal.mark)
          z[["t.value.label"]][i] <- t_value_label(value = z[["tstat"]][i],
                                                   digits = p.digits,
                                                   output.type = output.type,
                                                   decimal.mark = decimal.mark)
        }
      }

      if (output.type == "numeric") {
        z[["default.label"]] <- NA_character_
      } else {
        z[["default.label"]] <- z[["p.value.label"]]
      }

    } else if (label.type %in% c("letters", "LETTERS")) {
      # Letters encoding of multiple contrast results.
      #
      # We build a data frame suitable for plotting with geom_text()
      # or geom_label()
      #
      names(pairwise.p.values) <- pairwise.contrasts
      Letters <-
        multcompView::multcompLetters(x = pairwise.p.values,
                                      threshold = mc.critical.p.value,
                                      Letters = get(label.type),
                                      reversed = TRUE)[["Letters"]]
      if (adj.method.legend) {
        p.crit.label <- p_value_label(value = mc.critical.p.value,
                                      subscript = adj.label,
                                      superscript = "crit",
                                      small.p = small.p,
                                      digits = p.digits,
                                      output.type = output.type,
                                      decimal.mark = decimal.mark)
        z <- tibble::tibble(x = c(0.1, 1:num.levels),
                            x.left.tip = NA_real_,
                            x.right.tip = NA_real_,
                            critical.p.value = mc.critical.p.value,
                            fm.method = method.name,
                            fm.class = fm.class,
                            fm.formula = formula.ls,
                            fm.formula.chr = format(formula.ls),
                            mc.adjusted = p.adjust.method,
                            mc.contrast = contrasts,
                            n = n,
                            letters.label = c(p.crit.label ,
                                              Letters[order(as.numeric(names(Letters)))]),
                            just = c("inward", rep("center", length(Letters))))
      } else {
        z <- tibble::tibble(x = 1:num.levels,
                            x.left.tip = NA_real_,
                            x.right.tip = NA_real_,
                            critical.p.value = mc.critical.p.value,
                            fm.method = method.name,
                            fm.class = fm.class,
                            fm.formula = formula.ls,
                            fm.formula.chr = format(formula.ls),
                            mc.adjusted = p.adjust.method,
                            mc.contrast = contrasts,
                            n = n,
                            letters.label = Letters[order(as.numeric(names(Letters)))],
                            just = rep("center", length(Letters)))
      }

      if (output.type == "numeric") {
        z[["default.label"]] <- NA_character_
      } else {
        z[["default.label"]] <- z[["letters.label"]]
      }
    }

    y.range <- scales$y$range$range

    if (is.character(label.y)) {
      # we need to use scale limits as observations are not necessarily plotted
      if (!label.y %in% c("top", "bottom")) {
        warning("'label.y' must be one of \"top\", \"bottom\", or numeric, not: \"", label.y, "\"")
        label.y <- "top"
      }
      if (label.y == "top") {
        if (vstep == 0) {
          z[["y"]] <- y.range[2] + (y.range[2] - y.range[1]) * 0.08
        } else {
          z[["y"]] <- y.range[2] + (y.range[2] - y.range[1]) * vstep * seq_along(z[["x"]])
        }
      } else {
        if (vstep == 0) {
          z[["y"]] <- y.range[1] - (y.range[2] - y.range[1]) * 0.08
        } else {
          z[["y"]] <- y.range[1] - (y.range[2] - y.range[1]) * vstep * seq_along(z[["x"]])
        }
      }
    } else if (is.numeric(label.y)) {
      # manual locations
      if (label.type == "bars" && length(label.y) == 1) {
        z[["y"]] <- label.y + (y.range[2] - y.range[1]) * vstep *
          (seq_along(z[["x"]]) - 1) * sign(label.y - 0.5 * (y.range[2] + y.range[1]))
      } else {
        z[["y"]] <- rep_len(label.y, nrow(z))
      }
    }

    z
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatMultcomp <-
  ggplot2::ggproto("StatMultcomp", ggplot2::Stat,
                   extra_params = c("na.rm", "parse"),
                   compute_panel = multcomp_compute_fun,
                   default_aes = ggplot2::aes(xmin = after_stat(x.left.tip),
                                              xmax = after_stat(x.right.tip),
                                              label = after_stat(default.label),
                                              weight = 1,
                                              size = 2.5,
                                              hjust = after_stat(just)),
                   dropped_aes = "weight",
                   required_aes = c("x", "y"),
  )
