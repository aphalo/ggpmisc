#' Labels for multiple comparisons
#'
#' \code{stat_multcomp} fits a linear model by default with \code{stats::lm()}
#' but alternatively using other model fit functions. The model is passed to
#' function \code{glht()} from package 'multcomp' to fit Tukey or Dunnet
#' contrasts and generates labels for \emph{P}-values.
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
#' @param method function or character If character, "lm", "rlm" or the name of
#'   a model fit function are accepted, possibly followed by the fit function's
#'   \code{method} argument separated by a colon (e.g. \code{"rlm:M"}). If a
#'   function different to \code{lm()}, it must accept as a minimum a model
#'   formula through its first parameter, and have formal parameters named
#'   \code{data}, \code{weights}, and \code{method}, and return a model fit
#'   object accepted by function \code{glht()}.
#' @param method.args named list with additional arguments.
#' @param contrast.type character One of "Tukey" or "Dunnet".
#' @param small.p logical Flags to switch use of lower case p for
#'   p-value.
#' @param p.digits integer Number of digits after the decimal point to
#'   use for \eqn{R^2} and P-value in labels.
#' @param label.type character One of "bars", "letters" or "LETTERS", selects
#'   how the results of the multiple comparisons are displayed.
#' @param main.cutoff.p.value numeric [0..1] The P-value for the main effect of
#'   factor \code{x} in the ANOVA test above which no pairwise comparisons are
#'   computed or labels generated.
#' @param test.cutoff.p.value numeric [0..1] The P-value for the individual
#'   contrasts above which no labelled bar is generated.
#' @param letters.p.value numeric The critical P-value used for tests when
#'   when encoded as letters.
#' @param label.y numeric in native data units or \code{character}.
#'   If too short they will be recycled.
#' @param vstep numeric in native data units, the vertical step
#'   used between labels for comparisons.
#' @param output.type character One of "expression", "LaTeX", "text",
#'   "markdown" or "numeric".
#' @param orientation character Either "x" or "y" controlling the default for
#'   \code{formula}.
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
#'   \emph{P}-values for multiple comparison tests, based on Tukey contrasts
#'   (all pairwise) or Dunnet contrasts (other levels against the first one).
#'   The explanatory variable must be a factor as this creates the required
#'   grouping.
#'
#'   This statistic generates labels as R expressions by default but LaTeX (use
#'   TikZ device), markdown (use package 'ggtext') and plain text are also
#'   supported, as well as numeric values for user-generated text labels. The
#'   value of \code{parse} is set automatically based on \code{output-type}, but
#'   if you assemble labels that need parsing from \code{numeric} output, the
#'   default needs to be overridden. This statistic only generates annotation
#'   labels and segments connecting the compared factor levels, or letter labels
#'   that discriminate significantly different groups.
#'
#'   A ggplot statistic receives as \code{data} a data frame that is not the one
#'   passed as argument by the user, but instead a data frame with the variables
#'   mapped to aesthetics. Consequently, the model formula must be based on
#'   \code{x} and \code{y} variables, not the original names of the variables
#'   mapped to these aesthetics.
#'
#' @section Aesthetics: \code{stat_multcomp()} understands \code{x} and
#'   \code{y}, to be referenced in the \code{formula} and \code{weight} passed
#'   as argument to parameter \code{weights}. A factor must be mapped to
#'   \code{x} and \code{numeric} variables to \code{y}, and if used to
#'   \code{weight}. In addition, the aesthetics understood by the geom
#'   (\code{"text_pairwise"} is the default for \code{label.type = "bars"},
#'   \code{"text"} is the default for \code{label.type = "letters"} and for
#'   \code{label.type = "LETTERS"}) are understood and grouping
#'   respected.
#'
#' @return A data frame with one row per comparison for \code{label.type = "bars"}, or
#'   a data frame with one row per factor \code{x} level for
#'   \code{label.type = "letters"} and for
#'   \code{label.type = "LETTERS"}. Columns as described under
#'   \strong{Computed variables}.
#'
#' @section Computed variables:
#' If output.type different from \code{"numeric"} the returned tibble contains
#' columns listed below. If the model fit function used does not return a value,
#' the label is set to \code{character(0L)}.
#' \describe{
#'   \item{x,xmin,xmax}{x position.}
#'   \item{y}{y position.}
#'   \item{default.label}{text label.}
#'   \item{p.value.label}{P-value.}
#'   \item{method.label}{Set according \code{method} used.}
#'   \item{p.value, coefficients, contrasts, tstat}{numeric values, from glht object}}
#'
#' If output.type is \code{"numeric"} the returned data frame lacks the labels.
#'
#' To explore the computed values returned for a given input we suggest the use
#' of \code{\link[gginnards]{geom_debug}} as shown in the last examples below.
#'
#' @section Alternatives: \code{stat_signif()} in package 'ggsignif' is
#'   an earlier and independent implementation of pairwise tests.
#'
#' @seealso This statistic uses the implementation of Tests of General Linear
#'   Hypotheses in function \code{\link[multcomp]{glht}}.
#'
#' @family ggplot statistics for linear and polynomial regression
#'
#' @examples
#'
#' my.cars <- mtcars
#' my.cars$cyl <- factor(my.cars$cyl)
#'
#' p1 <- ggplot(my.cars, aes(cyl, mpg)) +
#'   geom_boxplot(width = 0.33)
#'
#' ## labeleld bars
#'
#' p1 +
#'   stat_multcomp()
#'
#' p1 +
#'   stat_multcomp(contrast.type = "Dunnet")
#'
#' p1 +
#'   stat_multcomp(geom = "text_pairwise")
#'
#' p1 +
#'   stat_multcomp(label.y = "top")
#'
#' p1 +
#'   stat_multcomp(label.y = "bottom")
#'
#' p1 +
#'   stat_multcomp(label.y = 12)
#'
#' # use different labels: difference and P-value from hypothesis tests
#' p1 +
#'   stat_multcomp(use_label(c("Delta", "P")), size = 2.75)
#'
#' # use different labels: P-values encoded as stars
#' p1 +
#'   stat_multcomp(use_label("stars"))
#'
#' # control smallest value displayed and number of digits
#' p1 +
#'   stat_multcomp(p.digits = 4)
#'
#' # use colour to show significance
#' p1 +
#'   stat_multcomp(aes(colour = after_stat(p.value) < 0.01),
#'                 size = 2.75) +
#'   scale_colour_manual(values = c("grey60", "black")) +
#'   theme_bw()
#'
#' # add arrow heads to segments and use fill to show significance
#' p1 +
#'   stat_multcomp(aes(fill = after_stat(p.value) < 0.01),
#'                 arrow = grid::arrow(angle = 90,
#'                                     length = unit(1, "mm"),
#'                                     ends = "both")) +
#'   scale_fill_manual(values = c("white", "green"))
#'
#' # manual positioning of bars (= segments)
#' p1 +
#'   stat_multcomp(label.y = c(1, 3, 5)) +
#'   expand_limits(y = 0)
#'
#' p1 +
#'   stat_multcomp(label.y = c(3, 1, 5)) +
#'   expand_limits(y = 0)
#'
#' # label only significant differences
#' p1 +
#'   stat_multcomp(test.cutoff.p.value = 0.01)
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
#'   stat_multcomp(label.type = "letters", label.y = "top")
#'
#' p1 +
#'   stat_multcomp(label.type = "letters", label.y = 36)
#'
#' medians <-
#'   aggregate(my.cars$mpg, by = list(my.cars$cyl), FUN = median)[["x"]]
#' p1 +
#'   stat_multcomp(label.type = "letters",
#'                 label.y = medians,
#'                 position = position_nudge(x = 0.22))
#'
#' # geometry
#' p1 +
#'   stat_multcomp(label.type = "letters", geom = "label")
#'
#' # stricter critical p-value than default used for test
#' p1 +
#'   stat_multcomp(label.type = "letters", letters.p.value = 0.01)
#'
#' @export
#'
stat_multcomp <- function(mapping = NULL, data = NULL,
                          geom = NULL,
                          position = "identity",
                          ...,
                          formula = NULL,
                          method = "lm",
                          method.args = list(),
                          contrast.type = "Tukey",
                          small.p = FALSE,
                          p.digits = 3,
                          label.type = "bars",
                          main.cutoff.p.value = 0.05,
                          test.cutoff.p.value = 1.0,
                          letters.p.value = 0.05,
                          label.y = NULL,
                          vstep = NULL,
                          output.type = NULL,
                          na.rm = FALSE,
                          orientation = NA,
                          parse = NULL,
                          show.legend = FALSE,
                          inherit.aes = TRUE) {
  stopifnot(contrast.type %in% c("Tukey", "Dunnet"))
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

  if (grepl("_npc", geom)) {
    geom <- gsub("_npc", "", geom)
    warning("\"npc\"-based geometries not supported, using\"", geom, "\" instead.")
  }

  if (label.type == "bars") {
    if (is.null(vstep)) {
      vstep <- 0.08
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
                   contrast.type = contrast.type,
                   small.p = small.p,
                   p.digits = p.digits,
                   label.type = label.type,
                   label.y = label.y,
                   main.cutoff.p.value = main.cutoff.p.value,
                   test.cutoff.p.value = test.cutoff.p.value,
                   letters.p.value = letters.p.value,
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
           method,
           method.args,
           contrast.type,
           formula,
           weight,
           small.p,
           p.digits,
           label.type,
           main.cutoff.p.value,
           test.cutoff.p.value,
           letters.p.value,
           label.y,
           vstep,
           output.type,
           na.rm,
           orientation) {
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
    if ((num.levels > 5 && label.type == "bars") || nrow(data) < 2 * num.levels) {
      stop("Maximum number of factor levels supported is 5, each one with replicate observations. ",
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

    # some model fit functions can contain code with partial matching of names!
    # so we silence selectively only these warnings
    withCallingHandlers({
      fm <- do.call(method, args = fun.args)
      fm.summary <- summary(fm)
    }, warning = function(w) {
      if (startsWith(conditionMessage(w), "partial match of 'coef'") ||
          startsWith(conditionMessage(w), "partial argument match of 'contrasts'"))
        invokeRestart("muffleWarning")
    })
    fm.class <- class(fm)

    if ("fstatistic" %in% names(fm.summary)) {
      f.value <- fm.summary[["fstatistic"]]["value"]
      f.df1 <- fm.summary[["fstatistic"]]["numdf"]
      f.df2 <- fm.summary[["fstatistic"]]["dendf"]
      p.value <- stats::pf(q = f.value, f.df1, f.df2, lower.tail = FALSE)
      if (p.value > main.cutoff.p.value) {
        warning(sprintf("P-value for main effect = %.3f > cutoff = %.3f, skipping tests!",
                        p.value, main.cutoff.p.value))
        return(data.frame())
      }
    } else {
      warning("Fitting of ", method, " model may have failed.")
      if (main.cutoff.p.value < 1) {
        return(data.frame())
      }
    }
    n <- length(residuals(fm))

    # multiple comparisons test
    fm.glht <- multcomp::glht(model = fm,
                              linfct = multcomp::mcp(`factor(x)` = contrast.type),
                              rhs = 0)
    summary.fm.glht <- summary(fm.glht)

    if (contrast.type == "Tukey") {
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
    } else if (contrast.type == "Dunnet") {
      x.left.tip <- rep(1, num.levels - 1)
      x.right.tip <- 2:num.levels
    }

    pairwise.p.values <- summary.fm.glht[["test"]][["pvalues"]]
    pairwise.coefficients <- summary.fm.glht[["test"]][["coefficients"]]
    pairwise.tstat <- summary.fm.glht[["test"]][["tstat"]]
    pairwise.contrasts <- gsub(" ", "", names(pairwise.coefficients))

    if (label.type %in% c("bars")) {
      # Labelled bar representation of multiple contrast results.
      #
      # We build a data frame suitable for plotting with geom_text_pairwise()
      # or geom_label_pairwise(). We return multiple results, but map only
      # some.
      #
      z <- data.frame(x = (x.left.tip + x.right.tip) / 2,
                      x.left.tip,
                      x.right.tip,
                      coefficients = pairwise.coefficients,
                      contrasts = pairwise.contrasts,
                      tstat = pairwise.tstat,
                      p.value = pairwise.p.values,
                      method = method.name,
                      n = n
      )

      # Drop labels
      z <- z[z[["p.value"]] <= test.cutoff.p.value, ]

      if (nrow(z) == 0) {
        return(data.frame())
      }

      # build character strings
      stopifnot(p.digits > 0)
      if (p.digits < 2) {
        warning("'p.digits < 2' Likely information loss!")
      }

      if (output.type == "expression") {
        p.value.char <- sprintf_dm("\"%#.*f\"", p.digits, z[["p.value"]], decimal.mark = decimal.mark)
        stars.char <- paste("\"", stars_pval(z[["p.value"]]), "\"", sep = "")
        coefficients.char <- sprintf_dm("\"%#.*g\"", 3L, z[["coefficients"]], decimal.mark = decimal.mark)
        tstat.char <- sprintf_dm("\"%#.*g\"", 3L, z[["tstat"]], decimal.mark = decimal.mark)
      } else {
        p.value.char <- sprintf_dm("%#.*f", p.digits, z[["p.value"]], decimal.mark = decimal.mark)
        stars.char <- stars_pval(z[["p.value"]])
        coefficients.char <- sprintf_dm("%#.*g", 3L, z[["coefficients"]], decimal.mark = decimal.mark)
        tstat.char <- sprintf_dm("%#.*g", 3L, z[["tstat"]], decimal.mark = decimal.mark)
      }

      # Build the labels
      # We use a for loop because sep is not vectorized
      # More elegantly, we could handle this in sprintf_dm()
      z[["stars.label"]] <- stars.char
      z[["p.value.label"]] <- NA_character_
      if (output.type == "expression") {
        for (i in seq_along(z[["p.value"]])) {
          z[["p.value.label"]][i] <-
            ifelse(is.na(z[["p.value"]][i]) || is.nan(z[["p.value"]][i]), "",
                   paste(ifelse(small.p, "italic(p)",  "italic(P)"),
                         ifelse(z[["p.value"]][i] < 10^(-p.digits),
                                sprintf_dm("\"%.*f\"", p.digits, 10^(-p.digits), decimal.mark = decimal.mark),
                                p.value.char[i]),
                         sep = ifelse(z[["p.value"]][i] < 10^(-p.digits),
                                      "~`<`~",
                                      "~`=`~")))
        }
        z[["delta.label"]] <- paste("Delta~`=`~", coefficients.char, sep = "")
        z[["t.label"]] <- paste("italic(t)~`=`~", tstat.char, sep = "")
      } else if (output.type %in% c("latex", "tex", "text", "tikz")) {
        for (i in seq_along(z[["p.value"]])) {
          z[["p.value.label"]][i] <-
            ifelse(is.na(z[["p.value"]][i]) || is.nan(z[["p.value"]][i]), "",
                   paste(ifelse(small.p, "p",  "P"),
                         ifelse(z[["p.value"]][i] < 10^(-p.digits), as.character(10^(-p.digits)), p.value.char[i]),
                         sep = ifelse(z[["p.value"]][i] < 10^(-p.digits), " < ", " = ")))
        }
        z[["delta.label"]] <- paste("\\Delta = ", coefficients.char, sep = "")
        z[["t.label"]] <- paste("t = ", tstat.char, sep = "")
      } else if (output.type == "markdown") {
        for (i in seq_along(z[["p.value"]])) {
          z[["p.value.label"]][i] <-
            ifelse(is.na(z[["p.value"]][i]) | is.nan(z[["p.value"]][i]), "",
                   paste(ifelse(small.p, "_p_", "_P_"),
                         ifelse(z[["p.value"]][i] < 10^(-p.digits), as.character(10^(-p.digits)), p.value.char[i]),
                         sep = ifelse(z[["p.value"]][i] < 10^(-p.digits), " < ", " = ")))
        }
        z[["delta.label"]] <- paste("&Delta; = ", coefficients.char, sep = "")
        z[["t.label"]] <- paste("_t_ = ", tstat.char, sep = "")
      } else {
        if (output.type != "numeric") {
          warning("Unknown 'output.type' argument: ", output.type)
        }
        z[["p.value.label"]] <- NA_character_
      }
      z[["default.label"]] <- z[["p.value.label"]]
    } else if (label.type %in% c("letters", "LETTERS")) {
      # Letters encoding of multiple contrast results.
      #
      # We build a data frame suitable for plotting with geom_text()
      # or geom_label().
      #
      names(pairwise.p.values) <- pairwise.contrasts
      letters <-
        multcompView::multcompLetters(x = pairwise.p.values,
                                      threshold = letters.p.value,
                                      Letters = get(label.type),
                                      reversed = TRUE)[["Letters"]]

      z <- data.frame(x = 1:num.levels,
                      x.left.tip = NA_real_,
                      x.right.tip = NA_real_,
                      critical.p.value = letters.p.value,
                      method = method.name,
                      n = n,
                      letters = letters[order(names(letters))])
      z[["default.label"]] <- z[["letters"]]
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
        z[["y"]] <- label.y + (y.range[2] - y.range[1]) * vstep * seq_along(z[["x"]]) * sign(0.5 - label.y)
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
                                              weight = 1),
                   dropped_aes = "weight",
                   required_aes = c("x", "y"),
  )
