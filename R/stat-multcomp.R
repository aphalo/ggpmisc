#' Multiple comparisons
#'
#' \code{stat_multcomp} fits a linear model by default with \code{stats::lm()} but
#' alternatively using other model fit functions. The model is passed to
#' function \code{glht()} from package 'multcomp' to fit Tukey contrasts and generates
#' labels for \emph{P}-values.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset, only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data.
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
#' @param max.main.p.value numeric [0..1] The P-value for the main effect of _x_
#'   in ANOVA test above which no pairwise comparisons are run or labels
#'   generated.
#' @param small.p logical Flags to switch use of lower case p for
#'   p-value.
#' @param p.digits integer Number of digits after the decimal point to
#'   use for \eqn{R^2} and P-value in labels.
#' @param label.y \code{numeric} in native data units. If too short they will
#'   be recycled.
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
#'   \emph{P}-values for multiple comparison tests. The explanatory variable
#'   must be a factor as this creates a grouping. This statistic
#'   generates labels as R expressions by default but LaTeX (use TikZ device),
#'   markdown (use package 'ggtext') and plain text are also supported, as well
#'   as numeric values for user-generated text labels. The value of \code{parse}
#'   is set automatically based on \code{output-type}, but if you assemble
#'   labels that need parsing from \code{numeric} output, the default needs to
#'   be overridden. This stat only generates annotation labels and segments
#'   connecting the compared factor levels.
#'
#'   A ggplot statistic receives as \code{data} a data frame that is not the one
#'   passed as argument by the user, but instead a data frame with the variables
#'   mapped to aesthetics.
#'
#' @section Aesthetics: \code{stat_multcomp()} understands \code{x} and
#'   \code{y}, to be referenced in the \code{formula} and \code{weight} passed
#'   as argument to parameter \code{weights}. A factor must be mapped to
#'   \code{x} and \code{numeric} variables to \code{y}, and if used to
#'   \code{weight}. In addition, the aesthetics understood by the geom
#'   (\code{"text_pairwuse"} is the default) are understood and grouping
#'   respected.
#'
#' @return A data frame, with one row per comparison and columns as described
#'   under \strong{Computed variables}.
#'
#' @section Computed variables:
#' If output.type different from \code{"numeric"} the returned tibble contains
#' columns listed below. If the model fit function used does not return a value,
#' the label is set to \code{character(0L)}.
#' \describe{
#'   \item{xmin,xmax}{x position}
#'   \item{y}{y position}
#'   \item{p.value.label}{P-value for the F-value above.}
#'   \item{method.label}{Set according \code{method} used.}
#'   \item{p.value, n}{numeric values, from glht object}}
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
#' @export
#'
stat_multcomp <- function(mapping = NULL, data = NULL,
                          geom = "text_pairwise",
                          position = "identity",
                          ...,
                          formula = NULL,
                          method = "lm",
                          method.args = list(),
                          max.main.p.value = 0.05,
                          small.p = FALSE,
                          p.digits = 3,
                          label.y = NULL,
                          vstep = 0,
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
                   max.main.p.value = max.main.p.value,
                   small.p = small.p,
                   p.digits = p.digits,
                   label.y = label.y,
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
           max.main.p.value,
           formula,
           weight,
           small.p,
           p.digits,
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
      formula = y ~ x
    } else if (orientation == "y") {
      formula = x ~ y
    }
  }
  # we guess orientation from formula
  if (is.na(orientation)) {
    orientation <- unname(c(x = "y", y = "x")[as.character(formula)[2]])
  }

  if (orientation == "x") {
    if (length(unique(data$x)) < 2) {
      return(data.frame())
    }
  } else if (orientation == "y") {
    if (length(unique(data$y)) < 2) {
      return(data.frame())
    }
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
    if (p.value > max.main.p.value) {
      return(data.frame())
    }
  } else {
    f.value <- f.df1 <- f.df2 <- p.value <- NA_real_
    if (max.main.p.value < 1) {
      return(data.frame())
    }
  }
  n <- residuals(fm)

  # multiple comparisons test
  fm.glht <- multcomp::glht(model = fm,
                            linfct = multcomp::mcp(x = "Tukey"),
                            rhs = 0)
  summary.fm.glht <- summary(fm.glht)

  pairwise.p.values <- summary.fm.glht[["test"]][["pvalues"]]
  z <- data.frame(p.value = pairwise.p.values,
                  xmin = 1,
                  xmax = 2,
                  x = 1.5)

  if (output.type != "numeric") {
    # set defaults needed to assemble the equation as a character string

    # build the other character strings
    stopifnot(p.digits > 0)
    if (p.digits < 2) {
      warning("'p.digits < 2' Likely information loss!")
    }

    if (output.type == "expression") {
      p.value.char <- sprintf_dm("\"%#.*f\"", p.digits, p.value, decimal.mark = decimal.mark)
    } else {
      p.value.char <- sprintf_dm("%#.*f", p.digits, p.value, decimal.mark = decimal.mark)
    }

    # build the data frames to return
    if (output.type == "expression") {
      z[["p.value.label"]] <-
        ifelse(is.na(p.value) || is.nan(p.value), character(0L),
               paste(ifelse(small.p, "italic(p)",  "italic(P)"),
                     ifelse(p.value < 10^(-p.digits),
                            sprintf_dm("\"%.*f\"", p.digits, 10^(-p.digits), decimal.mark = decimal.mark),
                            p.value.char),
                     sep = ifelse(p.value < 10^(-p.digits),
                                  "~`<`~",
                                  "~`=`~")))
    } else if (output.type %in% c("latex", "tex", "text", "tikz")) {
      z[["p.value.label"]] <-
        ifelse(is.na(p.value), character(0L),
               paste(ifelse(small.p, "p",  "P"),
                     ifelse(p.value < 10^(-p.digits), as.character(10^(-p.digits)), p.value.char),
                     sep = ifelse(p.value < 10^(-p.digits), " < ", " = ")))
    } else if (output.type == "markdown") {
      z[["p.value.label"]] <-
                            ifelse(is.na(p.value) || is.nan(p.value), character(0L),
                                   paste(ifelse(small.p, "_p_", "_P_"),
                                         ifelse(p.value < 10^(-p.digits), as.character(10^(-p.digits)), p.value.char),
                                         sep = ifelse(p.value < 10^(-p.digits), " < ", " = ")))
    } else {
      warning("Unknown 'output.type' argument: ", output.type)
    }
  }

  if (is.character(label.y)) {
    z$y <- ggpp::compute_npcy(y = label.y, group = group.idx, v.step = vstep,
                              margin.npc = 0)
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
                   default_aes =
                     ggplot2::aes(xmin = after_stat(xmin),
                                  xmax = after_stat(xmax),
                                  label = after_stat(p.value.label),
                                  weight = 1),
                   dropped_aes = "weight",
                   required_aes = c("x", "y"),
  )
