#' @title Annotate plot with correlation test
#'
#' @description \code{stat_correlation()} applies \code{stats::cor.test()}
#'   respecting grouping with \code{method = "pearson"} default but
#'   alternatively using \code{"kendall"} or \code{"spearman"} methods. It
#'   generates labels for correlation coefficients and p-value, coefficient of
#'   determination (R^2) for method "pearson" and number of observations.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset, only needed if you want to override the
#'   plot defaults.
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
#' @param method character One of "pearson", "kendall" or "spearman".
#' @param n.min integer Minimum number of distinct values in the variables for
#'   fitting to the attempted.
#' @param alternative character One of "two.sided", "less" or "greater".
#' @param exact logical Whether an exact p-value should be computed. Used for
#'   Kendall's tau and Spearman's rho.
#' @param r.conf.level numeric Confidence level for the returned confidence
#'   interval. If set to \code{NA} computation of CI is skipped.
#' @param continuity logical If TRUE , a continuity correction is used for
#'   Kendall's tau and Spearman's rho when not computed exactly.
#' @param small.r,small.p logical Flags to switch use of lower case r and p for
#'   coefficient of correlation (only for \code{method = "pearson"}) and
#'   p-value.
#' @param coef.keep.zeros logical Keep or drop trailing zeros when formatting
#'   the correlation coefficients and t-value, z-value or S-value (see note
#'   below).
#' @param r.digits,t.digits,p.digits integer Number of digits after the decimal
#'   point to use for R, r.squared, tau or rho and P-value in labels. If
#'   \code{Inf}, use exponential notation with three decimal places.
#' @param CI.brackets character vector of length 2. The opening and closing
#'   brackets used for the CI label.
#' @param label.x,label.y \code{numeric} with range 0..1 "normalized parent
#'   coordinates" (npc units) or character if using \code{geom_text_npc()} or
#'   \code{geom_label_npc()}. If using \code{geom_text()} or \code{geom_label()}
#'   numeric in native data units. If too short they will be recycled.
#' @param hstep,vstep numeric in npc units, the horizontal and vertical
#'   displacement step-size used between labels for different groups.
#' @param output.type character One of "expression", "LaTeX", "text", "markdown"
#'   or "numeric".
#' @param boot.R interger The number of bootstrap resamples. Set to zero for no
#'   bootstrap estimates for the CI.
#' @param parse logical Passed to the geom. If \code{TRUE}, the labels will be
#'   parsed into expressions and displayed as described in \code{?plotmath}.
#'   Default is \code{TRUE} if \code{output.type = "expression"} and
#'   \code{FALSE} otherwise.
#'
#' @details This statistic can be used to annotate a plot with the correlation
#'   coefficient and the outcome of its test of significance. It supports
#'   Pearson, Kendall and Spearman methods to compute correlation. This
#'   statistic generates labels as R expressions by default but LaTeX (use TikZ
#'   device), markdown (use package 'ggtext') and plain text are also supported,
#'   as well as numeric values for user-generated text labels. The character
#'   labels include the symbol describing the quantity together with the numeric
#'   value. For the confidence interval (CI) the default is to follow the APA
#'   recommendation of using square brackets.
#'
#'   The value of \code{parse} is set automatically based on \code{output-type},
#'   but if you assemble labels that need parsing from \code{numeric} output,
#'   the default needs to be overridden. By default the value of
#'   \code{output.type} is guessed from the name of the geometry.
#'
#'   A ggplot statistic receives as \code{data} a data frame that is not the one
#'   passed as argument by the user, but instead a data frame with the variables
#'   mapped to aesthetics. \code{cor.test()} is always applied to the variables
#'   mapped to the \code{x} and \code{y} aesthetics, so the scales used for
#'   \code{x} and \code{y} should both be continuous scales rather than
#'   discrete.
#'
#' @section Aesthetics: \code{stat_correaltion()} requires \code{x} and
#'   \code{y}. In addition, the aesthetics understood by the geom
#'   (\code{"text"} is the default) are understood and grouping respected.
#'
#' @section Computed variables: If output.type is \code{"numeric"} the returned
#'   tibble contains the columns listed below with variations depending on the
#'   \code{method}. If the model fit function used does not return a value, the
#'   variable is set to \code{NA_real_}.
#' \describe{
#'   \item{x,npcx}{x position}
#'   \item{y,npcy}{y position}
#'   \item{r, and cor, tau or rho}{numeric values for correlation coefficient estimates}
#'   \item{t.value and its df, z.value or S.value }{numeric values for statistic estimates}
#'   \item{p.value, n}{numeric values.}
#'   \item{r.conf.level}{numeric value, as fraction of one.}
#'   \item{r.confint.low}{Confidence interval limit for \code{r}.}
#'   \item{r.confint.high}{Confidence interval limit for \code{r}.}
#'   \item{grp.label}{Set according to mapping in \code{aes}.}
#'   \item{method.label}{Set according \code{method} used.}
#'   \item{method, test}{character values}}
#'
#' If output.type different from \code{"numeric"} the returned tibble contains
#' in addition to the columns listed above those listed below. If the numeric
#' value is missing the label is set to \code{character(0L)}.
#'
#' \describe{
#'   \item{r.label, and cor.label, tau.label or rho.label}{Correlation coefficient as a character string.}
#'   \item{t.value.label, z.value.label or S.value.label}{t-value and degrees of freedom, z-value or S-value as a character string.}
#'   \item{p.value.label}{P-value for test against zero, as a character string.}
#'   \item{r.confint.label, and cor.conint.label, tau.confint.label or rho.confint.label}{Confidence interval for \code{r} (only with \code{method = "pearson"}).}
#'   \item{n.label}{Number of observations used in the fit, as a character string.}
#'   \item{grp.label}{Set according to mapping in \code{aes}, as a character string.}}
#'
#' To explore the computed values returned for a given input we suggest the use
#' of \code{\link[gginnards]{geom_debug}} as shown in the last examples below.
#'
#' @note Currently \code{coef.keep.zeros} is ignored, with trailing zeros always
#'   retained in the labels but not protected from being dropped by R when
#'   character strings are parsed into expressions.
#'
#' @seealso \code{\link[stats]{cor.test}} for details on the computations.
#'
#' @examples
#' # generate artificial data
#' set.seed(4321)
#' x <- (1:100) / 10
#' y <- x + rnorm(length(x))
#' my.data <- data.frame(x = x,
#'                       y = y,
#'                       y.desc = - y,
#'                       group = c("A", "B"))
#'
#' # by default only R is displayed
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_correlation()
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_correlation(small.r = TRUE)
#'
#' ggplot(my.data, aes(x, y.desc)) +
#'   geom_point() +
#'   stat_correlation(label.x = "right")
#'
#' # non-default methods
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_correlation(method = "kendall")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_correlation(method = "spearman")
#'
#' # use_label() can map a user selected label
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_correlation(use_label("R2"))
#'
#' # use_label() can assemble and map a combined label
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_correlation(use_label("R", "P", "n", "method"))
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_correlation(use_label("R", "R.CI"))
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_correlation(use_label("R", "R.CI"),
#'                    r.conf.level = 0.95)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_correlation(use_label("R", "R.CI"),
#'                    method = "kendall",
#'                    r.conf.level = 0.95)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_correlation(use_label("R", "R.CI"),
#'                    method = "spearman",
#'                    r.conf.level = 0.95)
#'
#' # manually assemble and map a specific label using paste() and aes()
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_correlation(aes(label = paste(after_stat(r.label),
#'                                      after_stat(p.value.label),
#'                                      after_stat(n.label),
#'                                      sep = "*\", \"*")))
#'
#' # manually format and map a specific label using sprintf() and aes()
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_correlation(aes(label = sprintf("%s*\" with \"*%s*\" for \"*%s",
#'                                        after_stat(r.label),
#'                                        after_stat(p.value.label),
#'                                        after_stat(t.value.label))))
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
#' # the whole of computed data
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_correlation(geom = "debug")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_correlation(geom = "debug", method = "pearson")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_correlation(geom = "debug", method = "kendall")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_correlation(geom = "debug", method = "spearman")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_correlation(geom = "debug", output.type = "numeric")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_correlation(geom = "debug", output.type = "markdown")
#'
#' if (gginnards.installed)
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_correlation(geom = "debug", output.type = "LaTeX")
#'
#' @family ggplot statistics for correlation.
#'
#' @export
#'
stat_correlation <-
  function(mapping = NULL,
           data = NULL,
           geom = "text_npc",
           position = "identity",
           ...,
           method = "pearson",
           n.min = 2L,
           alternative = "two.sided",
           exact = NULL,
           r.conf.level =
             ifelse(method == "pearson", 0.95, NA),
           continuity = FALSE,
           small.r = getOption("ggpmisc.small.r", default = FALSE),
           small.p = getOption("ggpmisc.small.p", default = FALSE),
           coef.keep.zeros = TRUE,
           r.digits = 2,
           t.digits = 3,
           p.digits = 3,
           CI.brackets = c("[", "]"),
           label.x = "left",
           label.y = "top",
           hstep = 0,
           vstep = NULL,
           output.type = NULL,
           boot.R =
             ifelse(method == "pearson", 0, 999),
           na.rm = FALSE,
           parse = NULL,
           show.legend = FALSE,
           inherit.aes = TRUE) {
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
    if (is.null(r.conf.level) || !is.finite(r.conf.level)) {
      r.conf.level <- 0
    }

    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = StatCorr,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params =
        rlang::list2(method = method,
                     n.min = n.min,
                     alternative = alternative,
                     exact = exact,
                     conf.level = r.conf.level,
                     continuity = continuity,
                     small.r = small.r,
                     small.p = small.p,
                     coef.keep.zeros = coef.keep.zeros,
                     r.digits = r.digits,
                     t.digits = t.digits,
                     p.digits = p.digits,
                     CI.brackets = CI.brackets,
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
                     boot.R = boot.R,
                     na.rm = na.rm,
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
cor_test_compute_fun <- function(data,
                                 scales,
                                 method = "pearson",
                                 n.min = 2L,
                                 alternative = "two.sided",
                                 exact = NULL,
                                 conf.level = NA,
                                 continuity = FALSE,
                                 small.r = FALSE,
                                 small.p = FALSE,
                                 coef.keep.zeros,
                                 r.digits = 2,
                                 t.digits = 3,
                                 p.digits = 3,
                                 CI.brackets = c("[", "]"),
                                 label.x = "left",
                                 label.y = "top",
                                 hstep = 0,
                                 vstep = 0.1,
                                 npc.used = TRUE,
                                 output.type = "expression",
                                 boot.R = 0,
                                 na.rm = FALSE) {

  # Much of the complexity of the label formatting is needed to
  # prevent the automatic dropping of trailing zeros in expressions.
  # The approach used is to include formatted numbers as character
  # strings within expressions, which is very cumbersome.

  force(data)

  if (length(unique(data$x)) < n.min ||
      length(unique(data$y)) < n.min) {
    return(data.frame())
  }

  # parse obeys this option, but as for some labels or output types we do not
  # use parse() to avoid dropping of trailing zeros, we need to manage this in
  # our code in this case.
  decimal.mark <- getOption("OutDec", default = ".")
  if (!decimal.mark %in% c(".", ",")) {
    warning("Decimal mark must be one of '.' or ',', not: '", decimal.mark, "'")
    decimal.mark <- "."
  }
  range.sep <- c("." = ", ", "," = "; ")[decimal.mark]

  formula = ~ y + x

  output.type <- tolower(output.type)
  stopifnot(output.type %in%
              c("expression", "text", "markdown", "numeric", "latex", "tex", "tikz"))

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

  htest.ls <- do.call(cor.test,
                      args = list(formula = formula,
                                  alternative = alternative,
                                  method = method,
                                  exact = exact,
                                  conf.level = conf.level,
                                  continuity = continuity,
                                  data = data,
                                  na.action = na.omit))

  idx.map <- list(pearson = c(statistic = "t.value",
                              parameter = "df",
                              p.value = "p.value",
                              estimate = "cor",
                              alternative = "test"),
                  kendall = c(statistic = "z.value",
                              p.value = "p.value",
                              estimate = "tau",
                              alternative = "test"),
                  spearman = c(statistic = "S.value",
                               p.value = "p.value",
                               estimate = "rho",
                               alternative = "test"))

  # assign values common to all output types

  z <- htest.ls[names(idx.map[[method]])]
  names(z) <- unname(idx.map[[method]])

  z <- tibble::as_tibble_row(z)

  z[["n"]] <- nrow(na.omit(data[ , c("x", "y")]))
  z[["method"]] <- method
  z[["r.conf.level"]] <- conf.level

  if (boot.R >= 50 && conf.level > 0) {
    confint.boot <-
      confintr::ci_cor(data[ , c("x", "y")],
                       probs = ((1 - conf.level) / 2) * c(1, -1) + c(0, 1),
                       method = method,
                       type = "bootstrap",
                       R = boot.R)
    z[["r.confint.low"]]  <- confint.boot[["interval"]][1]
    z[["r.confint.high"]] <- confint.boot[["interval"]][2]
  } else {
    if (method == "pearson") {
      z[["r.confint.low"]]  <-  htest.ls[["conf.int"]][1]
      z[["r.confint.high"]] <-  htest.ls[["conf.int"]][2]
    } else {
      if (conf.level <= 0) {
        message("Skipping bootstrap estimation as 'conf.level' <= 0")
      } else if (boot.R > 0) {
        warning("Skipping bootstrap estimation as 'boot.R' < 50")
      }
      z[["r.confint.low"]]  <- NA_real_
      z[["r.confint.high"]] <- NA_real_
    }
  }

  if (output.type == "numeric") {
    z[["r.label"]] <- NA_character_
  } else {
    # build the character strings
    r <- z[[unname(c(pearson = "cor", kendall = "tau", spearman = "rho")[method])]]

    z[["p.value.label"]] <- p_value_label(value = z[["p.value"]],
                                          small.p = small.p,
                                          digits = p.digits,
                                          fixed = TRUE,
                                          output.type = output.type,
                                          decimal.mark = decimal.mark)
    z[["n.label"]] <- italic_label(value =  z[["n"]],
                                   value.name = "n",
                                   digits = 0,
                                   output.type = output.type,
                                   decimal.mark = decimal.mark)
    z[["grp.label"]] <- grp.label
    z[["r.label"]] <- r_label(value = r,
                              method = method,
                              small.r = small.r,
                              digits = r.digits,
                              fixed = TRUE,
                              output.type = output.type,
                              decimal.mark = decimal.mark)
    z[["r.confint.label"]] <- r_ci_label(value = c(z[["r.confint.low"]],
                                                   z[["r.confint.high"]]),
                                         conf.level = z[["r.conf.level"]],
                                         range.brackets = CI.brackets,
                                         range.sep = NULL,
                                         digits = r.digits,
                                         output.type = output.type,
                                         decimal.mark = decimal.mark)
    z[["method.label"]] <- paste("\"method: ", method, "\"", sep = "")
    if (method == "pearson") {
      z[["cor.label"]] <- z[["r.label"]]
      z[["rr.label"]] <- rr_label(value = z[["cor"]]^2,
                                  small.r = small.r,
                                  digits = r.digits,
                                  fixed = TRUE,
                                  output.type = output.type,
                                  decimal.mark = decimal.mark)
      z[["t.value.label"]] <- t_value_label(value = z[["t.value"]],
                                            df =  z[["df"]],
                                            digits = t.digits,
                                            fixed = FALSE,
                                            output.type = output.type,
                                            decimal.mark = decimal.mark)
    } else if (method == "kendall") {
      z[["tau.label"]] <- z[["r.label"]]
      z[["tau.confint.label"]] <- z[["r.confint.label"]]
      z[["z.value.label"]] <- italic_label(value = z[["z.value"]],
                                           value.name = "z",
                                           digits = t.digits,
                                           fixed = FALSE,
                                           output.type = output.type,
                                           decimal.mark = decimal.mark)
    } else if (method == "spearman") {
      z[["rho.label"]] <- z[["r.label"]]
      z[["rho.confint.label"]] <- z[["r.confint.label"]]
      z[["S.value.label"]] <-  italic_label(value = z[["S.value"]],
                                            value.name = "S",
                                            digits = t.digits,
                                            fixed = FALSE,
                                            output.type = output.type,
                                            decimal.mark = decimal.mark)
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
StatCorr <-
  ggplot2::ggproto("StaCorr",
                   ggplot2::Stat,
                   extra_params = c("na.rm", "parse"),
                   compute_group = cor_test_compute_fun,
                   default_aes =
                     ggplot2::aes(npcx = after_stat(npcx),
                                  npcy = after_stat(npcy),
                                  label = after_stat(r.label),
                                  hjust = "inward",
                                  vjust = "inward"),
                   required_aes = c("x", "y"),
                   optional_aes = "grp.label"
  )
