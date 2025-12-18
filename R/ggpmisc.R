#' @details Package 'ggpmisc' is over 10 years-old but its development has
#'   tracked the changes in 'ggplot2' making possible the use of several new
#'   features soon after they became available in 'ggplot2'. Support for
#'   additional model fitting functions is added regularly.
#'
#'   The focus of package 'ggpmisc' is on statistical annotations, providing
#'   stats that generate labels useful to annotate plots. Model fitting is done
#'   by calling functions already available in R and other R packages. No new
#'   model fit method or algorithms are implemented, instead what 'ggpmisc'
#'   provides are new simpler ways of adding fitted values and other statistics
#'   as plot annotations.
#'
#'   Several geometries for annotations from package 'ggpp' are used by default
#'   in 'ggpmisc' statistics, with labels formatted by default ready to be
#'   parsed into R's plotmath expressions. However, other geometries can be also
#'   used. Markdown-formatted labels work smoothly with geoms from package
#'   'ggtext', but not with package 'Marquee'. LaTeX-formatted labels work
#'   smoothly with package 'xdvir' and most likely also with other approaches to
#'   the use of 'LaTeX' and 'TeX' formatted labels. 'LaTeX'-formatted labels can
#'   be generated as bare maths-mode-encoded text, or enclosed in "fences" that
#'   enable either in-line or display-maths modes.
#'
#'   The label formatting functions used to implement the statistics are
#'   exported and can be used as an aid in building customised labels.
#'
#' Extensions provided:
#' \itemize{
#' \item Statistics for annotations for parametric and non-parametric
#' correlations.
#' \item Statistics for generation of labels for fitted models, including
#' formatted equations. By default labels are R's plotmath expressions but LaTeX,
#' markdown and plain text formatted labels are optionally returned.
#' \item Matching statistics for plotting curves and confidence bands bands for
#' the same fitted models.
#' \item Statistics for adding ANOVA tables and fitted model summaries as inset
#' tables in plots.
#' \item Statistic for adding annotations based on pairwise multiple
#' comparisons based on arbitrary contrasts and a choice of \emph{P} adjustment
#' methods.
#' \item Statistics for locating and tagging "peaks" and "valleys" (local or
#' global maxima and minima) and spikes (very narrow peaks or valleys).
#' \item Access to functions and objects exported by
#' \link[ggpp:ggpp-package]{package ggpp}.}
#'
#' The stats for peaks and valleys are coded so as to work correctly both with
#' numeric and POSIXct variables mapped to the \emph{x} aesthetic.
#'
#' @import stats scales grid ggpp
#' @importFrom rlang .data
#' @importFrom ggplot2 expansion after_stat
#'
#' @note The signatures of \code{stat_peaks()} and \code{stat_valleys()} from
#'   'ggpmisc' are nearly identical to those of \code{stat_peaks()} and
#'   \code{stat_valleys()} from package 'ggspectra'. While those from 'ggpmisc'
#'   are designed for numeric or time objects mapped to the \emph{x} aesthetic,
#'   those from 'ggspectra' are for light spectra and expect a numeric variable
#'   describing wavelength mapped to the \emph{x} aesthetic.
#'
#' @examples
#'
#' ggplot(lynx, as.numeric = FALSE) + geom_line() +
#' stat_peaks(colour = "red") +
#'   stat_peaks(geom = "text", colour = "red", angle = 66,
#'              hjust = -0.1, x.label.fmt = "%Y") +
#'   ylim(NA, 8000)
#'
#' formula <- y ~ poly(x, 2, raw = TRUE)
#' ggplot(cars, aes(speed, dist)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(use_label("eq", "R2", "P"),
#'                formula = formula,
#'                parse = TRUE) +
#'   labs(x = expression("Speed, "*x~("mph")),
#'        y = expression("Stopping distance, "*y~("ft")))
#'
#' formula <- y ~ x
#' ggplot(PlantGrowth, aes(group, weight)) +
#'   stat_summary(fun.data = "mean_se") +
#'   stat_fit_tb(method = "lm",
#'               method.args = list(formula = formula),
#'               tb.type = "fit.anova",
#'               tb.vars = c(Term = "term", "df", "M.S." = "meansq",
#'                           "italic(F)" = "statistic",
#'                           "italic(p)" = "p.value"),
#'               tb.params = c("Group" = 1, "Error" = 2),
#'               table.theme = ttheme_gtbw(parse = TRUE)) +
#'   labs(x = "Group", y = "Dry weight of plants") +
#'   theme_classic()
#'
"_PACKAGE"
