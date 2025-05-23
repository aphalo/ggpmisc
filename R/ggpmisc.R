#' @details The new facilities for cleanly defining new stats and geoms added to
#'   'ggplot2' in version 2.0.0 and the support for nested data frames and lists
#'   and new syntax for mapping computed values to aesthetics added to 'ggplot2'
#'   in version 3.0.0 are used in this package's code, as well as some features
#'   added in more recent updates including 3.5.0. This means that current
#'   'ggpmisc' versions require recent versions of ggplot2.
#'
#' Extensions provided:
#' \itemize{
#' \item Function for conversion of time series data into tibbles that can be
#' plotted with ggplot.
#' \item \code{ggplot()} method for time series data.
#' \item Stats for locating and tagging "peaks" and "valleys" (local or global
#'  maxima and minima).
#' \item Stat for generating labels from model fit objects, including
#' formatted equations. By default labels are R's plotmath expressions but LaTeX,
#' markdown and plain text formatted labels are optionaly assembled.
#' \item Stats for extracting information from a any model fit supported by
#' package 'broom' and using it to generate various annotations and data labels.
#' \item Stat for computing and generating labels for the results from multiple
#' comparisons, including adjusted \emph{P}-values.}
#'
#' The stats for peaks and valleys are coded so as to work correctly both with
#' numeric and POSIXct variables mapped to the x aesthetic. Special handling was
#' needed as text labels are generated from the data.
#'
#' @references Package suite 'r4photobiology' web site at
#' \url{https://www.r4photobiology.info/}\cr Package 'ggplot2' documentation at
#' \url{https://ggplot2.tidyverse.org/}\cr Package 'ggplot2' source code at
#' \url{https://github.com/tidyverse/ggplot2}
#'
#' @import stats scales grid ggpp
#' @importFrom rlang .data
#' @importFrom ggplot2 expansion after_stat
#'
#' @note The signatures of \code{stat_peaks()} and \code{stat_valleys()} from
#'   'ggpmisc' are identical to those of \code{stat_peaks} and
#'   \code{stat_valleys} from package 'ggspectra' but the variables returned are
#'   a subset as special handling of values related to light spectra is missing.
#'   Furthermore the \code{stat_peaks()} and \code{stat_valleys()} from package
#'   'ggpmisc' work correctly when date or datetime values are mapped to the
#'   \emph{x} statistic, while those from package 'ggspectra' do not generate
#'   correct labels in this case.
#'
#' @examples
#' library(tibble)
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
