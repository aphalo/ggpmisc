#' @details The new facilities for cleanly defining new stats and geoms added to
#' 'ggplot2' in version 2.0.0 and the support for nested tibbles and new syntax
#' for mapping computed values to aesthetics added to
#' 'ggplot2' in version 3.0.0 are used in this package's code. This
#' means that 'ggpmisc' (>= 0.3.0) requires version 3.0.0 or later of ggplot2
#' while 'ggpmisc' (< 0.3.0) requires version 2.0.0 or later of ggplot2.
#'
#' Extensions provided:
#' \itemize{
#' \item Function for conversion of time series data into tibbles that can be
#' plotted with ggplot.
#' \item \code{ggplot()} method for time series data.
#' \item Stats for locating and tagging "peaks" and "valleys" (local or global
#'  maxima and minima).
#' \item Stat for generating labels from a \code{lm()} model fit, including
#' formatted equation. By default labels are expressions but tikz device is
#' supported optionally with LaTeX formatted labels.
#' \item Stats for extracting information from a any model fit supported by
#' package 'broom'.
#' \item Stats for filtering-out/filtering-in observations in regions of a
#' panel or group where the density of observations is high.
#' \item Geom for annotating plots with tables.
#' }
#'
#' The stats for peaks and valleys are coded so as to work correctly both with
#' numeric and POSIXct variables mapped to the x aesthetic. Special handling was
#' needed as text labels are generated from the data.
#'
#' @section Warning!: \code{geom_null()}, \code{stat_debug_group()},
#'   \code{stat_debug_panel()}, \code{geom_debug()}, \code{append_layers()},
#'   \code{bottom_layer()}, \code{delete_layers()}, \code{extract_layers()},
#'   \code{move_layers()}, \code{num_layesr()}, \code{shift_layers()},
#'   \code{top_layer()} and \code{which_layers()} have been moved from package
#'   'ggpmisc' into their own seperate package
#'   '\code{\link[gginnards]{gginnards-package}}.
#'
#' @section Acknowledgements: We thank Kamil Slowikowski not only for
#'   contributing ideas and code examples to this package but also for adding
#'   new features to his package 'ggrepel' that allow new use cases for
#'   \code{stat_dens2d_labels} from this package.
#'
#' @references Package suite 'r4photobiology' web site at
#' \url{https://www.r4photobiology.info/}\cr Package 'ggplot2' documentation at
#' \url{https://ggplot2.tidyverse.org/}\cr Package 'ggplot2' source code at
#' \url{https://github.com/hadley/ggplot2}
#'
#' @import scales grid ggplot2
#' @importFrom ggplot2 ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @note The signatures of \code{stat_peaks()} and \code{stat_valleys()} are
#'   identical to those of \code{stat_peaks} and \code{stat_valleys} from
#'   package \code{photobiology} but the variables returned are a subset as
#'   values related to light spectra are missing. Furthermore the stats from
#'   package \code{ggpmisc} work correctly when the x aesthetic uses a date or
#'   datetime scale, while those from package \code{photobiology} do not
#'   generate correct labels in this case.
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
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(aes(label = ..eq.label..), formula = formula,
#'                parse = TRUE)
#'
#' formula <- y ~ x
#' ggplot(PlantGrowth, aes(group, weight)) +
#'   stat_summary(fun.data = "mean_se") +
#'   stat_fit_tb(method = "lm",
#'               method.args = list(formula = formula),
#'               tb.type = "fit.anova") +
#'   theme_classic()
#'
"_PACKAGE"
