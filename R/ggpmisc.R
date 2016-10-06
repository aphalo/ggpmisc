#' @details The new facilities for cleanly defining new stats and geoms added to
#' 'ggplot2' in version 2.0.0 have made this package easy to code. However, this
#' means that this package requires version 2.0.0 or later of ggplot2.
#'
#' Extensions provided:
#' \itemize{
#' \item Function for conversion of time series data into data frames that can be
#' plotted with ggplot.
#' \item Stats for locating and tagging "peaks" and "valleys" (local or global
#'  maxima and minima).
#' \item Stat for generating labels from a \code{lm()} model fit, including
#' formatted equation. By default labels are expressions but tikz device is
#' supported optionally with LaTeX formatted labels.
#' \item Stats for extracting information from a any model fit supported by
#' package 'broom'.
#' \item Stats for filtering-out/filtering-in observations in regions of a
#' panel or group where the density of observations is high.
#' \item "Debug" stats and a "debug" geom that print to the console a summary
#' of their \code{data} input.
#' }
#'
#' The stats for peaks and valleys are coded so as to work correctly both with
#' numeric and POSIXct variables mapped to the x aesthetic. Special handling was
#' needed as text labels are generated from the data.
#'
#' @author Pedro J. Aphalo
#'
#' @section Acknowledgements:
#' We thank Kamil Slowikowski not only for contributing ideas and code examples
#' to this package but also for adding new features to his package 'ggrepel'
#' that allow new use cases for \code{stat_dens2d_labels} from this package.
#'
#' @references
#' Package suite 'r4photobiology' web site at
#' \url{http://www.r4photobiology.info/}\cr
#' Package 'ggplot2' web site at \url{http://ggplot2.org/}\cr
#' Package 'ggplot2' documentation at \url{http://docs.ggplot2.org/}\cr
#' Package 'ggplot2' source code at \url{https://github.com/hadley/ggplot2}
#'
#' @import ggplot2
#' @importFrom ggplot2 ggplot
#'
#' @note The signatures of \code{stat_peaks()} and \code{stat_valleys()} are
#'   identical to those of \code{stat_peaks} and \code{stat_valleys} from
#'   package \code{photobiology} but the variables returned are a subset as
#'   values related to light spectra are missing. Furthermore the stats from
#'   package \code{ggpmisc} work correctly when the x aesthetic uses a date or
#'   datetime scale, while those from package \code{photobiology} do not
#'   generate correct labels in this case.
#'
"_PACKAGE"
