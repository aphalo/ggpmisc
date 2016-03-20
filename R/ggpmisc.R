#' @details The new facilities for cleanly defining new stats and geoms added to
#' ggplot2 in version 2.0.0 have made this package easy to code. However, this
#' means that this package requires version 2.0.0 or later of ggplot2.
#'
#' Extensions provided:
#' \itemize{
#' \item Function for conversion of time series data into data frames that can be
#' plotted with ggplot.
#' \item Stats for locating and tagging "peaks" and "valleys" (local or global
#'  maxima and minima).
#' \item Stat for generating labels from a \code{lm()} model fit, including
#' formatted equation.
#' \item "Debug" stats and a "debug" geom that print to the console a summary
#' ot their \code{data} input.
#' }
#'
#' The stats for peaks and valleys are coded so as to work correctly both with
#' numeric and POSIXct variables mapped to the x aesthetic. Special handling was
#' needed as text labels are generated from the data.
#'
#' @author Pedro J. Aphalo
#'
#' @references
#' \code{ggplot2} web site at \url{http://ggplot2.org/}\cr
#' \code{ggplot2} source code at \url{https://github.com/hadley/ggplot2}
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
