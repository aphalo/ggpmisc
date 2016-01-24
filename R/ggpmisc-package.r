#' Miscelaneous extensions to ggplot2
#'
#' Pakage \code{ggpmisc} is a package with extensions to ggplot2.
#' It defines statistics for finding peaks and valleys, constructing labels
#' for polynomial equations fitted with \code{lm()}, and for debugging
#' or discovering the data that \code{compute_group} and \code{compute_panel}
#' are passed within ggplot statistics.
#'
#' @docType package
#' @keywords misc
#' @name ggpmisc-package
#' @aliases ggpmisc
#' @author Pedro J. Aphalo
#' @details
#' \tabular{ll}{
#' Package: \tab ggpmisc\cr
#' Type: \tab Package\cr
#' Version: \tab 0.2.0\cr
#' Date: \tab 2016-01-24\cr
#' License: \tab GPL (>= 3.0)\cr
#' URL: \tab \url{http://www.r4photobiology.info},\cr
#' }
#' @references
#' \code{ggplot2} web site at \url{http://ggplot2.org/}\cr
#' \code{ggplot2} source code at \url{https://github.com/hadley/ggplot2}
#'
#' @note The signatures of \code{stat_peaks()} and \code{stat_valleys()} are
#' identical to those of \code{\link[ggspectra]{stat_peaks}} and
#' \code{\link[ggspectra]{stat_valleys}} from package
#' \code{\link[photobiology]{photobiology}}
#' but the variables returned are a subset as values related to light spectra
#' are missing.
#'
NULL
