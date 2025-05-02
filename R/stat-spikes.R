#' Find spikes
#'
#' This function finds spikes in a numeric vector using the algorithm of
#' Whitaker and Hayes (2018). Spikes are values in spectra that are unusually
#' high or low compared to neighbours. They are usually individual values or very
#' short runs of similar "unusual" values. Spikes caused by cosmic radiation are
#' a frequent problem in Raman spectra. Another source of spikes are "hot
#' pixels" in CCD and diode arrays. Other kinds of accidental "outlayers" are
#' be also detected.
#'
#' @details Spikes are detected based on a modified Z score calculated from the
#'   differenced spectrum. The Z threshold used should be adjusted to the
#'   characteristics of the input and desired sensitivity. The lower the
#'   threshold the more stringent the test becomes, resulting in most cases in
#'   more spikes being detected. A modified version of the algorithm is used if
#'   a value different from \code{NULL} is passed as argument to
#'   \code{max.spike.width}. In such a case, an additional step filters out
#'   broader spikes (or falsely detected steep slopes) from the returned values.
#'
#' @param x numeric vector containing spectral data.
#' @param x.is.delta logical Flag indicating if x contains already differences.
#' @param z.threshold numeric Modified Z values larger than \code{z.threshold}
#'   are considered to be spikes.
#' @param max.spike.width integer Wider regions with high Z values are not detected as
#'   spikes.
#' @param na.rm logical indicating whether \code{NA} values should be stripped
#'   before searching for spikes.
#'
#' @return A logical vector of the same length as \code{x}. Values that are TRUE
#'   correspond to local spikes in the data.
#'
#' @references
#' Whitaker, D. A.; Hayes, K. (2018) A simple algorithm for despiking Raman
#' spectra. Chemometrics and Intelligent Laboratory Systems, 179, 82-84.
#'
#' @export
#'
#' @family peaks and valleys functions
#'
find_spikes <-
  function(x,
           x.is.delta = FALSE,
           z.threshold = 9,
           max.spike.width = 8,
           na.rm = FALSE) {
    if (na.rm) {
      na.idx <- which(is.na(x))
      x <- na.omit(x)
    }
    if (x.is.delta) {
      d.var <- x
    } else {
      d.var <- diff(x)
    }
    z <- (d.var - stats::median(d.var)) / stats::mad(d.var) * 0.6745
    outcomes <- abs(z) > z.threshold
    if (!x.is.delta) {
      # ensure same length as input
      outcomes <- c(FALSE, outcomes)
    }
    if (!is.null(max.spike.width) && max.spike.width > 0) {
      # ignore broad peaks using run length encoding
      runs <- rle(outcomes)
      runs[["values"]] <- ifelse(runs[["lengths"]] > max.spike.width, FALSE, runs[["values"]])
      outcomes <- inverse.rle(runs)
    }
    if (na.rm) {
      # restore length of logical vector
      for (i in na.idx) {
        outcomes <- append(outcomes, FALSE, after = i - 1L)
      }
    }
    # check assertion
    stopifnot(length(outcomes) == length(x))
    outcomes
  }

