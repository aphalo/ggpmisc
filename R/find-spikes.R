#' Find spikes
#'
#' This function finds spikes in a numeric vector using the algorithm of
#' Whitaker and Hayes (2018). Spikes are values in spectra that are unusually
#' high or low compared to neighbours. They are usually individual values or very
#' short runs of similar "unusual" values. Spikes caused by cosmic radiation are
#' a frequent problem in Raman spectra. Another source of spikes are "hot
#' pixels" in CCD and diode arrays. Other kinds of accidental "outliers" can
#' be also detected.
#'
#' @details Spikes are detected based on a modified \eqn{Z} score calculated
#'   from the differenced spectrum. The \eqn{Z} threshold used should be
#'   adjusted to the characteristics of the input and desired sensitivity. The
#'   lower the threshold the more stringent the test becomes, with shorter
#'   spikes being detected.
#'
#'   The algorithm uses running differences to detect abrupt changes in value,
#'   compared to an estimate of the baseline variation of the differences,
#'   approximating a baseline \eqn{Z} from MAD and a baseline value from the
#'   median differences. Currently, a single estimate of MAD is used but running
#'   medians, when posisble, as baseline. This comparison detects running
#'   differences that are unusually large, in most cases signalling a transition
#'   between values near the baseline and far from it, in both directions.
#'
#'   Transitions into- and out of spikes are distinguished based on the median
#'   of the non-differenced values, as a descriptor of the data baseline. As for
#'   the median of the differences, a running median is used when possible.
#'
#'   This function thus detects the start and end of each spike, and
#'   distinguishes upward and downward spikes.
#'
#'   \code{max.spike.width} is currently ignored as a threshold, but it does
#'   affect the width of the window used by the running medians and the minimum
#'   length of \code{x} at which running medians start being used instead of the
#'   overall median.
#'
#' @param x numeric vector containing the data.
#' @param x.is.delta logical Flag indicating whether \code{x} contains
#'   differences or original values.
#' @param x.threshold numeric The minimum height of spikes expressed relative
#'   to the range of the original \code{x} values, not the differences.
#' @param z.threshold numeric Modified Z values larger than \code{z.threshold}
#'   are considered to be spikes.
#' @param max.spike.width integer Maximum number of consecutive large or small
#'   observations accepted as a spike. Wider peaks and valleys are ignored.
#'   \code{NULL} disables this test.
#' @param spike.direction character One of \code{"up"}, \code{"down"} or
#'   \code{"both"}, indicating which spikes are to be returned.
#' @param na.rm logical indicating whether \code{NA} values should be stripped
#'   before searching for spikes.
#'
#' @return A logical vector of the same length as \code{x}. Values that are
#'   \code{TRUE} correspond to the boundaries of upwards, downwards or both
#'   downwards and upwards local spikes in the data.
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
           x.threshold = 0.02,
           z.threshold = 5,
           max.spike.width = NULL,
           spike.direction = "both",
           na.rm = FALSE) {
    if (is.null(x.threshold)) {
      x.threshold <- 0.1
    }
    if (is.null(max.spike.width)) {
      max.spike.width <- 5
    }
    if (na.rm) {
      na.idx <- which(is.na(x))
      x <- na.omit(x)
    }
    if (x.is.delta) {
      d.var <- x
      x <- stats::diffinv(x)
    } else {
      d.var <- diff(x)
      x <- x - x[1]
    }
    # running median is used to detect spikes relative to the local baseline
    window.span <- max.spike.width * 10
    window.span <- ifelse(window.span %% 2L, window.span, window.span + 1L)
    if (window.span > length(x) / 2) {
      x.median <- stats::median(x)
      d.var.median <- stats::median(d.var)
    } else {
      x.median <- stats::runmed(x, k = window.span, na.action = "na.omit")
      d.var.median <- stats::runmed(d.var, k = window.span, na.action = "na.omit")
    }
    z <- (d.var - d.var.median) / stats::mad(d.var) * 0.6745
    outcomes.up <- c(FALSE, z > z.threshold)
    outcomes.down <- c(FALSE, z < -z.threshold)

    # we can use a multiplier on the median if we normalize it to the range
    k.threshold <- abs(diff(range(x))) / x.median * x.threshold
    temp <-
      outcomes.down & x <= x.median * (1 + k.threshold)
    outcomes.tail.up <- logical(length(temp))
    outcomes.tail.up[which(temp) - 1L] <- TRUE
    spikes.up <- outcomes.up | outcomes.tail.up
    spikes.up <-
      spikes.up & x > x.median * (1 + k.threshold)

    temp <-
      outcomes.up & x >= x.median * (1 - k.threshold)
    outcomes.tail.down <- logical(length(temp))
    outcomes.tail.down[which(temp) - 1L] <- TRUE
    spikes.down <- outcomes.down | outcomes.tail.down
    spikes.down <-
      spikes.down & x < x.median * (1 - k.threshold)

    outcomes <-
      switch(spike.direction,
             "up" = spikes.up,
             "down" = spikes.down,
             "both" = spikes.up | spikes.down,
             {
               warning("'spike.direction' must be \"up\", \"down\" or \"both\", not \"",
                       spike.direction, "\"")
               NA
             }
      )

    # Here code to fill outcomes with the middle values of spikes is needed!
    #
    # currently outcomes points to discontinuities not the whole spike!
    #
    # if (!is.null(max.spike.width) && max.spike.width > 0) {
    #   # ignore broad peaks using run length encoding
    #   runs <- rle(outcomes)
    #   runs[["values"]] <- ifelse(runs[["lengths"]] > max.spike.width, FALSE, runs[["values"]])
    #   outcomes <- inverse.rle(runs)
    # }
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
