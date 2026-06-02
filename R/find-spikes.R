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
#'   \code{k} is the width in number of observations of the window used for
#'   running median smoothing to extract the baseline. A value several times the
#'   width of the broader spike but narrow enough to track broader peaks needs
#'   to be manually set in most cases.
#'
#'   If all spikes are guaranteed to be one observation-wide and either going up
#'   or down from the baseline, it is possible to detect them based purely on
#'   the \code{z.threshold} by passing \code{height.threshold = NA} and either
#'   \code{spike.direction = "up"} or \code{spike.direction = "down"}, which
#'   ensures very fast computation.
#'
#' @param x numeric vector containing the data.
#' @param x.is.delta logical Flag indicating whether \code{x} contains
#'   differences or original values.
#' @param height.threshold numeric The minimum height of spikes expressed relative
#'   to the median amplitude of the baseline local variation of \code{x}.
#' @param z.threshold numeric Modified local \eqn{Z} values larger than
#'   \code{z.threshold} are detected as boundaries of spikes.
#' @param k integer width of median window used for smoothing; must be odd
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
           height.threshold = 10,
           z.threshold = 5,
           k = 20,
           spike.direction = "both",
           na.rm = FALSE) {
    if (is.null(height.threshold)) {
      height.threshold <- 10
    } else if (!is.na(height.threshold) && height.threshold < 2) {
      warning("'height.threshold < 2' set to 2")
      height.threshold <- 2
    }
    if (is.null(k)) {
      k <- 20
    } else if (k %% 2 == 0) {
      k <- k + 1
    }
    x.len.original <- length(x)
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
    if (k > length(x) / 2) {
      x.median <- stats::median(x)
      d.var.median <- stats::median(d.var)
    } else {
      x.median <- stats::runmed(x,
                                k = k,
                                na.action = "na.omit",
                                endrule = "constant")
      d.var.median <- stats::runmed(d.var,
                                    k = k,
                                    na.action = "na.omit",
                                    endrule = "constant")
    }
    z <- (d.var - d.var.median) / stats::mad(d.var) * 0.6745
    outcomes.up <- c(FALSE, z > z.threshold)
    outcomes.down <- c(FALSE, z < -z.threshold)

    if (is.na(height.threshold)) {
      spikes.up <- outcomes.up
      spikes.down <- outcomes.down
    } else {
      scaled.threshold <- median(abs(d.var.median)) * height.threshold
      if (spike.direction %in% c("up", "both")) {
        outcomes.head.up <-
          outcomes.up & x > x.median + scaled.threshold
        temp <-
          outcomes.down &
          # near the baseline
          x <= x.median + scaled.threshold &
          x >= x.median - scaled.threshold
        outcomes.tail.up <- logical(length(temp))
        outcomes.tail.up[which(temp) - 1L] <- TRUE

        # fill gaps
        spk.starts <- which(outcomes.head.up)
        spk.ends <- which(outcomes.tail.up)

        if (length(spk.starts) > 1 && length(spk.ends) > 1) {
          # check if data ends or starts in a spike
          if (spk.ends[1] < spk.starts[1]) {
            if (spk.ends[1] > 1) {
              spk.starts <- c(1, spk.starts)
            } else {
              spk.ends <- spk.ends[-1L]
            }
          }
          if (spk.starts[length(spk.starts)] > spk.ends[length(spk.ends)]) {
            if (spk.ends[length(spk.ends)] < length(x)) {
              spk.ends <- c(spk.ends, length(x))
            } else {
              spk.starts <- spk.starts[-length(spk.starts)]
            }
          }
          outcomes.middle.up <- logical(length(x))
          i <- j <- 0
          i.max <- length(spk.starts)
          j.max <- length(spk.ends)
          while (i < i.max && j < j.max) {
            i <- i + 1
            j <- j + 1
            # skip narrow spikes
            while (spk.starts[i + 1] < spk.ends[j] && i < i.max) i <- i + 1
            while (spk.ends[j + 1] < spk.starts[i + 1] && j < j.max) j <- j + 1
            # fill in the middle of wide spikes
            if (spk.starts[i] + 1 < spk.ends[j]) {
              outcomes.middle.up[(spk.starts[i] + 1):(spk.ends[j] - 1)] <- TRUE
            }
          }
          spikes.up <- outcomes.head.up | outcomes.tail.up | outcomes.middle.up
        } else {
          spikes.up <- outcomes.up
        }
        spikes.up <-
          spikes.up & x > x.median + scaled.threshold
      }

      if (spike.direction %in% c("down", "both")) {
        outcomes.head.down <-
          outcomes.up & x < x.median - scaled.threshold
        temp <-
          outcomes.up &
          # near the baseline
          x <= x.median + scaled.threshold &
          x >= x.median - scaled.threshold
        outcomes.tail.down <- logical(length(temp))
        outcomes.tail.down[which(temp) - 1L] <- TRUE

        # fill gaps
        spk.starts <- which(outcomes.head.down)
        spk.ends <- which(outcomes.tail.down)

        if (length(spk.starts) > 1 && length(spk.ends) > 1) {
          # check if data ends or starts in a spike
          if (spk.ends[1] < spk.starts[1]) {
            if (spk.ends[1] > 1) {
              spk.starts <- c(1, spk.starts)
            } else {
              spk.ends <- spk.ends[-1L]
            }
          }
          if (spk.starts[length(spk.starts)] > spk.ends[length(spk.ends)]) {
            if (spk.ends[length(spk.ends)] < length(x)) {
              spk.ends <- c(spk.ends, length(x))
            } else {
              spk.starts <- spk.starts[-length(spk.starts)]
            }
          }
          outcomes.middle.down <- logical(length(x))
          i <- j <- 0
          i.max <- length(spk.starts)
          j.max <- length(spk.ends)
          while (i < i.max && j < j.max) {
            i <- i + 1
            j <- j + 1
            # skip narrow spikes
            while (spk.starts[i + 1] < spk.ends[j] && i < i.max) i <- i + 1
            while (spk.ends[j + 1] < spk.starts[i + 1] && j < j.max) j <- j + 1
            # fill in the middle of wide spikes
            if (spk.starts[i] + 1 < spk.ends[j]) {
              outcomes.middle.down[(spk.starts[i] + 1):(spk.ends[j] - 1)] <- TRUE
            }
          }
          spikes.down <- outcomes.head.down | outcomes.tail.down | outcomes.middle.down
        } else {
          spikes.down <- outcomes.down
        }
        spikes.down <-
          spikes.down & x < x.median - scaled.threshold



        temp <-
          outcomes.up &
          # near the baseline
          x <= x.median + scaled.threshold&
          x >= x.median - scaled.threshold
        outcomes.tail.down <- logical(length(temp))
        outcomes.tail.down[which(temp) - 1L] <- TRUE
        spikes.down <- outcomes.down | outcomes.tail.down
        spikes.down <-
          spikes.down & x < x.median - scaled.threshold
      }
    }

    outcomes <-
      switch(spike.direction,
             "up" = spikes.up,
             "down" = spikes.down,
             "both" = spikes.up | spikes.down,
             "skip" = logical(length(x)),
             {
               warning("'spike.direction' must be \"up\", \"down\", \"both\", or \"skip\", not \"",
                       spike.direction, "\"")
               logical(length(x))
             }
      )

    if (na.rm) {
      # restore length of logical vector
      for (i in na.idx) {
        outcomes <- append(outcomes, FALSE, after = i - 1L)
      }
    }
    # check assertion
    stopifnot(length(outcomes) == x.len.original)
    outcomes
  }
