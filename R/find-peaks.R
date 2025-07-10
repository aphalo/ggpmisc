#' Find local or global maxima (peaks) or minima (valleys)
#'
#' These functions find peaks (maxima) and valleys (minima) in a
#' numeric vector, using a user selectable span and global and local size
#' thresholds, returning a \code{logical} vector.
#'
#' @param x numeric vector.
#' @param global.threshold numeric A value belonging to class \code{"AsIs"} is
#'   interpreted as an absolute minimum height or depth expressed in data units.
#'   A bare \code{numeric} value (normally between 0.0 and 1.0), is interpreted
#'   as relative to \code{threshold.range}. In both cases it sets a
#'   \emph{global} height (depth) threshold below which peaks (valleys) are
#'   ignored. A bare negative \code{numeric} value indicates the \emph{global}
#'   height (depth) threshold below which peaks (valleys) are be ignored. If
#'   \code{global.threshold = NULL}, no threshold is applied and all peaks
#'   returned.
#' @param local.threshold numeric A value belonging to class \code{"AsIs"} is
#'   interpreted as an absolute minimum height (depth) expressed in data units
#'   relative to a within-window computed reference value. A bare \code{numeric}
#'   value (normally between 0.0 and 1.0), is interpreted as expressed in units
#'   relative to \code{threshold.range}. In both cases \code{local.threshold}
#'   sets a \emph{local} height (depth) threshold below which peaks (valleys)
#'   are ignored. If \code{local.threshold = NULL} or if \code{span} spans the
#'   whole of \code{x}, no threshold is applied.
#' @param local.reference character One of \code{"median"}, \code{"median.log"},
#'   \code{"median.sqrt"}, \code{"farthest"}, \code{"farthest.log"} or
#'   \code{"farthest.sqrt"}. The reference used to assess the height of the
#'   peak, either the minimum/maximum value within the window or the median of
#'   all values in the window.
#' @param threshold.range numeric vector If of length 2 or a longer vector
#'   \code{range(threshold.range)} is used to scale both thresholds. With
#'   \code{NULL}, the default, \code{range(x)} is used, and with a vector of
#'   length one \code{range(threshold.range, x)} is used, i.e., the range
#'   is expanded.
#' @param span odd positive integer A peak is defined as an element in a
#'   sequence which is greater than all other elements within a moving window of
#'   width \code{span} centred at that element. The default value is 5, meaning
#'   that a peak is taller than its four nearest neighbours. \code{span = NULL}
#'   extends the span to the whole length of \code{x}.
#' @param strict logical flag: if \code{TRUE}, an element must be strictly
#'   greater than all other values in its window to be considered a peak.
#'   Default: \code{FALSE} (since version 0.13.1).
#' @param na.rm logical indicating whether \code{NA} values should be stripped
#'   before searching for peaks.
#'
#' @return A vector of logical values of the same length as \code{x}. Values
#'   that are TRUE correspond to local peaks in vector \code{x} and can be used
#'   to extract the rows corresponding to peaks from a data frame.
#'
#' @details Function \code{find_peaks} is a wrapper built onto function
#'   \code{\link[splus2R]{peaks}} from \pkg{splus2R}, adds support for peak
#'   height thresholds and handles \code{span = NULL} and non-finite (including
#'   NA) values differently than \code{splus2R::peaks}. Instead of giving an
#'   error when \code{na.rm = FALSE} and \code{x} contains \code{NA} values,
#'   \code{NA} values are replaced with the smallest finite value in \code{x}.
#'   \code{span = NULL} is treated as a special case and selects \code{max(x)}.
#'   Passing `strict = TRUE` ensures that multiple global and within window
#'   maxima are ignored, and can result in no peaks being returned.
#'
#'   Two tests make it possible to ignore irrelevant peaks. One test
#'   (\code{global.threshold}) is based on the absolute height of the peaks and
#'   can be used in all cases to ignore globally low peaks. A second test
#'   (\code{local.threshold}) is available when the window defined by `span`
#'   does not include all observations and can be used to ignore peaks that are
#'   not locally prominent. In this second approach the height of each peak is
#'   compared to a summary computed from other values within the window of width
#'   equal to \code{span} where it was found. In this second case, the reference
#'   value used within each window containing a peak is given by
#'   \code{local.reference}. Parameter \code{threshold.range} determines how the
#'   bare \code{numeric} values passed as argument to \code{global.threshold}
#'   and \code{local.threshold} are scaled. The default, \code{NULL} uses the
#'   range of \code{x}. Thresholds for ignoring too small peaks are applied
#'   after peaks are searched for, and threshold values can in some cases
#'   result in no peaks being found. If either threshold is not available
#'   (\code{NA}) the returned value is a \code{NA} vector of the same length as
#'   \code{x}.
#'
#' @note The default for parameter \code{strict} is \code{FALSE} in functions
#'   \code{find_peaks()} and \code{find_valleys()}, while it is
#'   \code{strict = TRUE} in \code{\link[splus2R]{peaks}}.
#'
#' @seealso \code{\link[splus2R]{peaks}}.
#'
#' @family peaks and valleys functions
#'
#' @export
#'
#' @examples
#' # lynx is a time.series object
#' lynx_num.df <-
#'   try_tibble(lynx,
#'              col.names = c("year", "lynx"),
#'              as.numeric = TRUE) # years -> as numeric
#'
#' which(find_peaks(lynx_num.df$lynx, span = 5))
#' which(find_valleys(lynx_num.df$lynx, span = 5))
#' lynx_num.df[find_peaks(lynx_num.df$lynx, span = 5), ]
#' lynx_num.df[find_peaks(lynx_num.df$lynx, span = 51), ]
#' lynx_num.df[find_peaks(lynx_num.df$lynx, span = NULL), ]
#' lynx_num.df[find_peaks(lynx_num.df$lynx,
#'                        span = 15,
#'                        global.threshold = 2/3), ]
#' lynx_num.df[find_peaks(lynx_num.df$lynx,
#'                        span = 15,
#'                        global.threshold = I(4000)), ]
#' lynx_num.df[find_peaks(lynx_num.df$lynx,
#'                        span = 15,
#'                        local.threshold = 0.5), ]
#'
find_peaks <-
  function(x,
           global.threshold = NULL,
           local.threshold = NULL,
           local.reference = "median",
           threshold.range = NULL,
           span = 3,
           strict = FALSE,
           na.rm = FALSE) {
    # validate parameters
    if (length(unique(na.omit(x))) < 2L) {
      return(logical(length(x)))
    }
    if (is.null(span) || span >= length(x)) {
      # ignore local.threshold argument when not applicable
      local.threshold <- NULL
    }
    if (!is.null(span) && (is.na(span) || !is.numeric(span) || span < 1)) {
      stop("'span' must be NULL or a positive odd integer, not: ", format(span))
    }

    if (!is.null(local.threshold)) {
      if (is.na(local.threshold)) {
        return(rep(NA, length(x)))
      }
      if (!is.numeric(local.threshold) || length(local.threshold) > 1L ||
          local.threshold < 0 || local.threshold > 1) {
        stop("'local.threshold' must be NULL or a single number in [0..1], not: ",
             local.threshold)
      }
    }

    if (!is.null(global.threshold)) {
      if (is.na(global.threshold)) {
        return(rep(NA, length(x)))
      }
      if (!is.numeric(global.threshold) || length(global.threshold) > 1L) {
        stop("'global.threshold' must be NULL or a single number, not: ",
             local.threshold)
      } else if (!inherits(global.threshold, "AsIs") &&
                 (global.threshold < -1 || global.threshold > 1)) {
        stop("'global.threshold' when not \"AsIs\" must be a number in [-1..1]",
             "not: ", global.threshold)
      }
      if (inherits(global.threshold, "AsIs") && !is.finite(global.threshold)) {
        # accept all peaks/valleys
        global.threshold <- NULL
      }
    }

    # Replace NA, NaN and Inf with smallest finite value
    if (na.rm) {
      x <- ifelse(!is.finite(x), min(x, na.rm = TRUE), x)
    }

    # compute threshold range only if needed
    if (!is.null(global.threshold) || !is.null(local.threshold)) {
      if (is.null(threshold.range)) {
        threshold.range <- range(x, na.rm = TRUE)
      } else if (length(threshold.range) == 1L) {
        threshold.range <- range(threshold.range, x, na.rm = TRUE)
      } else {
        threshold.range <- range(threshold.range, na.rm = TRUE)
      }
      if (length(threshold.range) != 2L ||
          (threshold.range[2] - threshold.range[1]) < 1e-16) {
        warning("Skipping! Bad 'threshold.range': [",
                paste(threshold.range, collapse = ", "), "]")
        return(logical(length(x)))
      }
    }
    # compute global multiplier and base only if needed
    if (!is.null(global.threshold)) {
      if (inherits(global.threshold, "AsIs")) {
        global.multiplier <- 1
        global.base <- 0
      } else {
        global.multiplier <- abs(threshold.range[2] - threshold.range[1])
        global.base <- threshold.range[1]
      }
    }
    # compute local multiplier and base only if needed
    if (!is.null(local.threshold)) {
      if (inherits(local.threshold, "AsIs")) {
        local.multiplier <- 1
      } else {
        local.multiplier <- threshold.range[2] - threshold.range[1]
      }
    }

    # search for global or local maxima
    if (is.null(span) || span >= length(x)) {
      # find maximum
      pks <- (x == max(x, na.rm = na.rm))
      if (strict && sum(pks) > 1L) {
        message("Non-unique extreme value discarded as 'strict = TRUE'")
        pks <- logical(length(x)) # all FALSE
      }
    } else {
      # search for local maxima
      pks <-
        splus2R::peaks(x = x, span = span, strict = strict, endbehavior = 0)
    }

    # apply global height threshold test
    if (!is.null(global.threshold)) {
      if (global.threshold >= 0 || inherits(global.threshold, "AsIs")) {
        pks <- pks &
          x > global.base + global.threshold * global.multiplier
      } else {
        pks <- pks &
          x <= global.base + abs(global.threshold) * global.multiplier
      }
    }

    # apply local.threshold height test to found peaks
    if (!is.null(local.threshold)) {
      # apply local height threshold test
      local.base <-
        switch(local.reference,
               median =,
               median.sqrt =,
               median.log = stats::runmed(x, k = span, endrule = "median"),
               farthest =,
               farthest.sqrt =,
               farthest.log = caTools::runmin(x, k = span, endrule = "min"),
               stop("Bad 'local.reference': ", local.reference))
      if (grepl("\\.log$", local.reference)) {
        pks <- pks & log(x - local.base) >
          (local.threshold * log(local.multiplier))
      } else if (grepl("\\.sqrt$", local.reference)) {
        pks <- pks & sqrt(x - local.base) >
          (local.threshold * sqrt(local.multiplier))
      } else {
        pks <- pks & (x - local.base) > (local.threshold * local.multiplier)
      }
    }
    pks
  }

#' @rdname find_peaks
#'
#' @export
#'
find_valleys <-
  function(x,
           global.threshold = NULL,
           local.threshold = NULL,
           local.reference = "median",
           threshold.range = NULL,
           span = 3,
           strict = FALSE,
           na.rm = FALSE) {
    x <- -x
    if (inherits(global.threshold, "AsIs")) {
      global.threshold <- I(-global.threshold)
    }
    if (inherits(local.threshold, "AsIs")) {
      local.threshold <- I(-local.threshold)
    }
    if (!is.null(threshold.range)) {
      threshold.range <- rev(-threshold.range)
    }
    find_peaks(x = x,
               global.threshold = global.threshold,
               local.threshold = local.threshold,
               local.reference = local.reference,
               threshold.range = threshold.range,
               span = span,
               strict = strict,
               na.rm = na.rm)
  }
