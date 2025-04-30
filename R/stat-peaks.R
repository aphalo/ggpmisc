#' Find local maxima or global maximum (peaks)
#'
#' This function finds peaks (local maxima) in a vector, using a user selectable
#' span and size threshold relative to the tallest peak (global maximum).
#'
#' @param x numeric vector.
#' @param global.threshold numeric A value between 0.0 and 1.0 indicating the
#'   \emph{global} height threshold below which peaks will be ignored, or a
#'   negative value, between 0.0 and -1.0 indicating the  \emph{global} height
#'   threshold above which peaks will be ignored. These values are relative to
#'   the range of \code{x}.
#' @param local.threshold numeric A value between 0.0 and 1.0 indicating the
#'   \emph{within-window} height threshold below which peaks will be ignored.
#'   These values are relative to the range of \code{x}.
#' @param local.reference character One of \code{"minimum"} or \code{"median"}.
#'   The reference used to assess the height of the peak, either the minimum
#'   value wihtin the window or the median of all values in the window.
#' @param span odd integer A peak is defined as an element in a sequence which
#'   is greater than all other elements within a moving window of width
#'   \code{span} centred at that element. The default value is 3, meaning that a
#'   peak is bigger than both of its neighbours. \code{span = NULL} extends the
#'   span to the whole length of \code{x}.
#' @param strict logical flag: if TRUE, an element must be strictly greater than
#'   all other values in its window to be considered a peak. Default: TRUE.
#' @param na.rm logical indicating whether \code{NA} values should be stripped
#'   before searching for peaks.
#'
#' @return A vector of logical values. Values that are TRUE correspond to local
#'   peaks in vector \code{x} and can be used to extract the rows corresponding
#'   to peaks from a data frame.
#'
#' @details This function is a wrapper built onto function
#'   \code{\link[splus2R]{peaks}} from \pkg{splus2R} and handles non-finite
#'   (including NA) values differently than \code{peaks}, instead of giving an
#'   error when \code{na.rm = FALSE} and \code{x} contains \code{NA} values,
#'   \code{NA} values are replaced with the smallest finite value in \code{x}.
#'   \code{span = NULL} is treated as a special case and returns \code{max(x)}.
#'   Two tests are optional, one based on the absolute height of the peaks
#'   (\code{global.threshold}) and another based on the height of the peaks
#'   compared to other values within the window of width equal to \code{span}
#'   (\code{local.threshold}). The reference value used within each window
#'   containing a peak is given by \code{local.reference}.
#'
#' @note The default for parameter \code{strict} is \code{FALSE} in functions
#'   \code{peaks()} and \code{find_peaks()}, as in \code{stat_peaks()} and in
#'   \code{stat_valleys()}, while the default in \code{\link[splus2R]{peaks}}
#'   is \code{strict = TRUE}.
#'
#' @seealso \code{\link[splus2R]{peaks}}
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
#' which(find_peaks(lynx_num.df$lynx, span = 31))
#' lynx_num.df[find_peaks(lynx_num.df$lynx, span = 15), ]
#' lynx_num.df[find_peaks(lynx_num.df$lynx, span = NULL), ]
#' lynx_num.df[find_peaks(lynx_num.df$lynx,
#'                        span = 31,
#'                        global.threshold = 0.75), ]
#'
#' lynx_datetime.df <-
#'    try_tibble(lynx,
#'               col.names = c("year", "lynx")) # years -> POSIXct
#'
#' which(find_peaks(lynx_datetime.df$lynx, span = 31))
#' lynx_datetime.df[find_peaks(lynx_datetime.df$lynx, span = 31), ]
#' lynx_datetime.df[find_peaks(lynx_datetime.df$lynx,
#'                             span = 31,
#'                             global.threshold = 0.75), ]
#'
find_peaks <-
  function(x,
           global.threshold = 0,
           local.threshold = 0,
           local.reference = "minimum",
           span = 3,
           strict = FALSE,
           na.rm = FALSE) {
    # find peaks
    if(is.null(span)) {
      pks <- x == max(x, na.rm = na.rm)
      if (strict && sum(pks) != 1L) {
        pks <- logical(length(x)) # all FALSE
      }
    } else {
      pks <- splus2R::peaks(x = x, span = span, strict = strict)
    }
    # apply global height threshold test to found peaks
    if (abs(global.threshold) >= 1e-5) {
      range_x <- range(x, na.rm = na.rm, finite = TRUE)
      min_x <- range_x[1]
      max_x <- range_x[2]
      x <- ifelse(!is.finite(x), min_x, x)
      # this can cater for the case when max_x < 0, as with logs
      delta <- max_x - min_x
      top_flag <- global.threshold > 0.0
      scaled_threshold <- delta * abs(global.threshold)
      if (top_flag) {
        pks <- ifelse(x - min_x > scaled_threshold, pks , FALSE)
      } else {
        pks <- ifelse(max_x - x > scaled_threshold, pks , FALSE)
      }
    }
    # apply local.threshold height test to found peaks
    if (local.threshold > 0) {
      range_x <- range(x, na.rm = na.rm, finite = TRUE)
      min_x <- range_x[1]
      max_x <- range_x[2]
      local.threshold <- local.threshold * (max_x - min_x)
      smooth_x <-
        switch(local.reference,
               minimum = caTools::runmin(x, k = span, endrule = "min"),
               median = runmed(x, k = span, endrule = "median"))
      pks <- ifelse(pks,
                    (x - smooth_x) > local.threshold,
                    pks)
    }
    pks
  }

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

#' Local maxima (peaks) or minima (valleys)
#'
#' \code{stat_peaks} finds at which x positions local y maxima are located and
#' \code{stat_valleys} finds at which x positions local y minima are located.
#' Both stats return a subset of \code{data} with rows matching for peaks or
#' valleys with formatted character labels added. The formatting is determined
#' by a format string compatible with \code{sprintf()} or \code{strftime()}.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'    the plot defaults.
#' @param geom The geometric object to use display the data.
#' @param position The position adjustment to use for overlapping points
#'    on this layer.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical value indicating whether NA values should be
#'   stripped before the computation proceeds.
#' @inheritParams find_peaks
#' @param local.reference character One of \code{"median"} or \code{"minimum"}
#'   (for peaks) and \code{"maximum"} (for valleys).
#' @param label.fmt character  string giving a format definition for converting
#'   values into character strings by means of function \code{\link{sprintf}}
#'   or \code{\link{strptime}}, its use is deprecated.
#' @param x.label.fmt character  string giving a format definition for
#'   converting $x$-values into character strings by means of function
#'   \code{\link{sprintf}} or \code{\link{strftime}}. The default argument
#'   varies depending on the scale in use.
#' @param y.label.fmt character  string giving a format definition for
#'   converting $y$-values into character strings by means of function
#'   \code{\link{sprintf}}.
#' @param orientation character Either "x" or "y".
#'
#' @section Returned and computed variables:
#' \describe{
#'   \item{x}{x-value at the peak (or valley) as numeric}
#'   \item{y}{y-value at the peak (or valley) as numeric}
#'   \item{x.label}{x-value at the peak (or valley) as character}
#'   \item{y.label}{y-value at the peak (or valley) as character}
#' }
#'
#' @details These stats use \code{geom_point} by default as it is the geom most
#'   likely to work well in almost any situation without need of tweaking. The
#'   default aesthetics set by these stats allow their direct use with
#'   \code{geom_text}, \code{geom_label}, \code{geom_line}, \code{geom_rug},
#'   \code{geom_hline} and \code{geom_vline}. The formatting of the labels
#'   returned can be controlled by the user.
#'
#'   The default for parameter \code{strict} is \code{TRUE} in functions
#'   \code{splus2R::peaks()} and \code{find_peaks()}, while the default is \code{FALSE}
#'   in \code{stat_peaks()} and in \code{stat_valleys()}.
#'
#' @note These statistics check the scale of the \code{x} aesthetic and if it is
#'   Date or Datetime they correctly generate the labels by transforming the
#'   numeric \code{x} values to Date or POSIXct objects, respectively. In which
#'   case the \code{x.label.fmt} must follow the syntax supported by
#'   \code{strftime()} rather than by \code{sprintf()}. Overlap of labels with
#'   points can be avoided by use of one of the nudge positions, possibly together
#'   with geometry \code{\link[ggpp]{geom_text_s}} from package
#'   \code{\link[ggpp]{ggpp}}, or with \code{\link[ggrepel]{geom_text_repel}} or
#'   \code{\link[ggrepel]{geom_label_repel}} from package
#'   \code{\link[ggrepel]{ggrepel}}. To discard overlapping labels use
#'   \code{check_overlap = TRUE} as argument to \code{geom_text} or
#'   \code{geom_text_s}. By default the labels are character values suitable to
#'   be plotted as is, but with a suitable format passed as argument to
#'   \code{label.fmt} labels suitable for parsing by the geoms (e.g., into
#'   expressions containing Greek letters, super- or subscripts, maths symbols
#'   or maths constructs) can be also easily obtained.
#'
#' @section Warning!: The current version of these statistics does not support
#'   passing \code{nudge_x} or \code{nurge_y} named parameters to the geometry.
#'   Instead, use as an argument to parameter `position` one of the position
#'   functions such as \code{\link[ggpp]{position_nudge_keep}}.
#'
#' @seealso [find_peaks()]
#'
#' @examples
#' # lynx is a time.series object
#' lynx_num.df <-
#'   try_tibble(lynx,
#'              col.names = c("year", "lynx"),
#'              as.numeric = TRUE) # years -> as numeric
#'
#' ggplot(lynx_num.df, aes(year, lynx)) +
#'   geom_line() +
#'   stat_peaks(colour = "red") +
#'   stat_valleys(colour = "blue")
#'
#' ggplot(lynx_num.df, aes(lynx, year)) +
#'   geom_line(orientation = "y") +
#'   stat_peaks(colour = "red", orientation = "y") +
#'   stat_valleys(colour = "blue", orientation = "y")
#'
#' ggplot(lynx_num.df, aes(year, lynx)) +
#'   geom_line() +
#'   stat_peaks(colour = "red") +
#'   stat_peaks(colour = "red", geom = "rug")
#'
#' ggplot(lynx_num.df, aes(year, lynx)) +
#'   geom_line() +
#'   stat_peaks(colour = "red") +
#'   stat_peaks(colour = "red", geom = "text", hjust = -0.1, angle = 33)
#'
#' ggplot(lynx_num.df, aes(lynx, year)) +
#'   geom_line(orientation = "y") +
#'   stat_peaks(colour = "red", orientation = "y") +
#'   stat_peaks(colour = "red", orientation = "y",
#'              geom = "text", hjust = -0.1)
#'
#' lynx_datetime.df <-
#'    try_tibble(lynx,
#'               col.names = c("year", "lynx")) # years -> POSIXct
#'
#' ggplot(lynx_datetime.df, aes(year, lynx)) +
#'   geom_line() +
#'   stat_peaks(colour = "red") +
#'   stat_valleys(colour = "blue")
#'
#' ggplot(lynx_datetime.df, aes(year, lynx)) +
#'   geom_line() +
#'   stat_peaks(colour = "red") +
#'   stat_peaks(colour = "red",
#'              geom = "text",
#'              hjust = -0.1,
#'              x.label.fmt = "%Y",
#'              angle = 33)
#'
#' ggplot(lynx_datetime.df, aes(year, lynx)) +
#'   geom_line() +
#'   stat_peaks(colour = "red") +
#'   stat_peaks(colour = "red",
#'              geom = "text_s",
#'              position = position_nudge_keep(x = 0, y = 200),
#'              hjust = -0.1,
#'              x.label.fmt = "%Y",
#'              angle = 90) +
#'   expand_limits(y = 8000)
#'
#' ggplot(lynx_datetime.df, aes(year, lynx)) +
#'   geom_line() +
#'   stat_peaks(colour = "red",
#'              geom = "text_s",
#'              position = position_nudge_to(y = 7600),
#'              arrow = arrow(length = grid::unit(1.5, "mm")),
#'              point.padding = 0.7,
#'              x.label.fmt = "%Y",
#'              angle = 90) +
#'   expand_limits(y = 9000)
#'
#' @export
#'
stat_peaks <- function(mapping = NULL,
                       data = NULL,
                       geom = "point",
                       position = "identity",
                       ...,
                       span = 5,
                       global.threshold = 0,
                       local.threshold = 0,
                       local.reference = "minimum",
                       strict = FALSE,
                       label.fmt = NULL,
                       x.label.fmt = NULL,
                       y.label.fmt = NULL,
                       orientation = "x",
                       na.rm = FALSE,
                       show.legend = FALSE,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatPeaks,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params =
      rlang::list2(
        span = span,
        global.threshold = global.threshold,
        local.threshold = local.threshold,
        local.reference = local.reference,
        strict = strict,
        label.fmt = label.fmt,
        x.label.fmt = x.label.fmt,
        y.label.fmt = y.label.fmt,
        orientation = orientation,
        na.rm = na.rm,
        ...
      )
  )
}

# Define here to avoid a note in check as the imports are not seen by checks
# when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
peaks_compute_group_fun <- function(data,
                                    scales,
                                    span = 5,
                                    global.threshold = 0,
                                    local.threshold = 0,
                                    local.reference = "minimum",
                                    strict = FALSE,
                                    label.fmt = NULL,
                                    x.label.fmt = NULL,
                                    y.label.fmt = NULL,
                                    flipped_aes = FALSE) {
  data <- ggplot2::flip_data(data, flipped_aes)
  if (!is.null(label.fmt)) {
    warning("Use of parameter 'label.format' is deprecated, ",
            "use parameters 'x.label.format' and 'y.label.format' instead.")
    if (is.null(x.label.fmt)) {
      x.label.fmt <- label.fmt
    }
    if (is.null(y.label.fmt)) {
      y.label.fmt <- label.fmt
    }
  } else if (is.null(y.label.fmt)) {
    y.label.fmt <- "%.4g"
  }
  if (inherits(scales$x, "ScaleContinuousDatetime")) {
    tzone <- scales$x$timezone
    if (is.null(tzone) || is.na(tzone)) {
      tzone <- ""
    }
    as_label <- function(fmt, x, tz = tzone) {
      x <- as.POSIXct(x,
                      origin = lubridate::origin,
                      tz = tz)
      strftime(x, fmt, tz = tz)
    }
    if (is.null(x.label.fmt)) {
      x.label.fmt <- "%Y-%m-%d"
    }
  } else if (inherits(scales$x, "ScaleContinuousDate")) {
    as_label <- function(fmt, x, tz = tzone) { # avoid note from tz = NULL
      x <- as.Date(x,
                   origin = lubridate::origin)
      strftime(x, fmt)
    }
    if (is.null(x.label.fmt)) {
      x.label.fmt <- "%Y-%m-%d"
    }
  } else {
    as_label <- function(fmt, x, tz = tzone) { # avoid note from tz = NULL
      sprintf(fmt, x)
    }
    if (is.null(x.label.fmt)) {
      x.label.fmt <- "%.4g"
    }
  }
  if (is.null(span)) {
    peaks.df <- data[which.max(data$y), , drop = FALSE]
  } else {
    # for span to work as expected the data should be in the order they
    # will be plotted
    data <- data[order(data$x), ]
    peaks.df <- data[find_peaks(data$y,
                                span = span,
                                global.threshold = global.threshold,
                                local.threshold = local.threshold,
                                local.reference = local.reference,
                                strict = strict,
                                na.rm = TRUE), , drop = FALSE]
  }

  peaks.df$flipped_aes <- flipped_aes
  peaks.df <- ggplot2::flip_data(peaks.df, flipped_aes)

  peaks.df[["x.label"]] <- as_label(x.label.fmt, peaks.df[["x"]])
  peaks.df[["y.label"]] <- sprintf(y.label.fmt, peaks.df[["y"]])
  peaks.df
}

# Define here to avoid a note in check as the imports are not seen by checks
# when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
valleys_compute_group_fun <- function(data,
                                      scales,
                                      span,
                                      global.threshold,
                                      local.threshold,
                                      local.reference,
                                      strict,
                                      label.fmt,
                                      x.label.fmt,
                                      y.label.fmt,
                                      flipped_aes = FALSE) {
  data <- ggplot2::flip_data(data, flipped_aes)
  if (!is.null(label.fmt)) {
    warning("Use of parameter 'label.format' is deprecated, ",
            "use parameters 'x.label.format' and 'y.label.format' instead.")
    if (is.null(x.label.fmt)) {
      x.label.fmt <- label.fmt
    }
    if (is.null(y.label.fmt)) {
      y.label.fmt <- label.fmt
    }
  } else if (is.null(y.label.fmt)) {
    y.label.fmt <- "%.4g"
  }
  if (inherits(scales$x, "ScaleContinuousDatetime")) {
    tzone <- scales$x$timezone
    if (is.null(tzone) || is.na(tzone)) {
      tzone <- ""
    }
    as_label <- function(fmt, x, tz = tzone) {
      x <- as.POSIXct(x,
                      origin = lubridate::origin,
                      tz = tz)
      strftime(x, fmt, tz = tz)
    }
    if (is.null(x.label.fmt)) {
      x.label.fmt <- "%Y-%m-%d"
    }
  } else if (inherits(scales$x, "ScaleContinuousDate")) {
    as_label <- function(fmt, x, tz = tzone) { # avoid note from tz = NULL
      x <- as.Date(x, origin = lubridate::origin)
      strftime(x, fmt)
    }
    if (is.null(x.label.fmt)) {
      x.label.fmt <- "%Y-%m-%d"
    }
  } else {
    as_label <- function(fmt, x, tz = tzone) { # avoid note from tz = NULL
      sprintf(fmt, x)
    }
    if (is.null(x.label.fmt)) {
      x.label.fmt <- "%.4g"
    }
  }
  if (is.null(span)) {
    valleys.df <- data[which.min(data$y), , drop = FALSE]
  } else {
    # for span to work as expected the data should be in the order they
    # will be plotted
    data <- data[order(data$x), ]
    # we search for peaks in -y
    if (local.reference == "maximum") {
      local.reference <- "minimum"
    }
    valleys.df <- data[find_peaks(-data$y,
                                  span = span,
                                  global.threshold = global.threshold,
                                  local.threshold = local.threshold,
                                  local.reference = local.reference,
                                  strict = strict,
                                  na.rm = TRUE), , drop = FALSE]
  }

  valleys.df$flipped_aes <- flipped_aes
  valleys.df <- ggplot2::flip_data(valleys.df, flipped_aes)

  valleys.df[["x.label"]] <- as_label(x.label.fmt, valleys.df[["x"]])
  valleys.df[["y.label"]] <- sprintf(y.label.fmt, valleys.df[["y"]])
  valleys.df
}

#' \code{Stat*} Objects
#'
#' All \code{stat_*} functions (like \code{stat_bin}) return a layer that
#' contains a \code{Stat*} object (like \code{StatBin}). The \code{Stat*}
#' object is responsible for rendering the data in the plot.
#'
#' Each of the \code{Stat*} objects is a \code{\link[ggplot2]{ggproto}} object,
#' derived from the top-level \code{Stat}, and each implements various methods
#' and fields. To create a new type of Stat object, you typically will want to
#' implement one or more of the following:
#'
#' @name Stats
#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @seealso \code{\link[ggplot2]{ggplot2-ggproto}}
#' @keywords internal
StatPeaks <-
  ggplot2::ggproto("StatPeaks", ggplot2::Stat,
                   setup_params = function(data, params) {
                     params$flipped_aes <- ggplot2::has_flipped_aes(data, params)

                     has_x <- !(is.null(data$x) && is.null(params$x))
                     has_y <- !(is.null(data$y) && is.null(params$y))
                     if (!has_x && !has_y) {
                       rlang::abort("stat_density() requires an x or y aesthetic.")
                     }

                     params
                   },
                   extra_params = c("na.rm", "orientation"),
                   compute_group = peaks_compute_group_fun,
                   default_aes = ggplot2::aes(label = after_stat(x.label),
                                              xintercept = after_stat(x),
                                              yintercept = after_stat(y)),
                   required_aes = c("x", "y")
  )

#' @rdname stat_peaks
#'
#' @export
#'
stat_valleys <- function(mapping = NULL,
                         data = NULL,
                         geom = "point",
                         position = "identity",
                         ...,
                         span = 5,
                         global.threshold = 0,
                         local.threshold = 0,
                         local.reference = "maximum",
                         strict = FALSE,
                         label.fmt = NULL,
                         x.label.fmt = NULL,
                         y.label.fmt = NULL,
                         orientation = "x",
                         na.rm = FALSE,
                         show.legend = FALSE,
                         inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatValleys,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params =
      rlang::list2(
        span = span,
        global.threshold = global.threshold,
        local.threshold = local.threshold,
        local.reference = local.reference,
        strict = strict,
        label.fmt = label.fmt,
        x.label.fmt = x.label.fmt,
        y.label.fmt = y.label.fmt,
        orientation = orientation,
        na.rm = na.rm,
        ...
      )
  )
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#'
#' @export
#'
StatValleys <-
  ggplot2::ggproto("StatValleys", ggplot2::Stat,
                   setup_params = function(data, params) {
                     params$flipped_aes <- ggplot2::has_flipped_aes(data, params)

                     has_x <- !(is.null(data$x) && is.null(params$x))
                     has_y <- !(is.null(data$y) && is.null(params$y))
                     if (!has_x && !has_y) {
                       rlang::abort("stat_density() requires an x or y aesthetic.")
                     }

                     params
                   },
                   extra_params = c("na.rm", "orientation"),
                   compute_group = valleys_compute_group_fun,
                   default_aes = ggplot2::aes(label = after_stat(x.label),
                                              xintercept = after_stat(x),
                                              yintercept = after_stat(y)),
                   required_aes = c("x", "y")
  )
