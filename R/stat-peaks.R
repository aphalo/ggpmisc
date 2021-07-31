#' Find local maxima or global maximum (peaks)
#'
#' This method finds peaks (local maxima) in a spectrum, using a user selectable
#' span and size threshold relative to the tallest peak (global maximum). This
#' is a wrapper built on top of function peaks from package 'splus2R'.
#'
#' @param x numeric array
#' @param ignore_threshold numeric value between 0.0 and 1.0 indicating the size
#'   threshold below which peaks will be ignored.
#' @param span a peak is defined as an element in a sequence which is greater
#'   than all other elements within a window of width span centered at that
#'   element. The default value is 3, meaning that a peak is bigger than both of
#'   its neighbors. Default: 3.
#' @param strict logical flag: if TRUE, an element must be strictly greater than
#'   all other values in its window to be considered a peak. Default: TRUE.
#' @param na.rm logical indicating whether \code{NA} values should be stripped
#'   before searching for peaks.
#'
#' @return A vector of logical values. Values that are TRUE
#'   correspond to local peaks in the data.
#'
#' @details This function is a wrapper built on function
#'   \code{\link[splus2R]{peaks}} from \pkg{splus2R} and handles non-finite
#'   (including NA) values differently than \code{peaks}, instead of giving an
#'   error they are replaced with the smallest finite value in \code{x}.
#'
#' @note The default for parameter \code{strict} is \code{TRUE} in functions
#'   \code{peaks()} and \code{find_peaks()}, while the default is \code{FALSE}
#'   in \code{stat_peaks()} and in \code{stat_valleys()}.
#'
#' @seealso \code{\link[splus2R]{peaks}}
#'
#' @family peaks and valleys functions
#'
#' @keywords internal
#'
find_peaks <-
  function(x,
           ignore_threshold = 0,
           span = 3,
           strict = TRUE,
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
    # apply threshold to found peaks
    if (abs(ignore_threshold) < 1e-5) {
      pks
    } else {
      range_x <- range(x, na.rm = na.rm, finite = TRUE)
      min_x <- range_x[1]
      max_x <- range_x[2]
      x <- ifelse(!is.finite(x), min_x, x)
      # this can cater for the case when max_x < 0, as with logs
      delta <- max_x - min_x
      top_flag <- ignore_threshold > 0.0
      scaled_threshold <- delta * abs(ignore_threshold)
      if (top_flag) {
        ifelse(x - min_x > scaled_threshold, pks , FALSE)
      } else {
        ifelse(max_x - x > scaled_threshold, pks , FALSE)
      }
    }
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
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
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
#' @param ignore_threshold numeric value between 0.0 and 1.0 indicating the size
#'   threshold below which peaks will be ignored.
#' @param span a peak is defined as an element in a sequence which is greater
#'   than all other elements within a window of width span centered at that
#'   element. The default value is 5, meaning that a peak is bigger than two
#'   consecutive neighbors on each side. A \code{NULL} value for \code{span}
#'   is taken as a span covering the whole of the data range.
#' @param strict logical flag: if TRUE, an element must be strictly greater than
#'   all other values in its window to be considered a peak. Default: FALSE.
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
#' @note These stats check the scale of the \code{x} aesthetic and if it is Date
#'   or Datetime they correctly generate the labels by transforming the numeric
#'   \code{x} values to Date or POSIXct objects, respectively. In which case the
#'   \code{x.label.fmt} must follow the syntax supported by \code{strftime()}
#'   rather than by \code{sprintf()}. These stats work nicely together with
#'   geoms \code{\link[ggrepel]{geom_text_repel}} and
#'   \code{\link[ggrepel]{geom_label_repel}} from package
#'   \code{\link[ggrepel]{ggrepel}} to solve the problem of overlapping labels
#'   by displacing them. Alternatively, to discard overlapping labels use
#'   \code{check_overlap = TRUE} as argument to \code{geom_text}. By default the
#'   labels are character values suitable to be plotted as is, but with a
#'   suitable format passed as argument to \code{label.fmt} labels suitable for
#'   parsing by the geoms (e.g. into expressions containing Greek letters,
#'   super- or subscripts, maths symbols or maths constructs) can be also easily
#'   obtained.
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
#'              geom = "text_linked",
#'              position = position_nudge_keep(x = 0, y = 200),
#'              hjust = -0.1,
#'              x.label.fmt = "%Y",
#'              angle = 90) +
#'   expand_limits(y = 8000)
#'
#' ggplot(lynx_datetime.df, aes(year, lynx)) +
#'   geom_line() +
#'   stat_peaks(colour = "red",
#'              geom = "text_linked",
#'              position = position_nudge_to(y = 7200),
#'              arrow = arrow(length = grid::unit(1.5, "mm")),
#'              hjust = -0.1,
#'              x.label.fmt = "%Y",
#'              angle = 90) +
#'   expand_limits(y = 8000)
#'
#' @export
#' @family peaks and valleys functions
#'
stat_peaks <- function(mapping = NULL,
                       data = NULL,
                       geom = "point",
                       span = 5,
                       ignore_threshold = 0,
                       strict = FALSE,
                       label.fmt = NULL,
                       x.label.fmt = NULL,
                       y.label.fmt = NULL,
                       position = "identity",
                       na.rm = FALSE,
                       show.legend = FALSE,
                       inherit.aes = TRUE,
                       ...) {
  ggplot2::layer(
    stat = StatPeaks,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      span = span,
      ignore_threshold = ignore_threshold,
      strict = strict,
      label.fmt = label.fmt,
      x.label.fmt = x.label.fmt,
      y.label.fmt = y.label.fmt,
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
                                    span,
                                    ignore_threshold,
                                    strict,
                                    label.fmt,
                                    x.label.fmt,
                                    y.label.fmt) {
  force(data)
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
    as_label <- function(fmt, x) {
      x <- as.POSIXct(x,
                      origin = lubridate::origin)
      strftime(x, fmt)
    }
    if (is.null(x.label.fmt)) {
      x.label.fmt <- "%Y-%m-%d"
    }
  } else if (inherits(scales$x, "ScaleContinuousDate")) {
    as_label <- function(fmt, x) {
      x <- as.Date(x, origin = lubridate::origin)
      strftime(x, fmt)
    }
    if (is.null(x.label.fmt)) {
      x.label.fmt <- "%Y-%m-%d"
    }
  } else {
    as_label <- function(fmt, x) {
      sprintf(fmt, x)
    }
    if (is.null(x.label.fmt)) {
      x.label.fmt <- "%.4g"
    }
  }
  if (is.null(span)) {
    peaks.df <- data[which.max(data$y), , drop = FALSE]
  } else {
    peaks.df <- data[find_peaks(data$y,
                                span = span,
                                ignore_threshold = ignore_threshold,
                                strict = strict,
                                na.rm = TRUE), , drop = FALSE]
  }
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
                                      ignore_threshold,
                                      strict,
                                      label.fmt,
                                      x.label.fmt,
                                      y.label.fmt) {
  force(data)
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
    as_label <- function(fmt, x) {
      x <- as.POSIXct(x,
                      origin = lubridate::origin)
      strftime(x, fmt)
    }
    if (is.null(x.label.fmt)) {
      x.label.fmt <- "%Y-%m-%d"
    }
  } else if (inherits(scales$x, "ScaleContinuousDate")) {
    as_label <- function(fmt, x) {
      x <- as.Date(x, origin = lubridate::origin)
      strftime(x, fmt)
    }
    if (is.null(x.label.fmt)) {
      x.label.fmt <- "%Y-%m-%d"
    }
  } else {
    as_label <- function(fmt, x) {
      sprintf(fmt, x)
    }
    if (is.null(x.label.fmt)) {
      x.label.fmt <- "%.4g"
    }
  }
  if (is.null(span)) {
    valleys.df <- data[which.min(data$y), , drop = FALSE]
  } else {
    valleys.df <- data[find_peaks(-data$y,
                                  span = span,
                                  ignore_threshold = ignore_threshold,
                                  strict = strict,
                                  na.rm = TRUE), , drop = FALSE]
  }
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
                         span = 5,
                         ignore_threshold = 0,
                         strict = FALSE,
                         label.fmt = NULL,
                         x.label.fmt = NULL,
                         y.label.fmt = NULL,
                         position = "identity",
                         na.rm = FALSE,
                         show.legend = FALSE,
                         inherit.aes = TRUE,
                         ...) {
  ggplot2::layer(
    stat = StatValleys,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      span = span,
      ignore_threshold = ignore_threshold,
      strict = strict,
      label.fmt = label.fmt,
      x.label.fmt = x.label.fmt,
      y.label.fmt = y.label.fmt,
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
                   compute_group = valleys_compute_group_fun,
                   default_aes = ggplot2::aes(label = after_stat(x.label),
                                              xintercept = after_stat(x),
                                              yintercept = after_stat(y)),
                   required_aes = c("x", "y")
  )
