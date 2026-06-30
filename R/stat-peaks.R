#' Local maxima (peaks) or minima (valleys)
#'
#' \code{stat_peaks()} tags or extracts rows in \code{data} containing local
#' or global maxima of \code{y}.
#' \code{stat_valleys} tags or extracts rows in \code{data} containing local
#' or global minima of \code{y}. They make it
#' easy to highlight and label peaks and valleys based on their \code{x} and/or \code{y}
#' coordinates. Orientations flipping as well as dates and times are
#' supported.
#'
#' @inheritParams find_peaks
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points on this
#'   layer
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical value indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param label.fmt,x.label.fmt,y.label.fmt character strings giving a format
#'   definition for construction of character strings labels with function
#'   \code{\link{sprintf}} from \code{x} and/or \code{y} values.
#' @param extract.peaks,extract.valleys If \code{TRUE} only the rows containing
#'   peaks or valleys are returned. If \code{FALSE} the whole of \code{data} is
#'   returned but with labels set to \code{""} in rows not containing peaks or
#'   valleys. If \code{NULL}, the default, \code{TRUE}, is used unless the
#'   argument passed to \code{geom} is \code{"text_repel"}, \code{"label_repel"}
#'   or \code{"marquee_repel"}.
#' @param orientation character The orientation of the layer can be set to
#'   either \code{"x"}, the default, or \code{"y"}.
#'
#' @aesthetics StatPeaks
#' @aesthetics StatValleys
#'
#' @return A data frame with one row for each peak (or valley) found in the data
#'   extracted from the input \code{data} or all rows in data. Added columns
#'   contain the labels.
#'
#' @section Computed and copied variables in the returned data frame:
#' \describe{
#'   \item{x}{x-value at the peak (or valley) as numeric.}
#'   \item{y}{y-value at the peak (or valley) as numeric.}
#'   \item{x.label}{x-value at the peak (or valley) formatted as character.}
#'   \item{y.label}{y-value at the peak (or valley) formatted as character.}
#'   \item{is.peak/is.valley}{logical vector, \code{TRUE} at peaks or valleys.}
#' }
#'
#' @inherit find_peaks details
#'
#' @section Label positioning and formatting: \code{stat_peaks()},
#'   \code{stat_valleys()} and \code{stat_spikes()} work nicely together with
#'   geoms \code{geom_text_repel()}, \code{geom_label_repel()}, and
#'   \code{geom_marquee_repel()} from package \code{\link[ggrepel]{ggrepel}} to
#'   solve the problem of overlapping labels by displacing them. If using
#'   \code{geom_text()}, discard overlapping labels using
#'   \code{check_overlap = TRUE}.
#'
#'   By default the labels are character values ready to be ploted as plain
#'   text, but with a suitable \code{label.fmt} argument, labels formatted as
#'   \code{\link[grDevices]{plotmath}} expressions, markdown or LaTeX can be
#'   created (e.g., containing Greek letters or super or subscripts, maths or
#'   colour) can be generated for use with geoms from packages 'marquee',
#'   'ggtext' and 'xdvir'.
#'
#'   The default is \code{geom = "point"} it is likely to work well in almost
#'   any situation. The default aesthetics mappings set by these stats allow
#'   their direct use with \code{geom_text()}, \code{geom_label()},
#'   \code{geom_line()}, \code{geom_rug()}, \code{geom_hline()} and
#'   \code{geom_vline()} by just passing an argument to \code{geom}.
#'
#' @seealso \code{\link{find_peaks}()}, for the function used to located the
#'   peaks and valleys.
#'
#' @family statistics for peak, valley and spikes annotation
#'
#' @examples
#' # lynx and Nile are time.series objects recognized by
#' # ggpp::ggplot.ts() and converted on-the-fly with a default mapping
#'
#' # numeric, date times and dates are supported with data frames
#'
#' # using defaults
#' ggplot(Nile) +
#'   geom_line() +
#'   stat_peaks(colour = "red") +
#'   stat_valleys(colour = "blue")
#'
#' # using wider window
#' ggplot(Nile) +
#'   geom_line() +
#'   stat_peaks(colour = "red", span = 11) +
#'   stat_valleys(colour = "blue", span = 11)
#'
#' # global threshold for peak height
#' ggplot(Nile) +
#'   geom_line() +
#'   stat_peaks(colour = "red",
#'              global.threshold = 0.5) # half of data range
#'
#' ggplot(Nile) +
#'   geom_line() +
#'   stat_peaks(colour = "red",
#'              global.threshold = I(1100)) + # data unit
#'              expand_limits(y = c(0, 1500))
#'
#' # local (within window) threshold for peak height
#' # narrow peaks at the tip and locally tall
#'
#' ggplot(Nile) +
#'   geom_line() +
#'   stat_peaks(colour = "red",
#'              span = 9,
#'              local.threshold = 0.3,
#'              local.reference = "farthest")
#'
#' # with narrower window
#' ggplot(Nile) +
#'   geom_line() +
#'   stat_peaks(colour = "red",
#'              span = 5,
#'              local.threshold = 0.25,
#'              local.reference = "farthest")
#'
#' ggplot(lynx) +
#'   geom_line() +
#'   stat_peaks(colour = "red",
#'              local.threshold = 1/5,
#'              local.reference = "median")
#'
#' ggplot(Nile) +
#'   geom_line() +
#'   stat_valleys(colour = "blue",
#'                global.threshold = I(700))
#'
#' # orientation is supported
#' ggplot(lynx, aes(lynx, time)) +
#'   geom_line(orientation = "y") +
#'   stat_peaks(colour = "red", orientation = "y") +
#'   stat_valleys(colour = "blue", orientation = "y")
#'
#' # default aesthetic mapping supports additional geoms
#' ggplot(lynx) +
#'   geom_line() +
#'   stat_peaks(colour = "red") +
#'   stat_peaks(colour = "red", geom = "rug")
#'
#' ggplot(lynx) +
#'   geom_line() +
#'   stat_peaks(colour = "red") +
#'   stat_peaks(colour = "red", geom = "text", hjust = -0.1, angle = 33)
#'
#' ggplot(lynx, aes(lynx, time)) +
#'   geom_line(orientation = "y") +
#'   stat_peaks(colour = "red", orientation = "y") +
#'   stat_peaks(colour = "red", orientation = "y",
#'              geom = "text", hjust = -0.1)
#'
#' # Force conversion of time series time into POSIXct date time
#' ggplot(lynx, as.numeric = FALSE) +
#'   geom_line() +
#'   stat_peaks(colour = "red") +
#'   stat_peaks(colour = "red",
#'              geom = "text",
#'              hjust = -0.1,
#'              x.label.fmt = "%Y",
#'              angle = 33)
#'
#' ggplot(Nile, as.numeric = FALSE) +
#'   geom_line() +
#'   stat_peaks(colour = "red") +
#'   stat_peaks(colour = "red",
#'              geom = "text_s",
#'              position = position_nudge_keep(x = 0, y = 60),
#'              hjust = -0.1,
#'              x.label.fmt = "%Y",
#'              angle = 90) +
#'   expand_limits(y = 2000)
#'
#' ggplot(lynx, as.numeric = FALSE) +
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
                       orientation = "x",
                       span = 5,
                       global.threshold = 0,
                       local.threshold = 0,
                       local.reference = "median",
                       strict = FALSE,
                       label.fmt = NULL,
                       x.label.fmt = label.fmt,
                       y.label.fmt = NULL,
                       extract.peaks = NULL,
                       na.rm = FALSE,
                       show.legend = FALSE,
                       inherit.aes = TRUE) {

  if (is.null(extract.peaks)) {
    extract.peaks <- !grepl("^text_repel$|^label_repel$", "geom")
  }
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
        x.label.fmt = x.label.fmt,
        y.label.fmt = y.label.fmt,
        extract.peaks = extract.peaks,
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
                                    global.threshold = 0.01,
                                    local.threshold = NULL,
                                    local.reference = "median",
                                    strict = FALSE,
                                    x.label.fmt = NULL,
                                    y.label.fmt = NULL,
                                    extract.peaks = TRUE,
                                    flipped_aes = FALSE) {
  data <- ggplot2::flip_data(data, flipped_aes)
  if (is.null(y.label.fmt)) {
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
  if (is.null(span) || span >= nrow(data)) {
    peaks.selector <- data$y == max(data$y)
  } else {
    # for span to work as expected the data should be in the order they
    # will be plotted
    data <- data[order(data$x), ]

    peaks.selector <- find_peaks(data$y,
                                 span = span,
                                 global.threshold = global.threshold,
                                 local.threshold = local.threshold,
                                 local.reference = local.reference,
                                 strict = strict,
                                 na.rm = TRUE)
  }
  peaks.df <- data
  peaks.df$is.peak <- peaks.selector

  if (extract.peaks) {
    peaks.df <- peaks.df[peaks.df$is.peak, , drop = FALSE]
  }

  if (nrow(peaks.df)) {
    peaks.df$flipped_aes <- flipped_aes
    peaks.df <- ggplot2::flip_data(peaks.df, flipped_aes)

    peaks.df[["x.label"]] <- ifelse(peaks.df$is.peak,
                                    as_label(x.label.fmt, peaks.df[["x"]]),
                                    "")
    peaks.df[["y.label"]] <- ifelse(peaks.df$is.peak,
                                    sprintf(y.label.fmt, peaks.df[["y"]]),
                                    "")
    z <- peaks.df
  } else {
    z <- data.frame()
  }

  show_labels(z, stat.name = "stat_peaks")

  z
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
                                      span = 5,
                                      global.threshold = 0.01,
                                      local.threshold = NULL,
                                      local.reference = "median",
                                      strict = FALSE,
                                      x.label.fmt = NULL,
                                      y.label.fmt = NULL,
                                      extract.valleys = TRUE,
                                      flipped_aes = FALSE) {
  data <- ggplot2::flip_data(data, flipped_aes)
  if (is.null(y.label.fmt)) {
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
  if (is.null(span) || span >= nrow(data)) {
    valleys.selector <- data$y == min(data$y)
  } else {
    # for span to work as expected the data should be in the order they
    # will be plotted
    data <- data[order(data$x), ]

    valleys.selector <- find_valleys(data$y,
                                     span = span,
                                     global.threshold = global.threshold,
                                     local.threshold = local.threshold,
                                     local.reference = local.reference,
                                     strict = strict,
                                     na.rm = TRUE)
  }
  valleys.df <- data
  valleys.df$is.valley <- valleys.selector

  if (extract.valleys) {
    valleys.df <- valleys.df[valleys.df$is.valley, , drop = FALSE]
  }

  if (nrow(valleys.df)) {
    valleys.df$flipped_aes <- flipped_aes
    valleys.df <- ggplot2::flip_data(valleys.df, flipped_aes)

    valleys.df[["x.label"]] <- ifelse(valleys.df$is.valley,
                                      as_label(x.label.fmt, valleys.df[["x"]]),
                                      "")
    valleys.df[["y.label"]] <- ifelse(valleys.df$is.valley,
                                      sprintf(y.label.fmt, valleys.df[["y"]]),
                                      "")
    z <- valleys.df
  } else {
    z <- data.frame()
  }

  show_labels(z, stat.name = "stat_valleys")

  z
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
#' @family statistics for peak, valley and spikes annotation
#'
#' @export
#'
stat_valleys <- function(mapping = NULL,
                         data = NULL,
                         geom = "point",
                         position = "identity",
                         ...,
                         orientation = "x",
                         span = 5,
                         global.threshold = 0.01,
                         local.threshold = NULL,
                         local.reference = "median",
                         strict = FALSE,
                         label.fmt = NULL,
                         x.label.fmt = label.fmt,
                         y.label.fmt = NULL,
                         extract.valleys = NULL,
                         na.rm = FALSE,
                         show.legend = FALSE,
                         inherit.aes = TRUE) {
   if (is.null(extract.valleys)) {
     extract.valleys <- !grepl("^text_repel$|^label_repel$", "geom")
   }
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
        x.label.fmt = x.label.fmt,
        y.label.fmt = y.label.fmt,
        extract.valleys = extract.valleys,
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
                     params
                   },
                   extra_params = c("na.rm", "orientation"),
                   compute_group = valleys_compute_group_fun,
                   default_aes = ggplot2::aes(label = after_stat(x.label),
                                              xintercept = after_stat(x),
                                              yintercept = after_stat(y)),
                   required_aes = c("x", "y")
  )
