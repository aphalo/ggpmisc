#' Find local maxima (peaks)
#'
#' This function finds all peaks (local maxima) in a spectrum, using a user
#' selectable size threshold relative to the tallest peak (global maximum). This
#' is a wrapper built on top of function peaks from package splus2R.
#'
#' @param x numeric array
#' @param ignore_threshold numeric value between 0.0 and 1.0 indicating the size
#'   threshold below which peaks will be ignored.
#' @param span a peak is defined as an element in a sequence which is greater
#'   than all other elements within a window of width span centered at that
#'   element. The default value is 3, meaning that a peak is bigger than both of
#'   its neighbors. Default: 3.
#' @param strict ogical flag: if TRUE, an element must be strictly greater than
#'   all other values in its window to be considered a peak. Default: TRUE.
#'
#' @return A vector of logical values. Values that are TRUE
#'   correspond to local peaks in the data.
#'
#' @details This function is a wrapper built on function
#'   \code{\link[splus2R]{peaks}} from \pkg{splus2R} and handles non-finite
#'   (including NA) values differently than \code{peaks}, instead of giving an
#'   error they are replaced with the smallest finite value in \code{x}.
#'
#' @seealso \code{\link[splus2R]{peaks}}
#'
#' @family peaks and valleys functions
#'
#' @keywords internal
#'
find_peaks <-
  function(x,
           ignore_threshold = 0.0,
           span = 3,
           strict = TRUE) {
    range_x <- range(x, finite = TRUE)
    min_x <- range_x[1]
    max_x <- range_x[2]
    x <- ifelse(!is.finite(x), min_x, x)
    # the next two lines catter for the case when max_x < 0, which is quite
    # common with logs
    delta <- max_x - min_x
    top_flag <- ignore_threshold > 0.0
    scaled_threshold <- delta * abs(ignore_threshold)
    pks <- splus2R::peaks(x = x, span = span, strict = strict)
    if (abs(ignore_threshold) < 1e-5)
      return(pks)
    if (top_flag) {
      return(ifelse(x - min_x > scaled_threshold, pks , FALSE))
    } else {
      return(ifelse(max_x - x > scaled_threshold, pks , FALSE))
    }
  }

#' Find peaks and valleys.
#'
#' \code{stat_peaks} finds at which x positions of local y maxima are located and
#' \code{stat_valleys} finds at which x positions local y minima are located.
#' Both stats return x and y numeric values for the peaks and formatted
#' character labels. The formatting is determined by a format string suitable
#' for \code{sprintf()}.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_string}}. Only needs
#'   to be set
#'    at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'    the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points
#'    on this layer
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This can
#'   include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical value indicating whether NA values should be
#'   stripped before the computation proceeds.
#' @param ignore_threshold numeric value between 0.0 and 1.0 indicating the size
#'   threshold below which peaks will be ignored.
#' @param span a peak is defined as an element in a sequence which is greater
#'   than all other elements within a window of width span centered at that
#'   element. The default value is 5, meaning that a peak is bigger than two
#'   consequtive neighbors on each side. A \code{NULL} value for \code{span}
#'   is taken as a span covering the whole of the data range.
#' @param strict logical flag: if TRUE, an element must be strictly greater than
#'   all other values in its window to be considered a peak. Default: FALSE.
#' @param label.fmt character  string giving a format definition for converting
#'   values into character strings by means of function \code{\link{sprintf}}.
#' @param x.label.fmt character  string giving a format definition for converting
#'   $x$-values into character strings by means of function \code{\link{sprintf}}
#'   or \code{\link{strftime}}.
#' @param y.label.fmt character  string giving a format definition for converting
#'   $y$-values into character strings by means of function \code{\link{sprintf}}.
#'
#' @section Computed variables:
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
#' @note This stats check the scale of the \code{x} aesthetic and if is Datetime it
#'   correctly generates the labels by transforming the numeric x values to
#'   POSIXct objects, in which case the \code{x.label.fmt} must be suitable for
#'   \code{strftime()} rather than for \code{sprintf()}.
#' These stats work nicely together with geoms
#'   \code{\link[ggrepel]{geom_text_repel}} and
#'   \code{\link[ggrepel]{geom_label_repel}} from package
#'   \code{\link[ggrepel]{ggrepel}} to solve the problem of overlapping labels
#'   by displacing them. To discard overlapping labels use \code{check_overlap =
#'   TRUE} as argument to \code{geom_text}.
#'  By default the labels are character values suitable to be plotted as is, but
#'  with a suitable \code{label.fmt} labels suitable for parsing by the geoms
#'  (e.g. into expressions containing greek letters or super or subscripts) can
#'  be also easily obtained.
#'
#' @examples
#' library(ggplot2)
#' lynx.df <- data.frame(year = as.numeric(time(lynx)), lynx = as.matrix(lynx))
#' ggplot(lynx.df, aes(year, lynx)) + geom_line() +
#'   stat_peaks(colour = "red")
#' ggplot(lynx.df, aes(year, lynx)) + geom_line() +
#'   stat_peaks(colour = "red") +
#'   stat_peaks(colour = "red", geom = "rug")
#'
#' @export
#' @family peaks and valleys functions
#'
stat_peaks <- function(mapping = NULL, data = NULL, geom = "point",
                       span = 5, ignore_threshold = 0, strict = FALSE,
                       label.fmt = "%.4g",
                       x.label.fmt = NULL, y.label.fmt = label.fmt,
                       position = "identity", na.rm = FALSE, show.legend = FALSE,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatPeaks, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(span = span,
                  ignore_threshold = ignore_threshold,
                  strict = strict,
                  label.fmt = label.fmt,
                  x.label.fmt = x.label.fmt,
                  y.label.fmt = y.label.fmt,
                  na.rm = na.rm,
                  ...)
  )
}

#' \code{Stat*} Objects
#'
#' All \code{stat_*} functions (like \code{stat_bin}) return a layer that
#' contains a \code{Stat*} object (like \code{StatBin}). The \code{Stat*}
#' object is responsible for rendering the data in the plot.
#'
#' Each of the \code{Stat*} objects is a \code{\link[ggplot2]{ggproto}} object, descended
#' from the top-level \code{Stat}, and each implements various methods and
#' fields. To create a new type of Stat object, you typically will want to
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
                   compute_group = function(data,
                                            scales,
                                            span,
                                            ignore_threshold,
                                            strict,
                                            label.fmt,
                                            x.label.fmt,
                                            y.label.fmt) {
                     if (inherits(scales$x, c("ScaleContinuousDatetime",
                                              "ScaleContinuousDate"))) {
                       as_label <- function(fmt, x) {
                         x <- as.POSIXct(x,
                                    origin = lubridate::origin)
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
                         x.label.fmt <- label.fmt
                       }
                     }
                     if (is.null(span)) {
                       peaks.df <- data[which.max(data$y), , drop = FALSE]
                     } else {
                       peaks.df <- data[find_peaks(data$y,
                                        span = span,
                                        ignore_threshold = ignore_threshold,
                                        strict = strict), , drop = FALSE]
                     }
                     dplyr::mutate(peaks.df,
                                   x.label = as_label(x.label.fmt, x),
                                   y.label = sprintf(y.label.fmt, y))
                   },
                   default_aes = ggplot2::aes(label = ..x.label..,
                                              xintercept = ..x..,
                                              yintercept = ..y..),
                   required_aes = c("x", "y")
  )

#' @rdname stat_peaks
#'
#' @export
#'
stat_valleys <- function(mapping = NULL, data = NULL, geom = "point",
                         span = 5, ignore_threshold = 0, strict = FALSE,
                         label.fmt = "%.4g",
                         x.label.fmt = NULL, y.label.fmt = label.fmt,
                         position = "identity", na.rm = FALSE, show.legend = FALSE,
                         inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatValleys, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(span = span,
                  ignore_threshold = ignore_threshold,
                  strict = strict,
                  label.fmt = label.fmt,
                  x.label.fmt = x.label.fmt,
                  y.label.fmt = y.label.fmt,
                  na.rm = na.rm,
                  ...)
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
                   compute_group = function(data,
                                            scales,
                                            span,
                                            ignore_threshold,
                                            strict,
                                            label.fmt,
                                            x.label.fmt,
                                            y.label.fmt) {
                     if (inherits(scales$x, c("ScaleContinuousDatetime",
                                              "ScaleContinuousDate"))) {
                       as_label <- function(fmt, x) {
                         x <- as.POSIXct(x,
                                         origin = lubridate::origin)
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
                         x.label.fmt <- label.fmt
                       }
                     }
                     if (is.null(span)) {
                       valleys.df <- data[which.min(data$y), , drop = FALSE]
                     } else {
                       valleys.df <- data[find_peaks(-data$y,
                                          span = span,
                                          ignore_threshold = ignore_threshold,
                                          strict = strict), , drop = FALSE]
                     }
                     dplyr::mutate(valleys.df,
                                   x.label = as_label(x.label.fmt, x),
                                   y.label = sprintf(y.label.fmt, y))
                   },
                   default_aes = ggplot2::aes(label = ..x.label..,
                                              xintercept = ..x..,
                                              yintercept = ..y..),
                   required_aes = c("x", "y")
  )

