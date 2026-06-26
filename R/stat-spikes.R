#' Local narrow maxima or minima (spikes)
#'
#' \code{stat_spikes()} tags or extracts rows in \code{data} containing local
#' \code{y} narrow maxima and/or minima with very steep shoulders. It makes it
#' possible to highlight and label spikes based on their \code{x} and/or
#' \code{y} coordinates. Orientations flipping as well as dates and times are
#' supported.
#'
#' @inheritParams stat_peaks
#' @inheritParams find_spikes
#' @param extract.spikes If \code{TRUE} only the rows containing
#'   spikes are returned. If \code{FALSE} the whole of \code{data} is
#'   returned but with labels set to \code{""} in rows not containing spikes.
#'   If \code{NULL}, the default, \code{TRUE}, is used unless the argument
#'   passed to \code{geom} is \code{"text_repel"}, \code{"label_repel"}
#'   or \code{"marquee_repel"}.
#'
#' @aesthetics StatSpikes
#'
#' @return A data frame with one row for each spike found in the data
#'   extracted from the input \code{data} or all rows in data. Added columns
#'   contain the labels.
#'
#' @section Computed and copied variables in the returned data frame:
#' \describe{
#'   \item{x}{x-values at the spikes as numeric.}
#'   \item{y}{y-values at the spikes  as numeric.}
#'   \item{x.label}{x-values at the spikes formatted as character.}
#'   \item{y.label}{y-values at the spikes formatted as character.}
#'   \item{is.spike}{integer vector of \code{0}, \code{1} or \code{-1}.}
#' }
#'
#' @inherit find_spikes details references
#'
#' @inheritSection stat_peaks Label positioning and formatting
#'
#' @seealso \code{\link{find_spikes}()}, for the function used to located the
#'   spikes.
#'
#' @family \emph{statistics} for peak, valley and spikes annotation
#'
#' @examples
#' # lynx and Nile are time.series objects recognized by
#' # ggpp::ggplot.ts() and converted on-the-fly with a default mapping
#'
#' n = 500
#' set.seed(45678)
#' my.data <- data.frame(x = 1:n,
#'                       y = rep(sin((0:19)/20 * 2 * pi), n / 20) +
#'                           stats::rnorm(n, sd = 0.5))
#' selector <- sample(seq_len(n), 5)
#' my.data$y[selector] <- my.data$y[selector] + 10
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_line() +
#'   stat_spikes(colour = "orange")
#'
#' ggplot(my.data, aes(x, -y)) +
#'   geom_line() +
#'   stat_spikes(colour = "orange")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_line() +
#'   stat_spikes(geom = "text", vjust = -0.5) +
#'   stat_spikes(geom = "rug", colour = "red")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_line() +
#'   stat_spikes(colour = "red", spike.direction = "up") +
#'   stat_spikes(colour = "blue", spike.direction = "down")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_line() +
#'   stat_spikes(colour = "red", spike.direction = "up")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_line() +
#'   stat_spikes(colour = "blue", spike.direction = "down")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_line() +
#'   stat_spikes(z.threshold = 2, colour = "orange")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_line() +
#'   stat_spikes(z.threshold = 20, colour = "orange")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_line() +
#'   stat_spikes(colour = "red",
#'               spike.direction = "up",
#'               height.threshold = NA)
#'
#' # Inspecting the returned data using geom_debug_group()
#' # This provides a quick way of finding out the names of the variables that
#' # are available for mapping to aesthetics with after_stat().
#'
#' gginnards.installed <- requireNamespace("gginnards", quietly = TRUE)
#'
#' if (gginnards.installed)
#'   library(gginnards)
#'
#' if (gginnards.installed)
#' ggplot(my.data, aes(x, y)) +
#'   geom_line() +
#'   stat_spikes(geom = "debug_group")
#'
#' if (gginnards.installed)
#' ggplot(my.data, aes(x, y)) +
#'   geom_line() +
#'   stat_spikes(geom = "debug_group", extract.spikes = FALSE)
#'
#' @export
#'
stat_spikes <- function(mapping = NULL,
                        data = NULL,
                        geom = "point",
                        position = "identity",
                        ...,
                        orientation = "x",
                        height.threshold = 20,
                        z.threshold = 7,
                        k = 20,
                        spike.direction = "both",
                        label.fmt = NULL,
                        x.label.fmt = label.fmt,
                        y.label.fmt = NULL,
                        extract.spikes = NULL,
                        na.rm = FALSE,
                        show.legend = FALSE,
                        inherit.aes = TRUE) {

  if (is.null(extract.spikes)) {
    extract.spikes <- !grepl("^text_repel$|^label_repel$", "geom")
  }
  ggplot2::layer(
    stat = StatSpikes,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params =
      rlang::list2(
        height.threshold = height.threshold,
        z.threshold = z.threshold,
        k = k,
        spike.direction = spike.direction,
        x.label.fmt = x.label.fmt,
        y.label.fmt = y.label.fmt,
        extract.spikes = extract.spikes,
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
spikes_compute_group_fun <- function(data,
                                     scales,
                                     height.threshold = 10,
                                     z.threshold = 5,
                                     k = 20,
                                     spike.direction = "both",
                                     x.label.fmt = NULL,
                                     y.label.fmt = NULL,
                                     extract.spikes = TRUE,
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
  # for the algorithm to work as expected the data should be in the order they
  # will be plotted
  data <- data[order(data$x), ]

  spikes.selector <- find_spikes(data$y,
                                 x.is.delta = FALSE,
                                 height.threshold = height.threshold,
                                 z.threshold = z.threshold,
                                 k = k,
                                 spike.direction = spike.direction,
                                 na.rm = TRUE)

  spikes.df <- data
  spikes.df$is.spike <- spikes.selector

  if (extract.spikes) {
    spikes.df <- spikes.df[as.logical(spikes.df$is.spike), , drop = FALSE]
  }

  if (nrow(spikes.df)) {
    spikes.df$flipped_aes <- flipped_aes
    spikes.df <- ggplot2::flip_data(spikes.df, flipped_aes)

    spikes.df[["x.label"]] <- ifelse(spikes.df$is.spike,
                                    as_label(x.label.fmt, spikes.df[["x"]]),
                                    "")
    spikes.df[["y.label"]] <- ifelse(spikes.df$is.spike,
                                    sprintf(y.label.fmt, spikes.df[["y"]]),
                                    "")
    z <- spikes.df
  } else {
    z <- data.frame()
  }

  show_labels(z, stat.name = "stat_spikes")

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
StatSpikes <-
  ggplot2::ggproto("StatSpikes", ggplot2::Stat,
                   setup_params = function(data, params) {
                     params$flipped_aes <- ggplot2::has_flipped_aes(data, params)
                     params
                   },
                   extra_params = c("na.rm", "orientation"),
                   compute_group = spikes_compute_group_fun,
                   default_aes = ggplot2::aes(label = after_stat(x.label),
                                              xintercept = after_stat(x),
                                              yintercept = after_stat(y)),
                   required_aes = c("x", "y")
  )

