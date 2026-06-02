#' Local narrow maxima (spikes) or minima (spikes)
#'
#' \code{stat_spikes} finds at which \code{x} positions local \code{y} narrow
#' maxima are located.
#'
#' @inheritParams stat_peaks
#' @inheritParams find_spikes
#' @param extract.spikes If \code{TRUE} only the rows containing
#'   spikes are returned. If \code{FALSE} the whole of \code{data} is
#'   returned but with labels set to \code{NA} in rows not containing spikes.
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
#'   \item{x}{x-values at the spikes as numeric}
#'   \item{y}{y-values at the spikes  as numeric}
#'   \item{x.label}{x-values at the spikes formatted as character}
#'   \item{y.label}{y-values at the spikes formatted as character}
#' }
#'
#' @inherit find_spikes details
#'
#' @inheritSection stat_peaks Label positioning and formatting
#'
#' @seealso \code{\link{find_spikes}}, for the function used to located the
#'   spikes.
#'
#' @examples
#' # lynx and Nile are time.series objects recognized by
#' # ggpp::ggplot.ts() and converted on-the-fly with a default mapping
#'
#' set.seed(45678)
#' my.data <- data.frame(x = 1:1000,
#'                       y = 1:1000 + stats::rnorm(1000, sd = 5))
#' my.data$y2 <- my.data$y1 <- my.data$y
#' selector <- sample(seq_len(nrow(my.data)), 25)
#' selector <- unique(c(selector - 1L, selector, selector + 1L))
#' my.data$y[selector] <- my.data$y[selector] + 100
#' selector <- sample(seq_len(nrow(my.data)), 25)
#' my.data$y[selector] <- my.data$y[selector] - 100
#'
#' # using defaults for
#' ggplot(my.data, aes(x, y)) +
#'   geom_line() +
#'   stat_spikes(colour = "orange")
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
#' @export
#'
stat_spikes <- function(mapping = NULL,
                        data = NULL,
                        geom = "point",
                        position = "identity",
                        ...,
                        orientation = "x",
                        x.threshold = 0.02,
                        z.threshold = 5,
                        max.spike.width = NULL,
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
        x.threshold = x.threshold,
        z.threshold = z.threshold,
        max.spike.width = max.spike.width,
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
                                     x.threshold = 0.1,
                                     z.threshold = 9,
                                     max.spike.width = 8,
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
                                 x.threshold = x.threshold,
                                 z.threshold = z.threshold,
                                 max.spike.width = max.spike.width,
                                 spike.direction = spike.direction,
                                 na.rm = TRUE)

  spikes.df <- data
  spikes.df$is.spike <- spikes.selector

  if (extract.spikes) {
    spikes.df <- spikes.df[spikes.df$is.spike, , drop = FALSE]
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
    spikes.df
  } else {
    data.frame()
  }
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

                     has_x <- !(is.null(data$x) && is.null(params$x))
                     has_y <- !(is.null(data$y) && is.null(params$y))
                     if (!has_x && !has_y) {
                       rlang::abort("stat_spikes() requires both x and y aesthetics.")
                     }
                     params
                   },
                   extra_params = c("na.rm", "orientation"),
                   compute_group = spikes_compute_group_fun,
                   default_aes = ggplot2::aes(label = after_stat(x.label),
                                              xintercept = after_stat(x),
                                              yintercept = after_stat(y)),
                   required_aes = c("x", "y")
  )

