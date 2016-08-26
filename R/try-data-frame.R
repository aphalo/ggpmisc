#' Convert an R object containing observations into a tibble.
#'
#' This functions tries to convert any R object into a data.frame object.
#' If \code{x} is already a data.frame, it is returned as is. If it is
#' a list or a vector it is converted by means of \code{as.data.frame()}.
#' If of any other type, a conversion into an object of class \code{xts} is
#' attempted by means of \code{try.xts()} and if succesful the \code{xts}
#' object is converted into a data frame with a variable \code{time}
#' containing times as \code{POSIXct} and the remaining data columns with
#' the time series data. In this conversion row names are stripped.
#'
#' @param x An R object
#' @param time.resolution character The time unit to which the returned time
#'   values will be rounded.
#' @param as.numeric logical If TRUE convert time to numeric, expressed as
#'   fractional calendar years.
#'
#' @return A \code{tibble::tibble} object, derived from \code{data.frame}.
#'
#' @note This function can be used to easily converttime series data into a
#'   format that can be easily plotted with pacakge \code{ggplot2}.
#'   \code{try_tibble} is another name for \code{try_data_frame} which tracks
#'   the separation and re-naming of \code{data_frame} into
#'   \code{tibble::tibble} in the imported packages.
#'
#' @section Warning!: The time zone was set to "UTC" by try.xts() in the test
#'   cases I used. Setting TZ to "UTC" can cause some trouble as several
#'   frequently used functions have as default the local or system TZ and will
#'   apply a conversion before printing or plotting time data, which in addition
#'   is affected by summer/winter time transitions. This should be taken into
#'   account as even for yearly data when conversion is to POSIXct a day (1st of
#'   Jannuary) will be set, but then shifted some hours if printed on a TZ
#'   different from "UTC". I recommend reading the documentation of package
#'   \code{\link[lubridate]{lubridate}} where the irregularities of time data
#'   and the difficulties they cause are very well described. In many cases when
#'   working with time series with yearly observations it is best to work with
#'   numeric values for years.
#'
#' @export
#'
#' @examples
#' library(xts)
#' class(lynx)
#' try_data_frame(lynx)
#' try_data_frame(lynx, "year")
#' class(austres)
#' try_data_frame(austres)
#' try_data_frame(austres, "quarter")
#' class(cars)
#' try_data_frame(cars)
#'
try_data_frame <- function(x,
                           time.resolution = "second",
                           as.numeric = FALSE) {
  if (inherits(x, "data.frame")) {
    return(tibble::as_tibble(x))
  }
  if (!xts::xtsible(x) &&
      (is.list(x) || is.factor(x) || is.vector(x) || is.matrix(x))) {
    return(tibble::as_tibble(x))
  }
  if (!xts::is.xts(x)) {
    stopifnot(xts::xtsible(x))
    data.xts <- xts::try.xts(x)
  } else {
    data.xts <- x
  }
  times.raw <- zoo::index(data.xts) # TZ = "UTC"
  if (lubridate::is.POSIXct(times.raw[1])) {
    times <- times.raw
  } else {
    times <- as.POSIXct(times.raw) # handles conversion from classes in xts and zoo
  }
  if (lubridate::tz(times) == "") {
    times <- lubridate::with_tz(times, "UTC")
  }
  if (is.null(names(x))) {
    data.names <- paste("V.", as.character(substitute(x)), sep = "")
  } else {
    data.names <- names(x)
  }
  times <- lubridate::round_date(times, unit = time.resolution)
  if (as.numeric) {
    times <- lubridate::decimal_date(times)
  }
  z <- data.frame(time = times)
  z <- cbind(z, as.data.frame(data.xts))
  names(z)[-1] <- data.names
  rownames(z) <- NULL
  tibble::as_tibble(z)
}

#' @export
#' @rdname try_data_frame
try_tibble <- try_data_frame
