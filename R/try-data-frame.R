#' Convert an R object to a data frame
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
#' @return A dataframe.
#'
#' @note This function can be used to easily plot time series data with ggplot2.
#'
#' @export
#'
#' @examples
#' library(xts)
#' try_data_frame(lynx)
#' try_data_frame(lynx, "year")
#' try_data_frame(austres)
#' try_data_frame(austres, "quarter")
#' try_data_frame(cars)
#' try_data_frame(photobiology::sun.spct)
#'
try_data_frame <- function(x, time.resolution = "second", as.numeric = FALSE) {
  if (inherits(x, "data.frame")) {
    return(x)
  }
  if (!inherits(x, c("ts", "zoo", "timeSeries", "irts", "fts")) &&
      (is.list(x) || is.factor(x) || is.vector(x) || is.matrix(x))) {
    return(as.data.frame(x))
  }
  if (!xts::is.xts(x)) {
    data.xts <- xts::try.xts(x)
  } else {
    data.xts <- x
  }
  times.raw <- zoo::index(data.xts)
  if (lubridate::is.POSIXct(times.raw[1])) {
    times <- times.raw
  } else {
    times <- as.POSIXct(times.raw) # handles conversion from classes in xts and zoo
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
  z
}
