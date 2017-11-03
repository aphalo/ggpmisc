#' Count the number of observations in each quadrat of a plot.
#'
#' \code{stat_quadrat_counts} counts the number of observations in each quadrat
#' of a plot.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_string}}. Only needs
#'   to be set at the layer level if you are overriding the plot defaults.
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
#'   define both data and aesthetics and should not inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param quadrats integer vector indicating which quadrats are of interest, with
#'   a \code{OL} indicating the whole plot.
#' @param origin.x,origin.y numeric the coordinates of the origin of the quadrats.
#' @param labels.range.x,labels.range.y \code{numeric} Coordinates (in data units) to be used
#'   for absolute positioning of the labels.
#'
#' @details This stat can be used to automatically observations in each of the
#' four quadrats of a plot, and by default add these counts as text labels.
#'
#' @section Computed variables: Data frame with one to four rows, one for each
#'   quadrat for which observations are present in \code{data}.
#'   \describe{
#'   \item{x}{extreme x value in the quadrat}
#'   \item{y}{extreme y value in the quadrat}
#'   \item{count}{number of ovserbations}
#'   }
#'
#' @examples
#' library(ggplot2)
#' # generate artificial data
#' set.seed(4321)
#' x <- 1:100
#' y <- rnorm(length(x), mean = 10)
#' my.data <- data.frame(x, y)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quadrat_counts()
#'
#' ggplot(my.data, aes(x - 50, y - 10)) +
#'   geom_hline(yintercept = 0, colour = "blue") +
#'   geom_vline(xintercept = 0, colour = "blue") +
#'   geom_point() +
#'   stat_quadrat_counts(colour = "blue")
#'
#' ggplot(my.data, aes(x - 50, y - 10)) +
#'   geom_point() +
#'   stat_quadrat_counts(quadrats = 0)
#'
#' @export
#'
stat_quadrat_counts <- function(mapping = NULL, data = NULL, geom = "text",
                                position = "identity",
                                quadrats = NULL,
                                origin.x = 0, origin.y = 0,
                                labels.range.x = NULL, labels.range.y = NULL,
                                na.rm = FALSE, show.legend = FALSE,
                                inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatQuadratCounts, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  quadrats = quadrats,
                  origin.x = origin.x,
                  origin.y = origin.y,
                  labels.range.x = labels.range.x,
                  labels.range.y = labels.range.y,
                  ...)
  )
}

#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
compute_counts_fun <- function(data,
                               scales,
                               quadrats,
                               origin.x,
                               origin.y,
                               labels.range.x,
                               labels.range.y) {

  which_quadrat <- function(x, y) {
    ifelse(x >= origin.x & y >= origin.y,
           1L,
           ifelse(x >= origin.x & y < origin.y,
                  2L,
                  ifelse(x < origin.x & y < origin.y,
                         3L,
                         4L)))
  }

  stopifnot(length(origin.x) == 1 && length(origin.y) == 1)
  stopifnot(length(quadrats) <= 4)
  stopifnot(is.numeric(labels.range.x) && is.numeric(labels.range.y))

  force(data)
  # compute range of whole data
  range.x <- range(data$x)
  range.y <- range(data$y)
  # compute postion for labels
  if (is.null(labels.range.x)) {
    labels.range.x <- range.x
  } else {
    labels.range.x <- range(labels.range.x)
  }
  if (is.null(labels.range.y)) {
    labels.range.y <- range.y
  } else {
    labels.range.y <- range(labels.range.y)
  }

  # dynamic default based on data range
  if (is.null(quadrats)) {
    if (all(range.x >= origin.x) && all(range.y >= origin.y)) {
      quadrats = 1L
    } else if (all(range.x < origin.x) && all(range.y < origin.y)) {
      quadrats = 3L
    } else if (all(range.x >= origin.x)) {
      quadrats = c(1L, 2L)
    } else if (all(range.y >= origin.y)) {
      quadrats = c(1L, 4L)
    } else {
      quadrats = c(1L, 2L, 3L, 4L)
    }
  }

  if (all(is.na(quadrats)) || 0L %in% quadrats) {
  # total count
    data.frame(count = nrow(data),
               x = range.x[2],
               y = range.y[2],
               hjust = 1,
               vjust = 1)
  } else {
  # counts for the selected quadrats
    data %>%
      dplyr::mutate(quadrat = which_quadrat(.data$x, .data$y)) %>%
      dplyr::filter(.data$quadrat %in% quadrats) %>%
      dplyr::group_by(.data$quadrat) %>%
      dplyr::summarise(count = length(.data$x), # dplyr::n() triggers error
                x = ifelse(.data$quadrat[1] %in% c(1L, 2L), labels.range.x[2], labels.range.x[1]),
                y = ifelse(.data$quadrat[1] %in% c(1L, 4L), labels.range.y[2], labels.range.y[1]),
                hjust = ifelse(.data$quadrat[1] %in% c(1L, 2L), 1, 0),
                vjust = ifelse(.data$quadrat[1] %in% c(1L, 4L), 0, 1)) %>%
      dplyr::ungroup()
  }
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatQuadratCounts <-
  ggplot2::ggproto("StatQuadratCounts", ggplot2::Stat,
                   compute_panel = compute_counts_fun,
                   default_aes =
                     ggplot2::aes(label = paste("n=", ..count..),
                                  hjust = ..hjust..,
                                  vjust = ..vjust..),
                   required_aes = c("x", "y")
  )


