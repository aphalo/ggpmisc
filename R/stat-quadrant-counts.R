#' Number of observations in quadrants
#'
#' \code{stat_quadrant_counts()} counts the number of observations in each quadrant
#' of a plot panel. By default it adds a text label to the far corner of each
#' quadrant. It can also be used to obtain the total number of observations in
#' each of two pairs of quadrants or in the whole panel. Grouping is ignored, so
#' en every case a single count is computed for each quadrant in a plot panel.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs
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
#' @param na.rm	a logical indicating whether NA values should be stripped before
#'   the computation proceeds.
#' @param quadrants integer vector indicating which quadrants are of interest,
#'   with a \code{OL} indicating the whole plot.
#' @param pool.along character, one of "none", "x" or "y", indicating which
#'   quadrants to pool to calculate counts by pair of quadrants.
#' @param origin.x,origin.y numeric the coordinates of the origin of the
#'   quadrants.
#' @param labels.range.x,labels.range.y \code{numeric} Coordinates (in npc
#'   units) to be used for absolute positioning of the labels.
#'
#' @details This stat can be used to automatically count observations in each of
#'   the four quadrants of a plot, and by default add these counts as text
#'   labels.
#'
#' @section Computed variables: Data frame with one to four rows, one for each
#'   quadrant for which observations are present in \code{data}. \describe{
#'   \item{quadrant}{integer, one of 0:4} \item{x}{extreme x value in the
#'   quadrant} \item{y}{extreme y value in the quadrant} \item{count}{number of
#'   observations} }
#'
#' @note Values exactly equal to zero are counted as belonging to the positve
#'   quadrant. An argument value of zero, passed to formal parameter
#'   \code{quadrants} is interpreted as a request for the count of all
#'   observations in each plot panel. By default, which quadrants to compute
#'   counts for is decided based on which quadrants are expected to be visible in
#'   the plot. In the current implementation, the default positions of the
#'   labels is based on the range of the coordinates in a given panel.
#'   Consequently, when using facets even with free limits for x and y axes,
#'   the location of the labels will be consistent accross panels. This is
#'   achieved by use of \code{geom = "text_npc"}. To explicitly pass the
#'   positions in native data units, pass \code{geom = "text"} explicitly as
#'   argument.
#'
#' @examples
#' # generate artificial data
#' set.seed(4321)
#' x <- 1:100
#' y <- rnorm(length(x), mean = 10)
#' my.data <- data.frame(x, y)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quadrant_counts()
#'
#' ggplot(my.data, aes(x - 50, y - 10)) +
#'   geom_hline(yintercept = 0, colour = "blue") +
#'   geom_vline(xintercept = 0, colour = "blue") +
#'   geom_point() +
#'   stat_quadrant_counts(colour = "blue")
#'
#' ggplot(my.data, aes(x - 50, y - 10)) +
#'   geom_hline(yintercept = 0, colour = "blue") +
#'   geom_point() +
#'   stat_quadrant_counts(colour = "blue", pool.along = "x")
#'
#' ggplot(my.data, aes(x - 50, y - 10)) +
#'   geom_vline(xintercept = 0, colour = "blue") +
#'   geom_point() +
#'   stat_quadrant_counts(colour = "blue", pool.along = "y")
#'
#' ggplot(my.data, aes(x - 50, y - 10)) +
#'   geom_point() +
#'   stat_quadrant_counts(quadrants = 0)
#'
#' @export
#'
stat_quadrant_counts <- function(mapping = NULL, data = NULL, geom = "text_npc",
                                position = "identity",
                                quadrants = NULL,
                                pool.along = "none",
                                origin.x = 0, origin.y = 0,
                                labels.range.x = NULL, labels.range.y = NULL,
                                na.rm = FALSE, show.legend = FALSE,
                                inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatQuadrantCounts, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  quadrants = quadrants,
                  pool.along = pool.along,
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
                               quadrants,
                               pool.along,
                               origin.x,
                               origin.y,
                               labels.range.x,
                               labels.range.y) {

  which_quadrant <- function(x, y) {
    z <- ifelse(x >= origin.x & y >= origin.y,
                1L,
                ifelse(x >= origin.x & y < origin.y,
                       2L,
                       ifelse(x < origin.x & y < origin.y,
                              3L,
                              4L)))
    if (pool.along == "x") {
      z <- ifelse(z %in% c(1L, 4L), 1L, 2L)
    } else if(pool.along == "y") {
      z <- ifelse(z %in% c(1L, 2L), 1L, 4L)
    }
    z
  }

  stopifnot(pool.along %in% c("none", "x", "y"))
  stopifnot(length(origin.x) == 1 && length(origin.y) == 1)
  stopifnot(length(quadrants) <= 4)
  stopifnot(is.null(labels.range.x) || is.numeric(labels.range.x))
  stopifnot(is.null(labels.range.y) || is.numeric(labels.range.y))

  force(data)
  # compute range of whole data
  range.x <- range(data$x)
  range.y <- range(data$y)
  # set position for labels in npc units
  if (is.null(labels.range.x)) {
    if (pool.along == "x") {
      labels.range.x <- rep(0.5, 2)
    } else {
      labels.range.x <- c(0.05, 0.95)
    }
  } else {
    labels.range.x <- range(labels.range.x)
  }

  if (is.null(labels.range.y)) {
    if (pool.along == "y") {
      labels.range.y <- rep(0.5, 2)
    } else {
      labels.range.y <- c(0.05, 0.95)
    }
  } else {
    labels.range.y <- range(labels.range.y)
  }

  # dynamic default based on data range
  if (is.null(quadrants)) {
    if (all(range.x >= origin.x) && all(range.y >= origin.y)) {
      quadrants <- 1L
    } else if (all(range.x < origin.x) && all(range.y < origin.y)) {
      quadrants <- 3L
    } else if (all(range.x >= origin.x)) {
      quadrants <- c(1L, 2L)
    } else if (all(range.y >= origin.y)) {
      quadrants <- c(1L, 4L)
    } else {
      quadrants <- c(1L, 2L, 3L, 4L)
    }
  }
  if (pool.along == "x") {
    quadrants <- intersect(quadrants, c(1L, 2L))
  }
  if (pool.along == "y") {
    quadrants <- intersect(quadrants, c(1L, 4L))
  }

  if (all(is.na(quadrants)) || 0L %in% quadrants) {
  # total count
    tibble::tibble(quadrant = 0,
                   count = nrow(data),
                   npcx = labels.range.x[2],
                   npcy = labels.range.y[2],
                   x = range.x[2],
                   y = range.y[2],
                   hjust = "inwards",
                   vjust = "inwards")
  } else {
  # counts for the selected quadrants
    data %>%
      dplyr::mutate(quadrant = which_quadrant(.data$x, .data$y)) %>%
      dplyr::filter(.data$quadrant %in% quadrants) %>%
      dplyr::group_by(.data$quadrant) %>%
      dplyr::summarise(count = length(.data$x)) %>% # dplyr::n() triggers error
      dplyr::ungroup() -> data

    zero.count.quadrants <- setdiff(quadrants, data$quadrant)

    if (length(zero.count.quadrants) > 0) {
      data <-
        rbind(data, tibble::tibble(quadrant = zero.count.quadrants, count = 0L))
    }

    data %>%
      dplyr::mutate(npcx = ifelse(.data$quadrant %in% c(1L, 2L),
                               labels.range.x[2],
                               labels.range.x[1]),
                    npcy = ifelse(.data$quadrant %in% c(1L, 4L),
                               labels.range.y[2],
                               labels.range.y[1]),
                    x = ifelse(.data$quadrant %in% c(1L, 2L),
                               range.x[2],
                               range.x[1]),
                    y = ifelse(.data$quadrant %in% c(1L, 4L),
                               range.y[2],
                               range.y[1]),
                    hjust = "inwards",
                    vjust = "inwards")
   }
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatQuadrantCounts <-
  ggplot2::ggproto("StatQuadrantCounts", ggplot2::Stat,
                   compute_panel = compute_counts_fun,
                   default_aes =
                     ggplot2::aes(npcx = stat(npcx),
                                  npcy = stat(npcy),
                                  label = paste("n=", stat(count), sep = ""),
                                  hjust = stat(hjust),
                                  vjust = stat(vjust)),
                   required_aes = c("x", "y")
  )


