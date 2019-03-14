#' Number of observations in quadrants
#'
#' \code{stat_quadrant_counts()} counts the number of observations in each
#' quadrant of a plot panel. By default it adds a text label to the far corner
#' of each quadrant. It can also be used to obtain the total number of
#' observations in each of two pairs of quadrants or in the whole panel.
#' Grouping is ignored, so en every case a single count is computed for each
#' quadrant in a plot panel.
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
#' @param xintercept,yintercept numeric the coordinates of the origin of the
#'   quadrants.
#' @param label.x,label.y \code{numeric} Coordinates (in npc units) to be used
#'   for absolute positioning of the labels.
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
#'   counts for is decided based on which quadrants are expected to be visible
#'   in the plot. In the current implementation, the default positions of the
#'   labels is based on the range of the coordinates in a given panel.
#'   Consequently, when using facets even with free limits for x and y axes, the
#'   location of the labels will be consistent accross panels. This is achieved
#'   by use of \code{geom = "text_npc"} or \code{geom = "label_npc"}. To pass
#'   the positions in native data units, pass \code{geom = "text"} explicitly as
#'   argument, or labels will be positioned relative to the data range in each
#'   panel.
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
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quadrant_counts(aes(label = sprintf("%i observations", stat(count)))) +
#'   expand_limits(y = 12.7)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_hline(yintercept = 10, colour = "blue") +
#'   geom_vline(xintercept = 50, colour = "blue") +
#'   geom_point() +
#'   stat_quadrant_counts(colour = "blue", xintercept = 50, yintercept = 10) +
#'   scale_y_continuous(expand = expand_scale(mult = 0.15, add = 0))
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_hline(yintercept = 10, colour = "blue") +
#'   geom_point() +
#'   stat_quadrant_counts(colour = "blue", label.x = "right",
#'                        pool.along = "x", yintercept = 10) +
#'   expand_limits(y = c(7, 13))
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quadrant_counts(quadrants = 0, label.x = "left", label.y = "bottom")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quadrant_counts(geom = "label_npc", quadrants = 0,
#'                        label.x = "left", label.y = "bottom")
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_quadrant_counts(geom = "text") # use "text_npc" instead
#'
#' @export
#'
stat_quadrant_counts <- function(mapping = NULL, data = NULL, geom = "text_npc",
                                position = "identity",
                                quadrants = NULL,
                                pool.along = "none",
                                xintercept = 0, yintercept = 0,
                                label.x = NULL, label.y = NULL,
                                na.rm = FALSE, show.legend = FALSE,
                                inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatQuadrantCounts, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  quadrants = quadrants,
                  pool.along = pool.along,
                  xintercept = xintercept,
                  yintercept = yintercept,
                  label.x = label.x,
                  label.y = label.y,
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
                               xintercept,
                               yintercept,
                               label.x,
                               label.y) {

  which_quadrant <- function(x, y) {
    z <- ifelse(x >= xintercept & y >= yintercept,
                1L,
                ifelse(x >= xintercept & y < yintercept,
                       2L,
                       ifelse(x < xintercept & y < yintercept,
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
  stopifnot(length(xintercept) == 1 && length(yintercept) == 1)
  stopifnot(length(quadrants) <= 4)
  stopifnot(is.null(label.x) || is.numeric(label.x) || is.character(label.x))
  stopifnot(is.null(label.y) || is.numeric(label.y) || is.character(label.y))

  force(data)
  # compute range of whole data
  range.x <- range(data$x)
  range.y <- range(data$y)
  # set position for labels in npc units
  if (is.null(label.x)) {
    if (pool.along == "x") {
      label.x <- rep("centre", 2)
    } else {
      label.x <- c("left", "right")
    }
  }
  if (is.null(label.y)) {
    if (pool.along == "y") {
      label.y <- rep("centre", 2)
    } else {
      label.y <- c("bottom", "top")
    }
  }
  # convert positions given as character to npc units
  margin.npc = 0.05
  npc.positions <- c(right = 1 - margin.npc,
                     left = 0 + margin.npc,
                     centre = 0.5,
                     center = 0.5,
                     middle = 0.5,
                     top = 1 - margin.npc,
                     bottom = 0 + margin.npc)
  if (is.character(label.x)) {
    label.x <- npc.positions[label.x]
  }
  if (is.character(label.y)) {
    label.y <- npc.positions[label.y]
  }
  label.x <- range(label.x)
  label.y <- range(label.y)

  # dynamic default based on data range
  if (is.null(quadrants)) {
    if (all(range.x >= xintercept) && all(range.y >= yintercept)) {
      quadrants <- 1L
    } else if (all(range.x < xintercept) && all(range.y < yintercept)) {
      quadrants <- 3L
    } else if (all(range.x >= xintercept)) {
      quadrants <- c(1L, 2L)
    } else if (all(range.y >= yintercept)) {
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
                   npcx = label.x[2],
                   npcy = label.y[2],
                   x = range.x[2],
                   y = range.y[2])
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
                                  label.x[2],
                                  label.x[1]),
                    npcy = ifelse(.data$quadrant %in% c(1L, 4L),
                                  label.y[2],
                                  label.y[1]),
                    x = ifelse(.data$quadrant %in% c(1L, 2L),
                               range.x[2],
                               range.x[1]),
                    y = ifelse(.data$quadrant %in% c(1L, 4L),
                               range.y[2],
                               range.y[1]))
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
                                  label = sprintf("n=%i", stat(count)),
                                  hjust = "inward",
                                  vjust = "inward"),
                   required_aes = c("x", "y")
  )


