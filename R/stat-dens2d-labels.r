#' @title Reset labels of observations in high density regions.
#'
#' @description \code{stat_low_dens} Sets \code{labels} to NA in regions of a
#'   plot panel with high density of observations.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_string}}. Only needs
#'   to be set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data.
#' @param keep.fraction numeric [0..1].
#' @param keep.number integer number of labels to keep.
#' @param h vector of bandwidths for x and y directions. Defaults to normal
#'   reference bandwidth (see bandwidth.nrd). A scalar value will be taken to
#'   apply to both directions.
#' @param n Number of grid points in each direction. Can be scalar or a length-2
#'   integer vector
#' @param label.fill character.
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
#'
#' @section Computed variables: \describe{ \item{labels}{x at centre of range} }
#'
#' @seealso \code{\link[MASS]{kde2d}} used internally.
#'
#' @examples
#'
#' library(ggrepel)
#'
#' random_string <- function(len = 6) {
#' paste(sample(letters, len, replace = TRUE), collapse = "")
#' }
#'
#' # Make random data.
#' set.seed(1001)
#' d <- tibble::tibble(
#'   x = rnorm(100),
#'   y = rnorm(100),
#'   group = rep(c("A", "B"), c(50, 50)),
#'   lab = replicate(100, { random_string() })
#' )
#'
#' ggplot(data = d, aes(x, y, label = lab)) +
#'   geom_point() +
#'   stat_dens2d_labels()
#'
#' ggplot(data = d, aes(x, y, label = lab, color = group)) +
#'   geom_point() +
#'   stat_dens2d_labels()
#'
#' ggplot(data = d, aes(x, y, label = lab, color = group)) +
#'   geom_point() +
#'   stat_dens2d_labels(geom = "text_repel")
#'
#' ggplot(data = d, aes(x, y, label = lab, color = group)) +
#'   geom_point() +
#'   stat_dens2d_labels(geom = "text_repel", label.fill = NA)
#'
#' @export
#'
stat_dens2d_labels <-
  function(mapping = NULL, data = NULL,
           geom = "text", position = "identity",
           keep.fraction = 0.10,
           keep.number = Inf,
           h = NULL,
           n = NULL,
           label.fill = "",
           na.rm = TRUE, show.legend = FALSE,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      stat = StatDens2dLabels, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm,
                    keep.fraction = keep.fraction,
                    keep.number = keep.number,
                    h = h,
                    n = n,
                    label.fill = label.fill,
                    ...)
    )
  }

dens2d_labs_compute_fun <-
  function(data, scales,
           keep.fraction,
           keep.number,
           h,
           n,
           label.fill) {
    if (nrow(data) * keep.fraction > keep.number) {
      keep.fraction <- keep.number / nrow(data)
    }

    if (is.null(h)) {
      h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
    }

    if (is.null(n)) {
      n = trunc(sqrt(nrow(data))) * 8L
    }

    #    kk <- MASS::kde2d(x,y)
    kk <-  MASS::kde2d(
      data$x, data$y, h = h, n = n,
      lims = c(scales$x$dimension(), scales$y$dimension()))

    dimnames(kk$z) <- list(kk$x,kk$y)

    # Identify points that are in the low density regions of the plot.
    kx <- cut(data$x, kk$x, labels = FALSE, include.lowest = TRUE)
    ky <- cut(data$y, kk$y, labels = FALSE, include.lowest = TRUE)
    kz <- sapply(seq_along(kx), function(i) kk$z[kx[i], ky[i]])

    # d$low <- kz < res$threshold
    keep <- kz < stats::quantile(kz, keep.fraction, names = FALSE)
    #  kz
    df <- data
    df[["label"]] <- ifelse(keep, data[["label"]], label.fill)
    df
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDens2dLabels <-
  ggplot2::ggproto(
    "StatDens2dLabels",
    ggplot2::Stat,
    compute_panel =
      dens2d_labs_compute_fun,
    required_aes = c("x", "y")
  )
