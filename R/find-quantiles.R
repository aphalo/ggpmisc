#' Find the location of quantiles
#'
#' Find the location of quantiles in fitted 1D density within a call to
#' \code{aes()} to map regions under the curve to the \code{fill} aesthetic.
#'
#' @param density numeric An density curve predicted at a large
#'   number of equal steps without truncation.
#' @param quantiles numeric The probabilities of the quantiles to locate.
#' @param group a ‘factor’ in the sense that \code{\link{as.factor}(f)} defines
#'  the grouping, or a list of such factors in which case their interaction is
#'  used for the grouping, used in a call to \code{\link{split}()}.
#'
#' @details A running cummulated density function (CDF) is computed as
#'   \code{cumsum(density)} assuming that \code{x} steps are uniform. Values are
#'   subsequently compared to the target quantiles, to identify the regions they
#'   delimit. No interpolation is done making it crucial that \code{density} is
#'   a long vector, as controlled by parameter \code{n} in
#'   \code{\link[ggplot2:geom_density]{stat_density}()}.
#'
#'   Unique and sorted values from the argument passed to \code{quantiles} are
#'   used. Values 0 and 1 are added if not present, thus, the number of regions
#'   returned is always one less than the length of these "normalized"
#'   quantiles.
#'
#'   In 'ggplot2' (>= 4.0.0) \code{\link[ggplot2:geom_ribbon]{geom_area}} when
#'   the \code{fill} aesthetic is mapped to a numeric variable or to a factor,
#'   the fill is rendered as a gradient, making it possible to highlight
#'   quantiles within a single plot layer. \emph{Currently, gradient fills are
#'   supported in R (>= 4.1.0) and only by some graphic devices. This function
#'   is of use only if gradient fills are supported!} The capabilities of the
#'   currently active device can be tested with a call to
#'   \code{\link[grDevices]{dev.capabilities}()} checking the field
#'   \code{"patterns"}.
#'
#'   \emph{The approach used is approximate and relies on assumptions that are
#'   known to be fulfiled by the \code{density} estimates returned by specific
#'   'ggplot2' stats such as \code{stat_density()}.}
#'
#' @return A factor with levels indicating the regions delimited by the
#'   quantiles. The levels are labelled by ordinal numbers.
#'
#' @note The approach used in \code{find_quantiles()} is very different to that
#'   used in package 'ggdensity', which is based on the local density rather
#'   than the accumulated one.
#'
#' @export
#'
#' @examples
#'
#'  ggplot(diamonds, aes(carat)) +
#'    stat_density(
#'      geom = "area", outline.type = "upper", colour = "black",
#'      aes(fill = after_stat(find_quantiles(density) != 2))) +
#'    scale_fill_grey(guide = "none", end = 0.3, start = 0.7) +
#'    expand_limits(x = 0)
#'
#'  ggplot(diamonds, aes(carat, fill = cut)) +
#'    stat_density(
#'      geom = "area", outline.type = "upper", colour = "black",
#'      position = "stack",
#'      aes(alpha = after_stat(find_quantiles(density, c(0.1, 0.9), group) != 2)),
#'      show.legend = FALSE) +
#'    expand_limits(x = c(0, 5.5))
#'
find_quantiles <- function(density,
                           quantiles = c(0.025, 0.975),
                           group = 1) {
  quantiles <- sort(unique(c(0, na.omit(quantiles), 1)))
  if (any(quantiles < 0 | quantiles > 1)) {
    stop("'quantiles' contains values outside 0..1!")
  }
  density.list <- split(density, factor(group))
  all.quant.splits <- integer()
  for (i in seq_along(density.list)) {
    dens <- density.list[[i]]
    if (length(dens) < 250) {
      warning("'density' vector for group '", names(density.list)[i],
              "' is short! Length: ",  length(dens))
    }
    safe.limit <- 1/length(dens)
    if (dens[1] > safe.limit || dens[length(dens)] > safe.limit) {
      warning("'density' prediction for group '", names(density.list)[i],
              "' is truncated!")
    }
    cum.dens <- cumsum(dens)
    cum.dens <- cum.dens / max(cum.dens)
    quant.splits <- integer(length(dens))
    for (i in seq_along(quantiles)) {
      quant.splits <-
        ifelse(cum.dens > quantiles[i], i, quant.splits)
    }
    all.quant.splits <- c(all.quant.splits, quant.splits)
  }
  factor(all.quant.splits)
}
