# broom::glance -----------------------------------------------------------

#' Return one row summary data frame for a fitted linear model.
#'
#' \code{stat_fit_glance} fits a model and returns a summary "glance" of the
#' model's statistics, using package 'broom'.
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
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param method character.
#' @param method.args list of arguments to pass to \code{method}.
#' @param label.x,label.y \code{numeric} Coordinates to be used in output. If
#'   too short they will be recycled.
#'
#' @section Computed variables:
#'   The output of \code{\link[broom]{glance}} is returned as is.
#'
#' @export
#'
stat_fit_glance <- function(mapping = NULL, data = NULL, geom = "null",
                            method = "lm",
                            method.args = list(formula = y ~ x),
                            label.x = NULL, label.y = NULL,
                            position = "identity",
                            na.rm = FALSE, show.legend = FALSE,
                            inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatFitGlance, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(method = method,
                  method.args = method.args,
                  label.x = label.x,
                  label.y = label.y,
                  na.rm = na.rm,
                  ...)
  )
}


# Defined here to avoid a note in check --as-cran as the imports from 'broom'
# are not seen when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
fit_glance_compute_group_fun <- function(data,
                                         scales,
                                         method,
                                         method.args,
                                         label.x,
                                         label.y) {
  if (length(unique(data$x)) < 2) {
    # Not enough data to perform fit
    return(data.frame())
  }
  group.idx <- abs(data$group[1])
  method.args <- c(method.args, list(data = quote(data)))
  if (is.character(method)) method <- match.fun(method)
  z <- broom::glance(do.call(method, method.args))
  if (is.numeric(label.x)) {
    if (label.x <= 1 & label.x >= 0) {
      z$x <- scales$x$dimension()[1] + label.x *
        diff(scales$x$dimension())
    } else {
      z$x <- label.x
    }
    z$hjust <- 0.5
  } else if (is.character(label.x) && label.x == "right") {
    z$x <- scales$x$dimension()[2]
    z$hjust <- 1
  } else if (is.character(label.x) && label.x == "center") {
    z$x <- mean(scales$x$dimension())
    z$hjust <- 0.5
  } else {
    z$x <- scales$x$dimension()[1]
    z$hjust <- 0
  }
  if (is.numeric(label.y)) {
    if (label.y <= 1 & label.y >= 0) {
      z$y <- scales$y$dimension()[1] + label.y *
        diff(scales$y$dimension())
    } else {
      z$y <- label.y
    }
    z$vjust <- 1.4 * group.idx - (0.7 * length(group.idx))
  } else if (is.character(label.y) && label.y == "bottom") {
    z$y <- scales$y$dimension()[1]
    z$vjust <- -1.4 * group.idx
  } else if (is.character(label.y) && label.y == "center") {
    z$y <- mean(scales$y$dimension())
    z$vjust <- 1.4 * group.idx - (0.7 * length(group.idx))
  } else {
    z$y <- scales$y$dimension()[2]
    z$vjust <- 1.4 * group.idx
  }
  z
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFitGlance <-
  ggplot2::ggproto("StatFitGlance", ggplot2::Stat,
                   compute_group = fit_glance_compute_group_fun,
                   default_aes =
                     ggplot2::aes(hjust = ..hjust.., vjust = ..vjust..),
                   required_aes = c("x", "y")
  )


# broom::augment ----------------------------------------------------------

#' Return the data augmented with fitted values and statistics.
#'
#' \code{stat_fit_augment} fits a model and returns a summary "glance" of the
#' model's statistics, using package 'broom'.
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
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param method character.
#' @param method.args list of arguments to pass to \code{method}.
#'
#' @section Computed variables:
#'   The output of \code{\link[broom]{glance}} is returned as is.
#'
#' @note This statistic does not do much at the moment. Use
#'   \code{ggplot2::stat_smooth()} instead.
#'
#' @export
#'
stat_fit_augment <- function(mapping = NULL, data = NULL, geom = "smooth",
                             method = "lm",
                             method.args = list(formula = y ~ x),
                             level = 0.95,
                             position = "identity",
                             na.rm = FALSE, show.legend = FALSE,
                             inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatFitAugment, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(method = method,
                  method.args = method.args,
                  level = level,
                  na.rm = na.rm,
                  ...)
  )
}

# Defined here to avoid a note in check --as-cran as the imports from 'broom'
# are not seen when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
fit_augment_compute_group_fun <- function(data,
                                          scales,
                                          method,
                                          method.args,
                                          level) {
  if (length(unique(data$x)) < 2) {
    # Not enough data to perform fit
    return(data.frame())
  }
  method.args <- c(method.args, list(data = quote(data)))
  if (is.character(method)) method <- match.fun(method)
  mf <- do.call(method, method.args)
  z <- broom::augment(mf)
  z[["y.observed"]] <- z[["y"]]
  z[["y"]] <- z[[".fitted"]]
#  print(dplyr::as_data_frame(z))
  z
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFitAugment <-
  ggplot2::ggproto("StatFitAugment", ggplot2::Stat,
                   compute_group = fit_augment_compute_group_fun,
                   default_aes =
                     ggplot2::aes(ymax = ...fitted.. + ...se.fit.. * 2,
                                  ymin = ...fitted.. - ...se.fit.. * 2),
                   required_aes = c("x", "y")
)

