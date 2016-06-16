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
#' @param label.x.npc,label.y.npc \code{numeric} with range 0..1 or character.
#'   Coordinates to be used for positioning the output, expresed in "normalized
#'   parent coordinates" or character string. If too short they will be recycled.
#' @param label.x,label.y \code{numeric} Coordinates (in data units) to be used
#'   for absolute positioning of the output. If too short they will be recycled.
#'
#' @section Computed variables:
#'   The output of \code{\link[broom]{glance}} is returned as is.
#'
#' @export
#'
stat_fit_glance <- function(mapping = NULL, data = NULL, geom = "null",
                            method = "lm",
                            method.args = list(formula = y ~ x),
                            label.x.npc = "left", label.y.npc = "top",
                            label.x = NULL, label.y = NULL,
                            position = "identity",
                            na.rm = FALSE, show.legend = FALSE,
                            inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatFitGlance, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(method = method,
                  method.args = method.args,
                  label.x.npc = label.x.npc,
                  label.y.npc = label.y.npc,
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
                                         label.x.npc,
                                         label.y.npc,
                                         label.x,
                                         label.y) {
  if (length(unique(data$x)) < 2) {
    # Not enough data to perform fit
    return(data.frame())
  }

  group.idx <- abs(data$group[1])
  if (length(label.x.npc) >= group.idx) {
    label.x.npc <- label.x.npc[group.idx]
  } else if (length(label.x.npc) > 0) {
    label.x.npc <- label.x.npc[1]
  }
  if (length(label.y.npc) >= group.idx) {
    label.y.npc <- label.y.npc[group.idx]
  } else if (length(label.y.npc) > 0) {
    label.y.npc <- label.y.npc[1]
  }

  if (length(label.x) >= group.idx) {
    label.x <- label.x[group.idx]
  } else if (length(label.x) > 0) {
    label.x <- label.x[1]
  }
  if (length(label.y) >= group.idx) {
    label.y <- label.y[group.idx]
  } else if (length(label.y) > 0) {
    label.y <- label.y[1]
  }

  method.args <- c(method.args, list(data = quote(data)))
  if (is.character(method)) method <- match.fun(method)
  mf <- do.call(method, method.args)
  z <- broom::glance(mf)
#  print(z)

  if (length(label.x) > 0) {
    z$x <- label.x
    z$hjust <- 0.5
  } else if (length(label.x.npc) > 0) {
    if (is.numeric(label.x.npc)) {
      if (any(label.x.npc < 0 | label.x.npc > 1)) {
        warning("'label.x.npc' argument is numeric but outside range 0..1.")
      }
      z$x <- scales$x$dimension()[1] + label.x.npc *
        diff(scales$x$dimension())
      z$hjust <- 0.5
    } else if (is.character(label.x.npc)) {
      if (label.x.npc == "right") {
        z$x <- scales$x$dimension()[2]
        z$hjust <- 1
      } else if (label.x.npc %in% c("center", "centre", "middle")) {
        z$x <- mean(scales$x$dimension())
        z$hjust <- 0.5
      } else if (label.x.npc == "left") {
        z$x <- scales$x$dimension()[1]
        z$hjust <- 0
      } else {
        stop("'label.x.npc' argument '", label.x.npc, " unsupported")
      }
    } else {
      stop("'label.x.npc' argument is neither numeric nor character")
    }
  }

  if (length(label.y) > 0) {
    z$y <- label.y
    z$vjust <- 0.5
  } else if (length(label.y.npc) > 0) {
    if (is.numeric(label.y.npc)) {
      if (any(label.y.npc < 0 | label.y.npc > 1)) {
        warning("'label.y.npc' argument is numeric but outside range 0..1.")
      }
      z$y <- scales$y$dimension()[1] + label.y.npc *
        diff(scales$y$dimension())
      z$vjust <- 1.4 * group.idx - (0.7 * length(group.idx))
    } else if (is.character(label.y.npc)) {
      if (label.y.npc == "bottom") {
        z$y <- scales$y$dimension()[1]
        z$vjust <- -1.4 * group.idx
      } else if (label.y.npc %in% c("center", "centre", "middle")) {
        z$y <- mean(scales$y$dimension())
        z$vjust <- 1.4 * group.idx - (0.7 * length(group.idx))
      } else if (label.y.npc == "top") {
        z$y <- scales$y$dimension()[2]
        z$vjust <- 1.4 * group.idx
      } else {
        stop("'label.y.npc' argument '", label.y.npc, " unsupported")
      }
    } else {
      stop("'label.y.npc' argument is neither numeric nor character")
    }
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
#' @param na.rm	logical indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param method character.
#' @param method.args list of arguments to pass to \code{method}.
#' @param augment.args list of arguments to pass to \code{broom:augment}.
#' @param level numeric Level of confidence interval to use (0.95 by default)
#' @param y.out character (or numeric) index to column to return as \code{y}.
#'
#' @section Computed variables:
#'   The output of \code{\link[broom]{augment}} is returned as is, except for
#'   \code{y} which is set based on \code{y.out} and \code{y.observed} which
#'   preserves the \code{y} returned by the \code{broom::augment} methods.
#'
#' @note The statistics \code{stat_fit_augment} and
#'   \code{stat_fit_augment_panel} at the moment accepts only \code{methods}
#'   that accept formulas under any formal parameter name and a \code{data}
#'   argument. Use \code{ggplot2::stat_smooth()} instead of
#'   \code{stat_fit_augment} in production code if the additional features are
#'   not needed. At the moment \code{stat_fit_augment} is under development and
#'   may change in the future.
#'
#' @export
#'
stat_fit_augment <- function(mapping = NULL, data = NULL, geom = "smooth",
                             method = "lm",
                             method.args = list(formula = y ~ x),
                             augment.args = list(),
                             level = 0.95,
                             y.out = ".fitted",
                             position = "identity",
                             na.rm = FALSE, show.legend = FALSE,
                             inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatFitAugment, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(method = method,
                  method.args = method.args,
                  augment.args = augment.args,
                  level = level,
                  y.out = y.out,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname stat_fit_augment
#'
#' @export
#'
stat_panel_fit_augment <- function(mapping = NULL, data = NULL, geom = "smooth",
                                   method = "lm",
                                   method.args = list(formula = y ~ x),
                                   augment.args = list(),
                                   level = 0.95,
                                   y.out = ".fitted",
                                   position = "identity",
                                   na.rm = FALSE, show.legend = FALSE,
                                   inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatPanelFitAugment, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(method = method,
                  method.args = method.args,
                  augment.args = augment.args,
                  level = level,
                  y.out = y.out,
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
                                    augment.args,
                                    level,
                                    y.out,
                                    ...) {

  unAsIs <- function(X) {
    if ("AsIs" %in% class(X)) {
      class(X) <- class(X)[-match("AsIs", class(X))]
    }
    X
  }

  if (length(unique(data[["x"]])) < 2) {
    # Not enough data to perform fit
    return(data.frame())
  }
  data <- data[order(data[["x"]]), ]
#  print(tibble::as_data_frame(data))
  method.args <- c(method.args, list(data = quote(data)))
  if (is.character(method)) method <- match.fun(method)
  mf <- do.call(method, method.args)
#  print(mf)
  augment.args <- c(list(x = mf), augment.args)
#  print(augment.args)
  z <- do.call(broom::augment, augment.args)
#  print(tibble::as_data_frame(z))
  z <- plyr::colwise(unAsIs)(z)
  tibble::as_data_frame(z)
  z[["y.observed"]] <- z[["y"]]
  z[["y"]] <- z[[y.out]]
  if (exists("df.residual", mf) && y.out == ".fitted") {
    z[["t.value"]] <- stats::qt(1 - (1 - level) / 2, mf[["df.residual"]])
  } else {
    z[["t.value"]] <- NA_real_
  }
  if (!exists(".se.fit", z)) {
    z[[".se.fit"]] <- NA_real_
  }
#  print(tibble::as_data_frame(z))
  z
}

#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
fit_augment_compute_panel_fun <- fit_augment_compute_group_fun

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFitAugment <-
  ggplot2::ggproto("StatFitAugment",
                   ggplot2::Stat,
                   compute_group = fit_augment_compute_group_fun,
                   default_aes =
                     ggplot2::aes(ymax = ..y.. + ...se.fit.. * ..t.value..,
                                  ymin = ..y.. - ...se.fit.. * ..t.value..),
                   required_aes = c("x", "y")
)

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPanelFitAugment <-
  ggplot2::ggproto("StatPanelFitAugment",
                   ggplot2::Stat,
                   compute_panel = fit_augment_compute_panel_fun,
                   default_aes =
                     ggplot2::aes(ymax = ..y.. + ...se.fit.. * ..t.value..,
                                  ymin = ..y.. - ...se.fit.. * ..t.value..),
                   required_aes = c("x", "y")
  )

