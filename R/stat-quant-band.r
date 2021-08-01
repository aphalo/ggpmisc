#' Compute predicted line from quantile regression fit
#'
#' Predicted values are computed and, by default, plotted as a band plus an
#' optional line within. \code{stat_quant_band()} supports the use of both
#' \code{x} and \code{y} as explanatory variable in the model formula.
#'
#' This statistic is similar to \code{\link{stat_quant_line}} but plots the
#' quantiles differently with the band representing a region between two
#' quantiles, while in \code{stat_quant_line()} the bands plotted whe
#' \code{se = TRUE} represent confidence intervals for the fitted quantile
#' lines.
#'
#' @details
#' \code{\link[ggplot2]{geom_smooth}}, which is used by default, treats each
#' axis differently and thus is dependent on orientation. If no argument is
#' passed to \code{formula}, it defaults to \code{y ~ x} but \code{x ~y} is also
#' accepted, and equivalent to \code{y ~ x} plus \code{orientation = "y"}.
#' Package 'ggpmisc' does not define a new geometry matching this statistic as
#' it is enough for the statistic to return suitable `x` and `y` values.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset, only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data.
#' @param position The position adjustment to use for overlapping points on this
#'   layer.
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
#' @param na.rm	a logical indicating whether NA values should be stripped before
#'   the computation proceeds.
#' @param formula a formula object. Using aesthetic names \code{x} and \code{y}
#'   instead of original variable names.
#' @param quantiles numeric vector Two or three values in 0..1 indicating the
#'   quantiles at the  edges of the band and optionally a line within the band.
#' @param method function or character If character, "lm", "rlm" and
#'   "rq" are accepted. If a function, it must have formal parameters
#'   \code{formula} and \code{data} and return a model fit object for which
#'   \code{summary()} and \code{coefficients()} are consistent with those for
#'   \code{lm} fits.
#' @param method.args named list with additional arguments.
#' @param n Number of points at which to evaluate smoother.
#' @param orientation character Either "x" or "y" controlling the default for
#'   \code{formula}.
#'
#' @return The value returned by the statistic is a data frame, that will have
#'   \code{n} rows of predicted values for three quantiles as \code{y},
#'   \code{ymin} and \code{ymax}, plus \code{x}.
#'
#' @note Even though the \code{ggplot2::geom_smooth()} is used by default, the
#'   default colour of the band is different to avoid confusion of between
#'   quantile bands and confidence bands. Of course, the \code{fill} aesthtics
#'   can be mapped to a variable or to a constant as usual
#'   (\code{fill = "grey60"} restores the geom's original default).
#'
#' @section Aesthetics: \code{stat_quant_eq} expects \code{x} and \code{y},
#'   aesthetics to be used in the \code{formula} rather than the names of the
#'   variables mapped to them. If present, the variable mapped to the
#'   \code{weight} aesthetics is passed as argument to parameter \code{weights}
#'   of the fitting function. All three must be mapped to \code{numeric}
#'   variables. In addition, the aesthetics recognized by the geometry
#'   (\code{"geom_smooth"} is the default) are obeyed and grouping
#'   respected.
#'
#' @family quantile regression functions.
#'
#' @export
#'
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_band()
#'
#' # If you need the fitting to be done along the y-axis set the orientation
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_band(orientation = "y")
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_band(formula = y ~ x)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_band(formula = x ~ y)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_band(formula = y ~ poly(x, 3))
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_band(formula = x ~ poly(y, 3))
#'
#' # Instead of a loess smooth, you can use any other modelling function:
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_band(method = "rqss")
#'
#' # Smooths are automatically fit to each group (defined by categorical
#' # aesthetics or the group aesthetic) and for each facet.
#'
#' ggplot(mpg, aes(displ, hwy, colour = class)) +
#'   geom_point() +
#'   stat_quant_band(formula = y ~ x)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_band(formula = y ~ poly(x, 2)) +
#'   facet_wrap(~drv)
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_band(linetype = "dashed", color = "darkred", fill = "red")
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   stat_quant_band(color = NA, alpha = 1) +
#'   geom_point()
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   stat_quant_band(quantiles = c(0, 0.1, 0.2)) +
#'   geom_point()
#'
#' @export
#'
stat_quant_band <- function(mapping = NULL,
                            data = NULL,
                            geom = "smooth",
                            position = "identity",
                            ...,
                            quantiles = c(0.25, 0.5, 0.75),
                            formula = NULL,
                            n = 80,
                            method = "rq",
                            method.args = list(),
                            na.rm = FALSE,
                            orientation = NA,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  if (is.null(formula)) {
    formula = y ~ x
    if (is.na(orientation)) {
      orientation = "x"
    }
  } else {
    formula.chr <- as.character(formula)
    if (is.na(orientation)) {
      # we guess orientation from formula
      if (formula.chr[2] == "y") {
        orientation <- "x"
      } else if (formula.chr[2] == "x") {
        orientation <- "y"
        formula <- swap_xy(formula)
      }
    } else if (formula.chr[2] != "y"){
      stop("When both 'orientation' and 'formula' are passed arguments ",
           "the formula should have 'x' as explanatory variable.")
    }
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatQuantBand,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      quantiles = quantiles,
      formula = formula,
      n = n,
      method = method,
      method.args = method.args,
      na.rm = na.rm,
      orientation = orientation,
      se = TRUE, # passed to geom_smooth
      ...
    )
  )
}

# Defined here to avoid a note in check --as-cran as the import from 'polynom'
# is not seen when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
quant_band_compute_group_fun <- function(data,
                                         scales,
                                         quantiles = c(0.25, 0.5, 0.75),
                                         formula = NULL,
                                         n = 80,
                                         method = "rq",
                                         method.args = list(),
                                         lambda = 1,
                                         na.rm = FALSE,
                                         flipped_aes = NA) {
  rlang::check_installed("quantreg", reason = "for `stat_quantile()`")

  data <- ggplot2::flip_data(data, flipped_aes)

  if (is.null(data[["weight"]])) {
    data[["weight"]] <- 1
  }

  min.indep <- min(data[["x"]], na.rm = TRUE)
  max.indep <- max(data[["x"]], na.rm = TRUE)
  seq.indep <- seq(min.indep, max.indep, length.out = n)

  grid <- data.frame(x = seq.indep)

  # if method was specified as a character string, replace with
  # the corresponding function
  if (is.character(method)) {
    if (identical(method, "rq")) {
      method <- quantreg::rq
    } else if (identical(method, "rqss")) {
      method <- quantreg::rqss
    } else {
      method <- match.fun(method) # allow users to supply their own methods
    }
  } else {
    stopifnot(is.function(method))
  }

  z.ls <- lapply(sort(quantiles), quant_pred, data = data, method = method,
                 formula = formula, weight = data$weight, grid = grid,
                 method.args = method.args, orientation = "x",
                 make.groups = FALSE)
  z <- z.ls[[2]]
  z[["ymin"]] <- z.ls[[1]][["y"]]
  z[["quantile.ymin"]] <- z.ls[[1]][["quantile"]]
  z[["ymax"]] <- z.ls[[3]][["y"]]
  z[["quantile.ymax"]] <- z.ls[[3]][["quantile"]]

  z[["flipped_aes"]] <- flipped_aes
  z <- ggplot2::flip_data(z, flipped_aes)
  if (!"fill" %in% colnames(z)) {
    z[["fill"]] <- "steelblue"
  }
  z
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatQuantBand <-
  ggplot2::ggproto("StatQuantBand", ggplot2::Stat,
                   setup_params = function(data, params) {
                     params$flipped_aes <-
                       ggplot2::has_flipped_aes(data, params, ambiguous = TRUE)
                     params
                   },
                   extra_params = c("na.rm", "orientation"),
                   compute_group = quant_band_compute_group_fun,
                   required_aes = c("x", "y")
  )

