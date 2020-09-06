# broom::glance -----------------------------------------------------------

#' @title One row summary data frame for a fitted model
#'
#' @description \code{stat_fit_glance} fits a model and returns a summary
#'   "glance" of the model's statistics, using package 'broom'.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs
#'   to be set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific data set - only needed if you want to override
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
#' @param na.rm	a logical indicating whether NA values should be stripped before
#'   the computation proceeds.
#' @param method character.
#' @param method.args list of arguments to pass to \code{method}.
#' @param label.x,label.y \code{numeric} with range 0..1 "normalized parent
#'   coordinates" (npc units) or character if using \code{geom_text_npc()} or
#'   \code{geom_label_npc()}. If using \code{geom_text()} or \code{geom_label()}
#'   numeric in native data units. If too short they will be recycled.
#' @param hstep,vstep numeric in npc units, the horizontal and vertical step
#'   used between labels for different groups.
#'
#' @details \code{stat_fit_glance} together with \code{\link{stat_fit_tidy}}
#'   and \code{\link{stat_fit_augment}}, based on package 'broom' can be used
#'   with a broad range of model fitting functions as supported at any given
#'   time by package 'broom'. In contrast to \code{\link{stat_poly_eq}} wich can
#'   generate text or expression labels automatically, for these functions the
#'   mapping of aesthetic \code{label} needs to be explicitly supplied in the
#'   callm, and labels built on the fly.
#'
#'   A ggplot statistic receives as data a data frame that is not the one passed
#'   as argument by the user, but instead a data frame with the variables mapped
#'   to aesthetics. In other words, it respects the grammar of graphics and
#'   consequently within arguments passed through \code{method.args} names of
#'   aesthetics like $x$ and $y$ should be used intead of the original variable
#'   names, while data is automatically passed the data frame. This helps ensure
#'   that the model is fitted to the same data as plotted in other layers.
#'
#' @section Handling of grouping: \code{stat_fit_glance} applies the function
#'   given by \code{method} separately to each group of observations, and
#'   factors mapped to aesthetics generate a separate group for each factor
#'   level. Because of this, \code{stat_fit_glance} is not useful for annotating
#'   plots with results from \code{t.test()}, ANOVA or ANCOVA. In such cases use
#'   the \code{stat_fit_tb()} statistic which applie the model fitting per
#'   panel.
#'
#' @section Model formula required: The current implementation works only with
#'   methods that accept a formula as argument and which have a \code{data}
#'   parameter through which a data frame can be passed. For example,
#'   \code{lm()} should be used with the formula interface, as the evaluation of
#'   \code{x} and \code{y} needs to be delayed until the internal \code{object}
#'   of the ggplot is available.  With some methods like \code{cor.test()} the
#'   data embedded in the \code{"ggplot"} object cannot be automatically passed
#'   as argument for the \code{data} parameter of the test or model fit
#'   function.
#'
#' @section Computed variables: The output of \code{glance()} is
#'   returned almost as is in the \code{data} object.
#'   The names of the columns in the returned data are consitent with those
#'   returned by method \code{glance()} from package 'broom', that will
#'   frequently differ from the name of values returned by the print methods
#'   corresponding to the fit or test function used. To explore the values
#'   returned by this statistic, which vary depending on the model fitting
#'   function and model formula we suggest the use of
#'   \code{\link[gginnards]{geom_debug}}. An example is shown below.
#'
#' @family ggplot2 statistics based on 'broom'.
#'
#' @seealso \code{\link[broom]{broom}}
#'
#' @export
#'
#' @examples
#' library(gginnards)
#' # Regression by panel example, using geom_debug.
#' ggplot(mtcars, aes(x = disp, y = mpg)) +
#'   stat_smooth(method = "lm") +
#'   geom_point(aes(colour = factor(cyl))) +
#'   stat_fit_glance(method = "lm",
#'                   method.args = list(formula = y ~ x),
#'                   geom = "debug")
#'
#' # Regression by panel example
#' ggplot(mtcars, aes(x = disp, y = mpg)) +
#'   stat_smooth(method = "lm") +
#'   geom_point(aes(colour = factor(cyl))) +
#'   stat_fit_glance(method = "lm",
#'                   label.y = "bottom",
#'                   method.args = list(formula = y ~ x),
#'                   mapping = aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2g',
#'                                 stat(r.squared), stat(p.value))),
#'                   parse = TRUE)
#'
#' # Regression by group example
#' ggplot(mtcars, aes(x = disp, y = mpg, colour = factor(cyl))) +
#'   stat_smooth(method = "lm") +
#'   geom_point() +
#'   stat_fit_glance(method = "lm",
#'                   label.y = "bottom",
#'                   method.args = list(formula = y ~ x),
#'                   mapping = aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2g',
#'                                 stat(r.squared), stat(p.value))),
#'                   parse = TRUE)
#'
#' # Weighted regression example
#' ggplot(mtcars, aes(x = disp, y = mpg, weight = cyl)) +
#'   stat_smooth(method = "lm") +
#'   geom_point(aes(colour = factor(cyl))) +
#'   stat_fit_glance(method = "lm",
#'                   label.y = "bottom",
#'                   method.args = list(formula = y ~ x, weights = quote(weight)),
#'                   mapping = aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2g',
#'                                 stat(r.squared), stat(p.value))),
#'                   parse = TRUE)
#'
stat_fit_glance <- function(mapping = NULL, data = NULL, geom = "text_npc",
                            method = "lm",
                            method.args = list(formula = y ~ x),
                            label.x = "left", label.y = "top",
                            hstep = 0,
                            vstep = 0.075,
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
                  hstep = hstep,
                  vstep = vstep,
                  npc.used = grepl("_npc", geom),
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
                                         label.y,
                                         hstep,
                                         vstep,
                                         npc.used) {

  force(data) # needed because it appears only wihtin quote()

  if (length(unique(data$x)) < 2) {
    # Not enough data to perform fit
    return(data.frame())
  }

  group.idx <- abs(data$group[1])
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

  if (is.character(method)) method <- match.fun(method)
  if (!any(grepl("formula|fixed|random", names(method.args)))) {
    warning("Only the 'formula' interface of methods is supported.")
    return(data.frame())
  }
  if ("data" %in% names(method.args)) {
    message("External 'data' passed to method, possibly inconsistent with plot!\n",
            "These data must be available at the time of printing!!!")
  } else {
    method.args <- c(method.args, list(data = quote(data)))
  }
  mf <- do.call(method, method.args)
  z <- broom::glance(mf)

  n.labels <- nrow(z)
  if (length(label.x) != n.labels) {
    if (length(label.x) != 1L) {
      warning("Length of 'label.x' is different from number of labels")
    }
    if (length(label.x) < n.labels) {
      label.x <- rep(label.x[1], n.labels)
    } else {
      label.x <- label.x[1:n.labels]
    }
  }
  if (length(label.y) != n.labels) {
    if (length(label.y) != 1L) {
      warning("Length of 'label.y' is different from number of labels")
    }
    if (length(label.y) < n.labels) {
      label.y <- rep(label.y[1], n.labels)
    } else {
      label.y <- label.y[1:n.labels]
    }
  }

  if (npc.used) {
    margin.npc <- 0.05
  } else {
    margin.npc <- 0
  }

  hsteps <- hstep * (group.idx - 1L)
  margin.npc <- 0.05
  if (is.character(label.x)) {
    label.x <- switch(label.x,
                      right = (1 - margin.npc) - hsteps,
                      center = 0.5 - hsteps,
                      centre = 0.5 - hsteps,
                      middle = 0.5 - hsteps,
                      left = (0 + margin.npc) +  hsteps
    )
    if (!npc.used) {
      x.delta <- abs(diff(range(data$x)))
      x.min <- min(data$x)
      label.x <- label.x * x.delta + x.min
    }
  }

  vsteps <- vstep * (group.idx - 1L)
  if (is.character(label.y)) {
    label.y <- switch(label.y,
                      top = (1 - margin.npc) - vsteps,
                      center = 0.5 - vsteps,
                      centre = 0.5 - vsteps,
                      middle = 0.5 - vsteps,
                      bottom = (0 + margin.npc) + vsteps
    )
    if (!npc.used) {
      y.delta <- abs(diff(range(data$y)))
      y.min <- min(data$y)
      label.y <- label.y * y.delta + y.min
    }
  }
  if (npc.used) {
    z$npcx <- label.x
    z$x <- NA_real_
    z$npcy <- label.y
    z$y <- NA_real_
  } else {
    z$x <- label.x
    z$npcx <- NA_real_
    z$y <- label.y
    z$npcy <- NA_real_
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
                     ggplot2::aes(npcx = stat(npcx),
                                  npcy = stat(npcy),
                                  hjust = "inward",
                                  vjust = "inward"),
                   required_aes = c("x", "y")
  )

# broom::augment ----------------------------------------------------------

#' @title Augment data with fitted values and statistics
#'
#' @description \code{stat_fit_augment} fits a model and returns the data
#'   augmented with information from the fitted model, using package 'broom'.
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
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	logical indicating whether NA values should be stripped before
#'   the computation proceeds.
#' @param method character.
#' @param method.args list of arguments to pass to \code{method}.
#' @param augment.args list of arguments to pass to \code{broom:augment}.
#' @param level numeric Level of confidence interval to use (0.95 by default)
#' @param y.out character (or numeric) index to column to return as \code{y}.
#'
#' @details \code{stat_fit_augment} together with \code{\link{stat_fit_glance}}
#'   and \code{\link{stat_fit_tidy}}, based on package 'broom' can be used
#'   with a broad range of model fitting functions as supported at any given
#'   time by 'broom'. In contrast to \code{\link{stat_poly_eq}} wich can
#'   generate text or expression labels automatically, for these functions the
#'   mapping of aesthetic \code{label} needs to be explicitly supplied in the
#'   call, and labels built on the fly.
#'
#'   A ggplot statistic receives as data a data frame that is not the one passed
#'   as argument by the user, but instead a data frame with the variables mapped
#'   to aesthetics. In other words, it respects the grammar of graphics and
#'   consequently within arguments passed through \code{method.args} names of
#'   aesthetics like $x$ and $y$ should be used intead of the original variable
#'   names, while data is automatically passed the data frame. This helps ensure
#'   that the model is fitted to the same data as plotted in other layers.
#'
#' @section Handling of grouping: \code{stat_fit_augment} applies the function
#'   given by \code{method} separately to each group of observations; in ggplot2
#'   factors mapped to aesthetics generate a separate group for each level.
#'   Because of this, \code{stat_fit_augment} is not useful for annotating plots
#'   with results from \code{t.test()} or ANOVA or ANCOVA. In such cases use
#'   instead \code{stat_fit_tb()} which applies the model fitting per panel.
#'
#' @section Computed variables: The output of \code{augment()} is
#'   returned as is, except for \code{y} which is set based on \code{y.out} and
#'   \code{y.observed} which preserves the \code{y} returned by the
#'   \code{broom::augment} methods. This renaming is needed so that the geom
#'   works as expected.
#'
#'   To explore the values returned by this statistic, which vary depending
#'   on the model fitting function and model formula we suggest the use of
#'   \code{\link[gginnards]{geom_debug}}. An example is shown below.
#'
#' @note The statistic \code{stat_fit_augment} can be used only with
#'   \code{methods} that accept formulas under any formal parameter name and a
#'   \code{data} argument. Use \code{ggplot2::stat_smooth()} instead of
#'   \code{stat_fit_augment} in production code if the additional features are
#'   not needed.
#'
#' @family ggplot2 statistics based on 'broom'.
#'
#' @seealso \code{\link[broom]{broom}}
#'
#' @export
#'
#' @examples
#' library(gginnards)
#' # Regression by panel, using geom_debug() to explore computed variables
#' ggplot(mtcars, aes(x = disp, y = mpg)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   stat_fit_augment(method = "lm",
#'                    method.args = list(formula = y ~ x),
#'                    geom = "debug",
#'                    summary.fun = colnames)
#'
#' # Regression by panel example
#' ggplot(mtcars, aes(x = disp, y = mpg)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   stat_fit_augment(method = "lm",
#'                    method.args = list(formula = y ~ x))
#'
#' # Residuals from regression by panel example
#' ggplot(mtcars, aes(x = disp, y = mpg)) +
#'   geom_hline(yintercept = 0, linetype = "dotted") +
#'   stat_fit_augment(geom = "point",
#'                    method = "lm",
#'                    method.args = list(formula = y ~ x),
#'                    y.out = ".resid")
#'
#' # Regression by group example
#' ggplot(mtcars, aes(x = disp, y = mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   stat_fit_augment(method = "lm",
#'                    method.args = list(formula = y ~ x))
#'
#' # Residuals from regression by group example
#' ggplot(mtcars, aes(x = disp, y = mpg, colour = factor(cyl))) +
#'   geom_hline(yintercept = 0, linetype = "dotted") +
#'   stat_fit_augment(geom = "point",
#'                    method.args = list(formula = y ~ x),
#'                    y.out = ".resid")
#'
#' # Weighted regression example
#' ggplot(mtcars, aes(x = disp, y = mpg, weight = cyl)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   stat_fit_augment(method = "lm",
#'                    method.args = list(formula = y ~ x,
#'                                  weights = quote(weight)))
#'
#' # Residuals from weighted regression example
#' ggplot(mtcars, aes(x = disp, y = mpg, weight = cyl)) +
#'   geom_hline(yintercept = 0, linetype = "dotted") +
#'   stat_fit_augment(geom = "point",
#'                    method.args = list(formula = y ~ x,
#'                                  weights = quote(weight)),
#'                    y.out = ".resid")
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
  force(data)
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
  method.args <- c(method.args, list(data = quote(data)))
  if (is.character(method)) method <- match.fun(method)
  mf <- do.call(method, method.args)
  augment.args <- c(list(x = mf), augment.args)
  z <- do.call(broom::augment, augment.args)
  z <- plyr::colwise(unAsIs)(z)
  tibble::as_tibble(z)
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
  z
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFitAugment <-
  ggplot2::ggproto("StatFitAugment",
                   ggplot2::Stat,
                   compute_group = fit_augment_compute_group_fun,
                   default_aes =
                     ggplot2::aes(ymax = stat(y + .se.fit * t.value),
                                  ymin = stat(y - .se.fit * t.value)),
                   required_aes = c("x", "y")
)

# broom::tidy -------------------------------------------------------------

#' @title One row data frame with fitted parameter estimates
#'
#' @description \code{stat_fit_tidy} fits a model and returns a "tidy" version
#'   of the model's summary, using package 'broom'. To add the summary in
#'   tabular form use \code{\link{stat_fit_tb}}. When using
#'   \code{stat_fit_tidy()} you will most likely want to change the default
#'   mapping for label.
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
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param method character.
#' @param method.args list of arguments to pass to \code{method}.
#' @param label.x,label.y \code{numeric} with range 0..1 or character.
#'   Coordinates to be used for positioning the output, expressed in "normalized
#'   parent coordinates" or character string. If too short they will be
#'   recycled.
#' @param hstep,vstep numeric in npc units, the horizontal and vertical step
#'   used between labels for different groups.
#'
#' @details \code{stat_fit_tidy} together with \code{\link{stat_fit_glance}}
#'   and \code{\link{stat_fit_augment}}, based on package 'broom' can be used
#'   with a broad range of model fitting functions as supported at any given
#'   time by 'broom'. In contrast to \code{\link{stat_poly_eq}} wich can
#'   generate text or expression labels automatically, for these functions the
#'   mapping of aesthetic \code{label} needs to be explicitly supplied in the
#'   call, and labels built on the fly.
#'
#'   A ggplot statistic receives as data a data frame that is not the one passed
#'   as argument by the user, but instead a data frame with the variables mapped
#'   to aesthetics. In other words, it respects the grammar of graphics and
#'   consequently within arguments passed through \code{method.args} names of
#'   aesthetics like $x$ and $y$ should be used intead of the original variable
#'   names, while data is automatically passed the data frame. This helps ensure
#'   that the model is fitted to the same data as plotted in other layers.
#'
#' @section Handling of grouping: \code{stat_fit_tidy} applies the function
#'   given by \code{method} separately to each group of observations; in ggplot2
#'   factors mapped to aesthetics generate a separate group for each level.
#'   Because of this, \code{stat_fit_tidy} is not useful for annotating plots
#'   with results from \code{t.test()} or ANOVA or ANCOVA. In such cases use
#'   instead \code{stat_fit_tb()} which applies the model fitting per panel.
#'
#' @section Computed variables: The output of \code{tidy()} is returned after
#'   reshaping it into a single row. Grouping is respected, and the model fit
#'   separatately to each group of data. The returned \code{data} object has one
#'   row for each group within a panel. To use the intercept, note that output
#'   of \code{tidy()} is renamed from \code{(Intercept)} to \code{Intercept}.
#'
#'   To explore the values returned by this statistic, which vary depending
#'   on the model fitting function and model formula we suggest the use of
#'   \code{\link[gginnards]{geom_debug}}. An example is shown below.
#'
#' @note The statistic \code{stat_fit_augment} can be used only with
#'   \code{methods} that accept formulas under any formal parameter name and a
#'   \code{data} argument. Use \code{ggplot2::stat_smooth()} instead of
#'   \code{stat_fit_augment} in production code if the additional features are
#'   not needed.
#'
#' @family ggplot2 statistics based on 'broom'.
#'
#' @seealso \code{\link[broom]{broom}}
#'
#' @export
#'
#' @examples
#' library(gginnards)
#' # Regression by panel, exploring computed variables with geom_debug()
#' ggplot(mtcars, aes(x = disp, y = mpg)) +
#'   stat_smooth(method = "lm") +
#'   geom_point(aes(colour = factor(cyl))) +
#'   stat_fit_tidy(method = "lm",
#'                 method.args = list(formula = y ~ x),
#'                 geom = "debug")
#'
#' # Regression by panel example
#' ggplot(mtcars, aes(x = disp, y = mpg)) +
#'   stat_smooth(method = "lm") +
#'   geom_point(aes(colour = factor(cyl))) +
#'   stat_fit_tidy(method = "lm",
#'                 label.x = "right",
#'                 method.args = list(formula = y ~ x),
#'                 mapping = aes(label = sprintf("Slope = %.3g\np-value = %.3g",
#'                                               stat(x_estimate),
#'                                               stat(x_p.value))))
#'
#' # Regression by group example
#' ggplot(mtcars, aes(x = disp, y = mpg, colour = factor(cyl))) +
#'   stat_smooth(method = "lm") +
#'   geom_point() +
#'   stat_fit_tidy(method = "lm",
#'                 label.x = "right",
#'                 method.args = list(formula = y ~ x),
#'                 mapping = aes(label = sprintf("Slope = %.3g, p-value = %.3g",
#'                                               stat(x_estimate),
#'                                               stat(x_p.value))))
#'
#' # Weighted regression example
#' ggplot(mtcars, aes(x = disp, y = mpg, weight = cyl)) +
#'   stat_smooth(method = "lm") +
#'   geom_point(aes(colour = factor(cyl))) +
#'   stat_fit_tidy(method = "lm",
#'                 label.x = "right",
#'                 method.args = list(formula = y ~ x, weights = quote(weight)),
#'                 mapping = aes(label = sprintf("Slope = %.3g\np-value = %.3g",
#'                                               stat(x_estimate),
#'                                               stat(x_p.value))))
#'
stat_fit_tidy <- function(mapping = NULL, data = NULL, geom = "text_npc",
                          method = "lm",
                          method.args = list(formula = y ~ x),
                          label.x = "left", label.y = "top",
                          hstep = 0,
                          vstep = NULL,
                          position = "identity",
                          na.rm = FALSE, show.legend = FALSE,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatFitTidy, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(method = method,
                  method.args = method.args,
                  label.x = label.x,
                  label.y = label.y,
                  hstep = hstep,
                  vstep = ifelse(is.null(vstep),
                                 ifelse(grepl("label", geom),
                                        0.125,
                                        0.075),
                                 vstep),
                  npc.used = grepl("_npc", geom),
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
fit_tidy_compute_group_fun <- function(data,
                                       scales,
                                       method,
                                       method.args,
                                       label.x,
                                       label.y,
                                       hstep,
                                       vstep,
                                       npc.used) {
  force(data)
  if (length(unique(data$x)) < 2) {
    # Not enough data to perform fit
    return(data.frame())
  }

  group.idx <- abs(data$group[1])
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
  mf.td <- broom::tidy(mf)
  z.estimate <- as.data.frame(t(mf.td[["estimate"]]))
  z.std.error <- as.data.frame(t(mf.td[["std.error"]]))
  clean.term.names <- gsub("(Intercept)", "Intercept", mf.td[["term"]], fixed = TRUE)
  names(z.estimate) <- paste(clean.term.names, "estimate", sep = "_")
  names(z.std.error) <- paste(clean.term.names, "se", sep = "_")
  z <- cbind(z.estimate, z.std.error)
  if (exists("statistic", mf.td, inherits = FALSE)) {
    z.statistic <- as.data.frame(t(mf.td[["statistic"]]))
    names(z.statistic) <- paste(clean.term.names, "stat", sep = "_")
    z <- cbind(z, z.statistic)
  }
  if (exists("p.value", mf.td, inherits = FALSE)) {
    z.p.value <- as.data.frame(t(mf.td[["p.value"]]))
    names(z.p.value) <- paste(clean.term.names, "p.value", sep = "_")
    z <- cbind(z, z.p.value)
  }

  if (npc.used) {
    margin.npc <- 0.05
  } else {
    margin.npc <- 0
  }

  hsteps <- hstep * (group.idx - 1L)
  margin.npc = 0.05
  if (is.character(label.x)) {
    label.x <- switch(label.x,
                      right = (1 - margin.npc) - hsteps,
                      center = 0.5 - hsteps,
                      centre = 0.5 - hsteps,
                      middle = 0.5 - hsteps,
                      left = (0 + margin.npc) +  hsteps
    )
    if (!npc.used) {
      x.delta <- abs(diff(range(data$x)))
      x.min <- min(data$x)
      label.x <- label.x * x.delta + x.min
    }
  }

  vsteps <- vstep * (group.idx - 1L)
  if (is.character(label.y)) {
    label.y <- switch(label.y,
                      top = (1 - margin.npc) - vsteps,
                      center = 0.5 - vsteps,
                      centre = 0.5 - vsteps,
                      middle = 0.5 - vsteps,
                      bottom = (0 + margin.npc) + vsteps
    )
    if (!npc.used) {
      y.delta <- abs(diff(range(data$y)))
      y.min <- min(data$y)
      label.y <- label.y * y.delta + y.min
    }
  }
  if (npc.used) {
    z$npcx <- label.x
    z$x <- NA_real_
    z$npcy <- label.y
    z$y <- NA_real_
  } else {
    z$x <- label.x
    z$npcx <- NA_real_
    z$y <- label.y
    z$npcy <- NA_real_
  }

  z
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFitTidy <-
  ggplot2::ggproto("StatFitTidy", ggplot2::Stat,
                   compute_group = fit_tidy_compute_group_fun,
                   default_aes =
                     ggplot2::aes(npcx = stat(npcx),
                                  npcy = stat(npcy),
                                  hjust = "inward",
                                  vjust = "inward"),
                   required_aes = c("x", "y")
  )

