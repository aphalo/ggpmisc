# generics::glance -----------------------------------------------------------

#' @title One row summary data frame for a fitted model
#'
#' @description \code{stat_fit_glance} fits a model and returns a "tidy" version
#'   of the model's fit, using '\code{glance()} methods from packages 'broom',
#'   'broom.mixed', or other sources.
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
#' @param method character or function.
#' @param method.args,glance.args list of arguments to pass to \code{method}
#'   and to [generics::glance()], respectively.
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
#' @section Warning!: Not all `glance()` methods are defined in package 'broom'.
#'   `glance()` especializations for mixed models fits of classes `lme`, `nlme`,
#'   `lme4`, and many others are defined in package 'broom.mixed'.
#'
#' @section Handling of grouping: \code{stat_fit_glance} applies the function
#'   given by \code{method} separately to each group of observations, and
#'   factors mapped to aesthetics generate a separate group for each factor
#'   level. Because of this, \code{stat_fit_glance} is not useful for annotating
#'   plots with results from \code{t.test()}, ANOVA or ANCOVA. In such cases use
#'   the \code{stat_fit_tb()} statistic which applies the model fitting per
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
#' @return The output of the \code{glance()} methods is returned almost as is in
#'   the \code{data} object, as a data frame. The names of the columns in the
#'   returned data are consistent with those returned by method \code{glance()}
#'   from package 'broom', that will frequently differ from the name of values
#'   returned by the print methods corresponding to the fit or test function
#'   used. To explore the values returned by this statistic including the name
#'   of variables/columns, which vary depending on the model fitting function
#'   and model formula we suggest the use of
#'   \code{\link[gginnards]{geom_debug}}. An example is shown below.
#'
#' @note Although arguments passed to parameter \code{glance.args} will be
#'   passed to [generics::glance()] whether they are silently ignored or obeyed
#'   depends on each specialization of [glance()], so do carefully read the
#'   documentation for the version of [glance()] corresponding to the `method`
#'   used to fit the model.
#'
#' @family ggplot statistics for model fits
#'
#' @seealso \code{\link[broom]{broom}} and \code{broom.mixed} for details on how
#'   the tidying of the result of model fits is done.
#'
#' @export
#'
#' @examples
#' # package 'broom' needs to be installed to run these examples
#'
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   library(broom)
#'   library(quantreg)
#'
#' # Inspecting the returned data using geom_debug()
#'   if (requireNamespace("gginnards", quietly = TRUE)) {
#'     library(gginnards)
#'
#'     ggplot(mtcars, aes(x = disp, y = mpg)) +
#'       stat_smooth(method = "lm") +
#'       geom_point(aes(colour = factor(cyl))) +
#'       stat_fit_glance(method = "lm",
#'                     method.args = list(formula = y ~ x),
#'                     geom = "debug")
#'   }
#'
#' # Regression by panel example
#'   ggplot(mtcars, aes(x = disp, y = mpg)) +
#'     stat_smooth(method = "lm") +
#'     geom_point(aes(colour = factor(cyl))) +
#'     stat_fit_glance(method = "lm",
#'                     label.y = "bottom",
#'                     method.args = list(formula = y ~ x),
#'                     mapping = aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2g',
#'                                   after_stat(r.squared), after_stat(p.value))),
#'                     parse = TRUE)
#'
#' # Regression by group example
#'   ggplot(mtcars, aes(x = disp, y = mpg, colour = factor(cyl))) +
#'     stat_smooth(method = "lm") +
#'     geom_point() +
#'     stat_fit_glance(method = "lm",
#'                     label.y = "bottom",
#'                     method.args = list(formula = y ~ x),
#'                     mapping = aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2g',
#'                                   after_stat(r.squared), after_stat(p.value))),
#'                     parse = TRUE)
#'
#' # Weighted regression example
#'   ggplot(mtcars, aes(x = disp, y = mpg, weight = cyl)) +
#'     stat_smooth(method = "lm") +
#'     geom_point(aes(colour = factor(cyl))) +
#'     stat_fit_glance(method = "lm",
#'                     label.y = "bottom",
#'                     method.args = list(formula = y ~ x, weights = quote(weight)),
#'                     mapping = aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2g',
#'                                   after_stat(r.squared), after_stat(p.value))),
#'                     parse = TRUE)
#'
#' # correlation test
#'   ggplot(mtcars, aes(x = disp, y = mpg)) +
#'     geom_point() +
#'     stat_fit_glance(method = "cor.test",
#'                     label.y = "bottom",
#'                     method.args = list(formula = ~ x + y),
#'                     mapping = aes(label = sprintf('r[Pearson]~"="~%.3f~~italic(P)~"="~%.2g',
#'                                   after_stat(estimate), after_stat(p.value))),
#'                     parse = TRUE)
#'
#'   ggplot(mtcars, aes(x = disp, y = mpg)) +
#'     geom_point() +
#'     stat_fit_glance(method = "cor.test",
#'                     label.y = "bottom",
#'                     method.args = list(formula = ~ x + y, method = "spearman", exact = FALSE),
#'                     mapping = aes(label = sprintf('r[Spearman]~"="~%.3f~~italic(P)~"="~%.2g',
#'                                   after_stat(estimate), after_stat(p.value))),
#'                     parse = TRUE)
#'
#' # Quantile regression by group example
#'   ggplot(mtcars, aes(x = disp, y = mpg)) +
#'     stat_smooth(method = "lm") +
#'     geom_point() +
#'     stat_fit_glance(method = "rq",
#'                     label.y = "bottom",
#'                     method.args = list(formula = y ~ x),
#'                     mapping = aes(label = sprintf('AIC = %.3g, BIC = %.3g',
#'                                   after_stat(AIC), after_stat(BIC))))
#'
#' }
#'
stat_fit_glance <- function(mapping = NULL, data = NULL, geom = "text_npc",
                            method = "lm",
                            method.args = list(formula = y ~ x),
                            glance.args = list(),
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
                  glance.args = glance.args,
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
                                         glance.args,
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
  if ("data" %in% names(method.args)) {
    message("External 'data' passed can be inconsistent with plot!\n",
            "These data must be available at the time of printing!!!")
  } else if (any(grepl("formula|fixed|random|model", names(method.args)))) {
#    method.args <- c(method.args, list(data = quote(data)))  works in most cases and avoids copying data
    method.args <- c(method.args, list(data = data)) # cor.test() needs the actual data
  } else {
    message("Only the 'formula' interface of methods is well supported.")
    if ("x" %in% names(method.args)) {
      message("Passing data$x as 'x'.")
      method.args[["x"]] <- data[["x"]]
    }
    if ("y" %in% names(method.args)) {
      message("Passing data$y as 'y'.")
      method.args[["y"]] <- data[["y"]]
    }
  }
  mf <- do.call(method, method.args)

  glance.args <- c(list(x = quote(mf), glance.args))
  z <- do.call(generics::glance, glance.args)

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
                     ggplot2::aes(npcx = after_stat(npcx),
                                  npcy = after_stat(npcy),
                                  hjust = "inward",
                                  vjust = "inward"),
                   required_aes = c("x", "y")
  )

# generics::augment ----------------------------------------------------------

#' @title Augment data with fitted values and statistics
#'
#' @description \code{stat_fit_augment} fits a model and returns a "tidy"
#'   version of the model's data with prediction added, using '\code{augmnent()}
#'   methods from packages 'broom', 'broom.mixed', or other sources. The
#'   prediction can be added to the plot as a curve.
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
#' @param method character or function.
#' @param method.args,augment.args list of arguments to pass to \code{method}
#'   and to to \code{broom:augment}.
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
#' @section Warning!: Not all `glance()` methods are defined in package 'broom'.
#'   `glance()` especializations for mixed models fits of classes `lme`, `nlme`,
#'   `lme4`, and many others are defined in package 'broom.mixed'.
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
#'   \code{generics::augment} methods. This renaming is needed so that the geom
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
#' @note Although arguments passed to parameter \code{augment.args} will be
#'   passed to [generics::augment()] whether they are silently ignored or obeyed
#'   depends on each specialization of [augment()], so do carefully read the
#'   documentation for the version of [augment()] corresponding to the `method`
#'   used to fit the model.
#'
#' @family ggplot statistics for model fits
#'
#' @seealso \code{\link[broom]{broom}} and \code{broom.mixed} for details on how
#'   the tidying of the result of model fits is done.
#'
#' @export
#'
#' @examples
#' # package 'broom' needs to be installed to run these examples
#'
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   library(broom)
#'   library(quantreg)
#'
#' # Inspecting the returned data using geom_debug()
#'   if (requireNamespace("gginnards", quietly = TRUE)) {
#'     library(gginnards)
#'
#' # Regression by panel
#'     ggplot(mtcars, aes(x = disp, y = mpg)) +
#'       geom_point(aes(colour = factor(cyl))) +
#'       stat_fit_augment(method = "lm",
#'                        method.args = list(formula = y ~ x),
#'                        geom = "debug",
#'                        summary.fun = colnames)
#'   }
#'
#' # Regression by panel example
#'   ggplot(mtcars, aes(x = disp, y = mpg)) +
#'     geom_point(aes(colour = factor(cyl))) +
#'     stat_fit_augment(method = "lm",
#'                      method.args = list(formula = y ~ x))
#'
#' # Residuals from regression by panel example
#'   ggplot(mtcars, aes(x = disp, y = mpg)) +
#'     geom_hline(yintercept = 0, linetype = "dotted") +
#'     stat_fit_augment(geom = "point",
#'                      method = "lm",
#'                      method.args = list(formula = y ~ x),
#'                      y.out = ".resid")
#'
#' # Regression by group example
#'   ggplot(mtcars, aes(x = disp, y = mpg, colour = factor(cyl))) +
#'     geom_point() +
#'     stat_fit_augment(method = "lm",
#'                      method.args = list(formula = y ~ x))
#'
#' # Residuals from regression by group example
#'   ggplot(mtcars, aes(x = disp, y = mpg, colour = factor(cyl))) +
#'     geom_hline(yintercept = 0, linetype = "dotted") +
#'     stat_fit_augment(geom = "point",
#'                      method.args = list(formula = y ~ x),
#'                      y.out = ".resid")
#'
#' # Weighted regression example
#'   ggplot(mtcars, aes(x = disp, y = mpg, weight = cyl)) +
#'     geom_point(aes(colour = factor(cyl))) +
#'     stat_fit_augment(method = "lm",
#'                      method.args = list(formula = y ~ x,
#'                                         weights = quote(weight)))
#'
#' # Residuals from weighted regression example
#'   ggplot(mtcars, aes(x = disp, y = mpg, weight = cyl)) +
#'     geom_hline(yintercept = 0, linetype = "dotted") +
#'     stat_fit_augment(geom = "point",
#'                      method.args = list(formula = y ~ x,
#'                                         weights = quote(weight)),
#'                      y.out = ".resid")
#'
#' # Quantile regression
#'   ggplot(mtcars, aes(x = disp, y = mpg)) +
#'     geom_point() +
#'     stat_fit_augment(method = "rq",
#'                     label.y = "bottom")
#'
#' }
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
  if (is.character(method)) method <- match.fun(method)
#  data <- data[order(data[["x"]]), ]
  if ("data" %in% names(method.args)) {
    message("External 'data' passed can be inconsistent with plot!\n",
            "These data must be available at the time of printing!!!")
  } else if (any(grepl("formula|fixed|random|model", names(method.args)))) {
#    method.args <- c(method.args, list(data = quote(data)))
    method.args <- c(method.args, list(data = data))
  } else {
    message("Only the 'formula' interface of methods is well supported.")
    if ("x" %in% names(method.args)) {
      message("Passing data$x as 'x'.")
      method.args[["x"]] <- data[["x"]]
    }
    if ("y" %in% names(method.args)) {
      message("Passing data$y as 'y'.")
      method.args[["y"]] <- data[["y"]]
    }
  }
  mf <- do.call(method, method.args)

  augment.args <- c(list(x = mf), augment.args)
  z <- do.call(generics::augment, augment.args)

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
                     ggplot2::aes(ymax = after_stat(y + .se.fit * t.value),
                                  ymin = after_stat(y - .se.fit * t.value)),
                   required_aes = c("x", "y")
)

# generics::tidy -------------------------------------------------------------

#' @title One row data frame with fitted parameter estimates
#'
#' @description \code{stat_fit_tidy} fits a model and returns a "tidy" version
#'   of the model's summary, using '\code{tidy()} methods from packages 'broom',
#'   'broom.mixed', or other sources. To add the summary in tabular form use
#'   \code{\link{stat_fit_tb}} instead of this statistic. When using
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
#' @param method character or function.
#' @param method.args,tidy.args list of arguments to pass to \code{method},
#'   and to [generics::tidy], respectively.
#' @param label.x,label.y \code{numeric} with range 0..1 or character.
#'   Coordinates to be used for positioning the output, expressed in "normalized
#'   parent coordinates" or character string. If too short they will be
#'   recycled.
#' @param hstep,vstep numeric in npc units, the horizontal and vertical step
#'   used between labels for different groups.
#' @param sanitize.names logical If true sanitize column names in the returned
#'   \code{data} with R's \code{make.names()} function.
#'
#' @details \code{stat_fit_tidy} together with \code{\link{stat_fit_glance}}
#'   and \code{\link{stat_fit_augment}}, based on package 'broom' can be used
#'   with a broad range of model fitting functions as supported at any given
#'   time by 'broom'. In contrast to \code{\link{stat_poly_eq}} which can
#'   generate text or expression labels automatically, for these functions the
#'   mapping of aesthetic \code{label} needs to be explicitly supplied in the
#'   call, and labels built on the fly.
#'
#'   A ggplot statistic receives as data a data frame that is not the one passed
#'   as argument by the user, but instead a data frame with the variables mapped
#'   to aesthetics. In other words, it respects the grammar of graphics and
#'   consequently within arguments passed through \code{method.args} names of
#'   aesthetics like $x$ and $y$ should be used instead of the original variable
#'   names, while data is automatically passed the data frame. This helps ensure
#'   that the model is fitted to the same data as plotted in other layers.
#'
#' @section Warning!: Not all `glance()` methods are defined in package 'broom'.
#'   `glance()` specializations for mixed models fits of classes `lme`, `nlme`,
#'   `lme4`, and many others are defined in package 'broom.mixed'.
#'
#' @section Handling of grouping: \code{stat_fit_tidy} applies the function
#'   given by \code{method} separately to each group of observations; in ggplot2
#'   factors mapped to aesthetics generate a separate group for each level.
#'   Because of this, \code{stat_fit_tidy} is not useful for annotating plots
#'   with results from \code{t.test()} or ANOVA or ANCOVA. In such cases use
#'   instead \code{stat_fit_tb()} which applies the model fitting per panel.
#'
#' @return The output of \code{tidy()} is returned after reshaping it into a
#'   single row. Grouping is respected, and the model fitted separately to each
#'   group of data. The returned \code{data} object has one row for each group
#'   within a panel. To use the intercept, note that output of \code{tidy()} is
#'   renamed from \code{(Intercept)} to \code{Intercept}. Otherwise, the names
#'   of the columns in the returned data are based on those returned by the
#'   \code{tidy()} method for the model fit class returned by the fit function.
#'   These will frequently differ from the name of values returned by the print
#'   methods corresponding to the fit or test function used. To explore the
#'   values returned by this statistic including the name of variables/columns,
#'   which vary depending on the model fitting function and model formula, we
#'   suggest the use of \code{\link[gginnards]{geom_debug}}. An example is shown
#'   below. Names of columns as returned by default are not always syntactically
#'   valid R names making it necessary to use back ticks to access them.
#'   Syntactically valid names are guaranteed if \code{sanitize.names = TRUE} is
#'   added to the call.
#'
#'   To explore the values returned by this statistic, which vary depending on
#'   the model fitting function and model formula we suggest the use of
#'   \code{\link[gginnards]{geom_debug}}. An example is shown below.
#'
#' @note The statistic \code{stat_fit_tidy} can be used only with
#'   \code{methods} that accept formulas under any formal parameter name and a
#'   \code{data} argument. Use \code{ggplot2::stat_smooth()} instead of
#'   \code{stat_fit_augment} in production code if the additional features are
#'   not needed.
#'
#' @note Although arguments passed to parameter \code{tidy.args} will be
#'   passed to [generics::tidy()] whether they are silently ignored or obeyed
#'   depends on each specialization of [tidy()], so do carefully read the
#'   documentation for the version of [tidy()] corresponding to the `method`
#'   used to fit the model. You will also need to manually intall the package,
#'   such as 'broom', where the tidier you intend to use are defined.
#'
#' @family ggplot statistics for model fits
#'
#' @seealso \code{\link[broom]{broom}} and \code{broom.mixed} for details on how
#'   the tidying of the result of model fits is done.
#'
#' @export
#'
#' @examples
#' # package 'broom' needs to be installed to run these examples
#'
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   library(broom)
#'   library(quantreg)
#'
#' # Inspecting the returned data using geom_debug()
#'   if (requireNamespace("gginnards", quietly = TRUE)) {
#'     library(gginnards)
#'
#' # This provides a quick way of finding out the names of the variables that
#' # are available for mapping to aesthetics. This is specially important for
#' # this stat as these names depend on the specific tidy() method used, which
#' # depends on the method used, such as lm(), used to fit the model.
#'
#' # Regression by panel, default column names
#'   ggplot(mtcars, aes(x = disp, y = mpg)) +
#'     stat_smooth(method = "lm", formula = y ~ x + I(x^2)) +
#'     geom_point(aes(colour = factor(cyl))) +
#'     stat_fit_tidy(method = "lm",
#'                   method.args = list(formula = y ~ x + I(x^2)),
#'                   geom = "debug")
#'
#' # Regression by panel, sanitized column names
#'   ggplot(mtcars, aes(x = disp, y = mpg)) +
#'     stat_smooth(method = "lm", formula = y ~ x + I(x^2)) +
#'     geom_point(aes(colour = factor(cyl))) +
#'     stat_fit_tidy(method = "lm",
#'                   method.args = list(formula = y ~ x + I(x^2)),
#'                   geom = "debug", sanitize.names = TRUE)
#' }
#'
#' # Regression by panel example
#'   ggplot(mtcars, aes(x = disp, y = mpg)) +
#'     stat_smooth(method = "lm") +
#'     geom_point(aes(colour = factor(cyl))) +
#'     stat_fit_tidy(method = "lm",
#'                   label.x = "right",
#'                   method.args = list(formula = y ~ x),
#'                   mapping = aes(label = sprintf("Slope = %.3g\np-value = %.3g",
#'                                                 after_stat(x_estimate),
#'                                                 after_stat(x_p.value))))
#'
#' # Regression by group example
#'   ggplot(mtcars, aes(x = disp, y = mpg, colour = factor(cyl))) +
#'     stat_smooth(method = "lm") +
#'     geom_point() +
#'     stat_fit_tidy(method = "lm",
#'                   label.x = "right",
#'                   method.args = list(formula = y ~ x),
#'                   mapping = aes(label = sprintf("Slope = %.3g, p-value = %.3g",
#'                                                 after_stat(x_estimate),
#'                                                 after_stat(x_p.value))))
#'
#' # Weighted regression example
#'   ggplot(mtcars, aes(x = disp, y = mpg, weight = cyl)) +
#'     stat_smooth(method = "lm") +
#'     geom_point(aes(colour = factor(cyl))) +
#'     stat_fit_tidy(method = "lm",
#'                   label.x = "right",
#'                   method.args = list(formula = y ~ x, weights = quote(weight)),
#'                   mapping = aes(label = sprintf("Slope = %.3g\np-value = %.3g",
#'                                                 after_stat(x_estimate),
#'                                                 after_stat(x_p.value))))
#'
#' # Correlation test
#'   ggplot(mtcars, aes(x = disp, y = mpg)) +
#'     stat_smooth(method = "lm") +
#'     geom_point() +
#'     stat_fit_tidy(method = "cor.test",
#'                   label.y = "bottom",
#'                   method.args = list(formula = ~ x + y),
#'                   mapping = aes(label = sprintf("R = %.3g\np-value = %.3g",
#'                                                 after_stat(`_estimate`),
#'                                                 after_stat(`_p.value`))))
#'
#' # Quantile regression
#'   ggplot(mtcars, aes(x = disp, y = mpg)) +
#'     stat_smooth(method = "lm") +
#'     geom_point() +
#'     stat_fit_tidy(method = "rq",
#'                   label.y = "bottom",
#'                   method.args = list(formula = y ~ x),
#'                   tidy.args = list(se.type = "nid"),
#'                   mapping = aes(label = sprintf("Slope = %.3g\np-value = %.3g",
#'                                                 after_stat(x_estimate),
#'                                                 after_stat(x_p.value))))
#'
#' }
#'
stat_fit_tidy <- function(mapping = NULL, data = NULL, geom = "text_npc",
                          method = "lm",
                          method.args = list(formula = y ~ x),
                          tidy.args = list(),
                          label.x = "left", label.y = "top",
                          hstep = 0,
                          vstep = NULL,
                          sanitize.names = FALSE,
                          position = "identity",
                          na.rm = FALSE, show.legend = FALSE,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatFitTidy, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(method = method,
                  method.args = method.args,
                  tidy.args = tidy.args,
                  label.x = label.x,
                  label.y = label.y,
                  hstep = hstep,
                  vstep = ifelse(is.null(vstep),
                                 ifelse(grepl("label", geom),
                                        0.125,
                                        0.075),
                                 vstep),
                  sanitize.names = sanitize.names,
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
                                       tidy.args,
                                       label.x,
                                       label.y,
                                       hstep,
                                       vstep,
                                       sanitize.names,
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

  if (is.character(method)) method <- match.fun(method)
  if ("data" %in% names(method.args)) {
    message("External 'data' passed can be inconsistent with plot!\n",
            "These data must be available at the time of printing!!!")
  } else if (any(grepl("formula|fixed|random|model", names(method.args)))) {
#    method.args <- c(method.args, list(data = quote(data)))
    method.args <- c(method.args, list(data = data))
  } else {
    message("Only the 'formula' interface of methods is well supported.")
    if ("x" %in% names(method.args)) {
      message("Passing data$x as 'x'.")
      method.args[["x"]] <- quote(data[["x"]])
    }
    if ("y" %in% names(method.args)) {
      message("Passing data$y as 'y'.")
      method.args[["y"]] <- quote(data[["y"]])
    }
  }
  mf <- do.call(method, method.args)
  tidy.args <- c(list(x = quote(mf)), tidy.args)
  mf.td <- do.call(generics::tidy, tidy.args)
  col.names <- colnames(mf.td)
  clean.term.names <- gsub("(Intercept)", "Intercept", mf.td[["term"]], fixed = TRUE)
  z <- as.data.frame(t(mf.td[["estimate"]]))
  names(z) <- paste(clean.term.names, "estimate", sep = "_")
  if ("std.error" %in% col.names) {
    z.std.error <- as.data.frame(t(mf.td[["std.error"]]))
    names(z.std.error) <- paste(clean.term.names, "se", sep = "_")
    z <- cbind(z, z.std.error)
  }
  if (exists("statistic", mf.td, inherits = FALSE)) {
    z.statistic <- as.data.frame(t(mf.td[["statistic"]]))
    names(z.statistic) <- paste(clean.term.names, "stat", sep = "_")
    z <- cbind(z, z.statistic)
  }
  for (col in setdiff(col.names, c("term", "estimate", "intercept", "std.error", "statistic"))) {
    zz <- as.data.frame(t(mf.td[[col]]))
    names(zz) <- paste(clean.term.names, col, sep = "_")
    z <- cbind(z, zz)
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

  if (sanitize.names) {
    names(z) <- make.names(names(z), unique = TRUE)
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
                     ggplot2::aes(npcx = after_stat(npcx),
                                  npcy = after_stat(npcy),
                                  hjust = "inward",
                                  vjust = "inward"),
                   required_aes = c("x", "y")
  )

