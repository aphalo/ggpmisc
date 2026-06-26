#' @rdname stat_fit_residuals
#'
#' @family \emph{statistics} for model fits residuals
#'
#' @export
#'
stat_fit_deviations <- function(mapping = NULL,
                                data = NULL,
                                geom = "segment",
                                position = "identity",
                                ...,
                                orientation = NA,
                                method = "lm",
                                method.args = list(),
                                n.min = 2L,
                                formula = NULL,
                                fit.seed = NA,
                                na.rm = FALSE,
                                show.legend = TRUE,
                                inherit.aes = TRUE) {

  if (is.character(method)) {
    method <- trimws(method, which = "both")
    method.name <- method
  } else if (is.function(method)) {
    method.name <- deparse(substitute(method))
    if (grepl("^function[ ]*[(]", method.name[1])) {
      method.name <- "function"
    }
  } else {
    method.name <- "missing"
  }

  temp <- guess_orientation(orientation = orientation,
                            formula = formula,
                            default.formula = y ~ x,
                            formula.on.x = FALSE)
  orientation <- temp[["orientation"]]
  formula <- temp[["formula"]]

  ggplot2::layer(
    stat = StatFitDeviations,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params =
      rlang::list2(method = method,
                   method.name = method.name,
                   method.args = method.args,
                   n.min = n.min,
                   formula = formula,
                   fit.seed = fit.seed,
                   na.rm = na.rm,
                   orientation = orientation,
                   ...)
  )
}

# Define here to avoid a note in check as the imports are not seen by checks
# when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
deviations_compute_group_fun <- function(data,
                                         scales,
                                         method,
                                         method.name,
                                         method.args = list(),
                                         n.min = 2L,
                                         formula = y ~ x,
                                         fit.seed = NA,
                                         orientation = "x") {

  temp.ls <- fit_models_internal(data = data,
                                 method = method,
                                 method.name = method.name,
                                 method.args = method.args,
                                 n.min = n.min,
                                 formula = formula,
                                 fit.seed = fit.seed,
                                 orientation = orientation)
  if (!length(temp.ls) || !length(temp.ls[["fm"]])) {
    # An empty data.frame results in no plot layer when passed to geoms
    return(data.frame())
  }
  fm <- temp.ls[["fm"]]
  method.name <- temp.ls[["method.name"]]
  method.args <- temp.ls[["method.args"]]


  fitted.vals <- extract_fitted(fm, n.row = nrow(data))
  weights.ls <- extract_weights(fm, n.row = nrow(data))

  if (orientation == "y") {
    z <- data.frame(x = data$x,
                    y = data$y,
                    x.fitted = fitted.vals,
                    y.fitted = data$y,
                    weights = weights.ls[["weight.vals"]],
                    robustness.weights = weights.ls[["rob.weight.vals"]],
                    hjust = 0)
  } else {
    z <- data.frame(x = data$x,
                    y = data$y,
                    x.fitted = data$x,
                    y.fitted = fitted.vals,
                    weights = weights.ls[["weight.vals"]],
                    robustness.weights = weights.ls[["rob.weight.vals"]],
                    hjust = 0)
  }

  show_colnames(z, stat.name = "stat_fit_deviations")

  z
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFitDeviations <-
  ggplot2::ggproto("StatFitDeviations", ggplot2::Stat,
                   extra_params = c("na.rm", "orientation"),
                   compute_group = deviations_compute_group_fun,
                   dropped_aes = "weight",
                   default_aes =
                     ggplot2::aes(xend = after_stat(x.fitted),
                                  yend = after_stat(y.fitted)),
                   required_aes = c("x", "y")
  )

#' @rdname stat_fit_residuals
#'
#' @export
#'
stat_fit_fitted <- function(mapping = NULL,
                            data = NULL,
                            geom = "point",
                            position = "identity",
                            orientation = NA,
                            ...,
                            method = "lm",
                            method.args = list(),
                            n.min = 2L,
                            formula = NULL,
                            fit.seed = NA,
                            na.rm = FALSE,
                            show.legend = FALSE,
                            inherit.aes = TRUE) {

  if (is.character(method)) {
    method <- trimws(method, which = "both")
    method.name <- method
  } else if (is.function(method)) {
    method.name <- deparse(substitute(method))
    if (grepl("^function[ ]*[(]", method.name[1])) {
      method.name <- "function"
    }
  } else {
    method.name <- "missing"
  }

  temp <- guess_orientation(orientation = orientation,
                            formula = formula,
                            default.formula = y ~ x,
                            formula.on.x = FALSE)
  orientation <- temp[["orientation"]]
  formula <-  temp[["formula"]]

  ggplot2::layer(
    stat = StatFitFitted, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params =
      rlang::list2(method = method,
                   method.name = method.name,
                   method.args = method.args,
                   n.min = n.min,
                   formula = formula,
                   fit.seed = fit.seed,
                   na.rm = na.rm,
                   orientation = orientation,
                   ...)
  )
}

# Define here to avoid a note in check as the imports are not seen by checks
# when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
fitted_compute_group_fun <- function(data,
                                     scales,
                                     method,
                                     method.name,
                                     method.args,
                                     n.min = 2L,
                                     formula =  y ~ x,
                                     fit.seed = NA,
                                     return.fitted = FALSE,
                                     flipped_aes = NA,
                                     orientation = "x") {

  # we flip the model formula, not the data

  temp.ls <- fit_models_internal(data = data,
                                 method = method,
                                 method.name = method.name,
                                 method.args = method.args,
                                 n.min = n.min,
                                 formula = formula,
                                 fit.seed = fit.seed,
                                 orientation = orientation)
  if (!length(temp.ls) || !length(temp.ls[["fm"]])) {
    # An empty data.frame results in no plot layer when passed to geoms
    return(data.frame())
  }
  fm <- temp.ls[["fm"]]
  method.name <- temp.ls[["method.name"]]
  method.args <- temp.ls[["method.args"]]

  fitted.vals <- extract_fitted(fm, n.row = nrow(data))

  if (orientation == "y") {
    z <- data.frame(x = fitted.vals,
                    y = data$y)
  } else {
    z <- data.frame(x = data$x,
                    y = fitted.vals)
  }

  z$flipped_aes <- flipped_aes
  # no need to flip the results, but we record the flipping

  show_colnames(z, stat.name = "stat_fit_fitted")

  z
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#'
#' @export
#'
StatFitFitted <-
  ggplot2::ggproto("StatFitFitted", ggplot2::Stat,
                   setup_params = function(data, params) {
                     params[["flipped_aes"]] <-
                       ggplot2::has_flipped_aes(data, params, ambiguous = TRUE)
                     params
                   },
                   extra_params = c("na.rm", "orientation"),
                   compute_group = fitted_compute_group_fun,
                   dropped_aes = c("weight"),
                   required_aes = c("x", "y")
  )
