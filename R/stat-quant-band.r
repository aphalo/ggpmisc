#' Predicted band from quantile regression fits
#'
#' Predicted values are computed and plotted as a band plus an optional line
#' within. Using the default quantiles, the band plotted is similar to a boxplot
#' in its interpretation.
#'
#' @details \code{stat_quant_band()} fits quantile regression and obtains
#'   predictions identically to \code{stat_quant_line()}.
#'   \code{stat_quant_band()} fits 2 or 3 quantiles in the same plot layer
#'   always display the area between the predicted regression lines for the
#'   extreme quantiles as a band. In contrast \code{stat_quant_line()} fits one
#'   or more regressions, adding a line for each. \code{stat_quant_line()}
#'   displays confidence bands for the regression lines when \code{se = TRUE}.
#'
#'   While \code{stat_quant_eq()} supports only method \code{"rq"},
#'   \code{stat_quant_line()} and \code{stat_quant_band()} support both
#'   \code{"rq"} and \code{"rqss"}, In the case of \code{"rqss"} the model
#'   formula makes normally use of \code{qss()} to formulate the spline and its
#'   constraints.
#'
#'   The minimum number of observations with distinct values in the explanatory
#'   variable can be set through parameter \code{n.min}. The default \code{n.min
#'   = 3L} is the smallest usable value. However, model fits with very few
#'   observations are of little interest and using larger values of \code{n.min}
#'   than the default is wise.
#'
#'   There are multiple uses for double regression on \code{x} and \code{y}. For
#'   example, when two variables are subject to mutual constrains, it is useful
#'   to consider both of them as explanatory and interpret the relationship
#'   based on them. 'ggpmisc' (>= 0.4.1) supports \code{orientation} making it
#'   easy implement the approach described by Cardoso (2019) under the name of
#'   "Double quantile regression".
#'
#' @inheritParams stat_quant_line
#' @param quantiles A numeric vector of length 3, with unique values in
#'   \eqn{0\ldots 1}. The three quantile regressions are mapped to \code{y},
#'   \code{ymax} and \code{ymin} aesthetics, and by default plotted as a line
#'   and band.
#'
#' @aesthetics StatQuantBand
#'
#' @return The value returned by the statistic is a data frame, that will have
#'   \code{n} rows of predicted values for three quantiles as \code{y},
#'   \code{ymin} and \code{ymax}, plus \code{x}.
#'
#' @inheritSection stat_poly_line Model formula and model fitting
#'
#' @inheritSection stat_poly_line Model fit methods supported
#'
#' @references
#' Cardoso, G. C. (2019) Double quantile regression accurately assesses
#'   distance to boundary trade-off. Methods in ecology and evolution,
#'   10(8), 1322-1331.
#'
#' @seealso \code{\link[quantreg]{rq}}, \code{\link[quantreg]{rqss}} and
#'   \code{\link[quantreg]{qss}}.
#'
#' @family 'ggpmisc' statistics for model fits
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
#' # Instead of rq() we can use rqss() to fit an additive model:
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_band(method = "rqss",
#'                   formula = y ~ qss(x))
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   stat_quant_band(method = "rqss",
#'                   formula = x ~ qss(y, constraint = "D"))
#'
#' # Regressions are automatically fit to each group (defined by categorical
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
#' # Inspecting the returned data using geom_debug_group()
#' gginnards.installed <- requireNamespace("gginnards", quietly = TRUE)
#'
#' if (gginnards.installed)
#'   library(gginnards)
#'
#' if (gginnards.installed)
#'   ggplot(mpg, aes(displ, hwy)) +
#'     stat_quant_band(geom = "debug_group")
#'
#' if (gginnards.installed)
#'   ggplot(mpg, aes(displ, hwy)) +
#'     stat_quant_band(geom = "debug_group", fm.values = TRUE)
#'
#' @export
#'
stat_quant_band <- function(mapping = NULL,
                            data = NULL,
                            geom = "smooth",
                            position = "identity",
                            ...,
                            orientation = NA,
                            quantiles = c(0.25, 0.5, 0.75),
                            formula = NULL,
                            fit.seed = NA,
                            fm.values = FALSE,
                            n = 80,
                            method = "rq",
                            method.args = list(),
                            n.min = 3L,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {

  stopifnot("Args 'formula' and/or 'data' in 'method.args'" =
              !any(c("formula", "data") %in% names(method.args)))

  # we make a character string name for the method
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

  if (grepl("^lm$|^lm[:]|^rlm$|^rlm[:]|^gls$|^gls[:]", method.name)) {
    stop("Methods 'lm', 'rlm' and 'gls' not supported, please use 'stat_poly_line()'.")
  } else if (grepl("^lmodel2$|^lmodel2[:]", method.name)) {
    stop("Method 'lmodel2' not supported, please use 'stat_ma_line()'.")
  }

  if (method.name == "rqss") {
    default.formula <- y ~ qss(x)
  } else {
    default.formula <- y ~ x
  }
  temp <- guess_orientation(orientation = orientation,
                            formula = formula,
                            default.formula = default.formula,
                            formula.on.x = TRUE)
  orientation <- temp[["orientation"]]
  formula <-  temp[["formula"]]

  quantiles <- unique(quantiles)
  if (length(quantiles) != 3) {
    stop("'quantiles' should be a vector of 3 unique quantiles, not ",
         length(quantiles), " quantiles. See 'stat_quant_line()'")
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatQuantBand,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params =
      rlang::list2(
        quantiles = quantiles,
        formula = formula,
        fit.seed = fit.seed,
        fm.values = fm.values,
        n = n,
        method = method,
        method.name = method.name,
        method.args = method.args,
        n.min = n.min,
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
                                         method,
                                         method.name,
                                         method.args = list(),
                                         n.min = 3L,
                                         lambda = 1,
                                         fit.seed = NA,
                                         fm.values = FALSE,
                                         na.rm = FALSE,
                                         flipped_aes = NA) {

  rlang::check_installed("quantreg", reason = "to use stat_quant_band()")

  data <- ggplot2::flip_data(data, flipped_aes)
  if (length(unique(data$x)) < n.min) {
    # Not enough data to perform fit
    return(data.frame())
  }

  if (is.null(data[["weight"]])) {
    data[["weight"]] <- 1
  }

  quantiles <- sort(quantiles)

  fms.ls <-  quant_helper_fun(data = data,
                              formula = formula,
                              quantiles = quantiles,
                              fit.by.quantile = TRUE,
                              method = method,
                              method.name = method.name,
                              method.args = method.args,
                              n.min = n.min,
                              fit.seed = fit.seed,
                              weight = data[["weight"]],
                              na.rm = na.rm,
                              orientation = "x")

  seq.indep <- seq(from = min(data[["x"]], na.rm = TRUE),
                   to   = max(data[["x"]], na.rm = TRUE),
                   length.out = n)
  newdata <- data.frame(x = seq.indep)

  preds.ls <- list()
  preds.names <- c("ymin", "y", "ymax")
  fms.idxs <- grep("^fm", names(fms.ls))
  for (i in seq_along(fms.idxs)) {

    fm <- fms.ls[[fms.idxs[i]]]
    if (!length(fm) || (is.atomic(fm) && is.na(fm))) {
      return(data.frame())
    }
    pred <- stats::predict(fm, newdata = newdata, level = .95,
                           type = "none", interval = "none")

    if (is.matrix(pred)) {
      preds.ls[[preds.names[i]]] <- pred[ , 1L]
    } else {
      preds.ls[[preds.names[i]]] <- pred
    }

    if (fm.values) {
      preds.ls[[paste(names(fms.ls)[[fms.idxs[i]]],
                      "class", sep = ".")]] <- class(fm)
      preds.ls[[paste(names(fms.ls)[[fms.idxs[i]]],
                      "formula.chr", sep = ".")]] <- format(formula(fm))
     }
  }

  newdata <- dplyr::bind_cols(newdata, preds.ls)
  if (!"y" %in% colnames(newdata)) {
    # y in required_aes
    newdata[["y"]] <- NA_real_
  }

  if (fm.values) {
    newdata[["n"]] <- length(resid(fm)) / length(fm[["tau"]])
    newdata[["fm.method"]] <- method.name
  }

  newdata[["flipped_aes"]] <- flipped_aes
  ggplot2::flip_data(newdata, flipped_aes)
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
                   dropped_aes = "weight",
                   required_aes = c("x", "y")
  )

