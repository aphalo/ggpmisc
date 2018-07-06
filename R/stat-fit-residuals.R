#' Residuals from a model fit
#'
#' \code{stat_fit_residuals} fits a linear model and returns
#'    residuals ready to be plotted as points.
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
#'   define both data and aesthetics and should not inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param method character Currently only "lm" is implemented.
#' @param formula a "formula" object.
#' @param resid.type character passed to \code{residuals()} as argument for
#'   \code{type}.
#'
#' @details This stat can be used to automatically plot residuals as points
#' in a plot. At the moment it supports only linear
#' models fitted with function \code{lm()}. This stat only generates the
#' residuals.
#'
#' @section Computed variables: Data frame with same \code{nrow} as \code{data}
#'   as subset for each group containing five numeric variables. \describe{
#'   \item{x1}{x coordinates of observations} \item{x2}{x coordinates of fitted
#'   values} \item{y1}{y coordinates of observations} \item{y2}{y coordinates of
#'   fitted values} \item{residuals}{residuals from the fit}}
#'
#' @examples
#' library(ggplot2)
#' # generate artificial data
#' set.seed(4321)
#' x <- 1:100
#' y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
#' my.data <- data.frame(x, y, group = c("A", "B"), y2 = y * c(0.5,2))
#' # give a name to a formula
#' my.formula <- y ~ poly(x, 3, raw = TRUE)
#' # plot
#' ggplot(my.data, aes(x, y)) +
#'   stat_fit_residuals(formula = my.formula, resid.type = "working")
#'
#' @export
#'
stat_fit_residuals <- function(mapping = NULL, data = NULL, geom = "point",
                               method = "lm",
                               formula = NULL,
                               resid.type = NULL,
                               position = "identity",
                               na.rm = FALSE, show.legend = FALSE,
                               inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatFitResiduals, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(method = method,
                  formula = formula,
                  resid.type = resid.type,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
residuals_compute_group_fun <- function(data,
                                        scales,
                                        method,
                                        formula,
                                        resid.type) {
  force(data)
  if (method == "lm") {
    if (is.null(resid.type)) {
      fit.residuals <- stats::residuals(stats::lm(formula, data = data))
    } else {
      fit.residuals <- stats::residuals(stats::lm(formula, data = data), type = resid.type)
    }
  } else {
    stop("Method '", method, "' not yet implemented.")
  }
  data.frame(x = data$x,
             y = fit.residuals,
             y.resid = fit.residuals,
             y.resid.abs = abs(fit.residuals))
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFitResiduals <-
  ggplot2::ggproto("StatFitResiduals", ggplot2::Stat,
                   compute_group = residuals_compute_group_fun,
                   required_aes = c("x", "y")
  )


