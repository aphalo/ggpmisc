#' Add a curve from a fitted linear model and a label to a plot.
#'
#' \code{stat_poly_eq} fits a polynomial and generates a label with an equation and/r .
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
#' @param na.rm	a logical value indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param formula a formula object
#' @param label.fmt character string giving a format definition for converting
#'   y-integral values into character strings by means of function
#'   \code{\link{sprintf}}.
#'
#' @section Computed variables:
#' \describe{
#'   \item{eq.label}{equation for the fitted polynomial as a character string to be parsed}
#'   \item{r2.label}{$R^2 of the fitted model as a character string to be parsed}
#'   \item{x}{x position for left edge}
#'   \item{y}{y position near upper edge}
#' }
#'
#' @export
#' @family utility functions
#'

stat_poly_eq <- function(mapping = NULL, data = NULL, geom = "text",
                            formula = NULL,
                            label.fmt = "%.3g",
                            position = "identity", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatPolyEq, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(formula = formula,
                  label.fmt = label.fmt,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPolyEq <-
  ggplot2::ggproto("StatPolyEq", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            formula,
                                            label.fmt) {
                     model.fit <- lm(formula, data)
                     my.eq <- as.character(signif(polynom::as.polynomial(coef(m)), 3))
                     my.r2 <- format(summary(m)$r.squared, digits = 2)
                     out.data <- data.frame(x = min(data$x),
                                            y = max(data$y) - 0.1 * diff(range(data$y)),
                                            eq.label = gsub("x", "~italic(x)", my.eq, fixed = TRUE),
                                            r2.label = paste("italic(R)^2",  
                                                             format(summary(m)$r.squared, digits = 2), 
                                                             sep = "~`=`~"))
                   },
                   default_aes = ggplot2::aes(label = paste(..eq.label.., ..r2.label.., sep = "~~~~"),
                   ),
                   required_aes = c("x", "y")
  )

