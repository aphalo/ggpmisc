#' Add a curve from a fitted linear model and a label to a plot.
#'
#' \code{stat_poly_eq} fits a polynomial and generates a label with
#'   an equation and/or coefficient of determination (R^2).
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
#' @param formula a formula object
#' @param eq.with.lhs logical indicating whether lhs of equation is to be
#'   included in label.
#'
#' @details This stat can be used to automatically annotate a plot with R^2,
#' adjusted R^2 or the fitted model equation. It supports only linear models
#' fitted with function \code{lm()}. The R^2 and adjusted R^2 annotations can be
#' used with any linear model formula. The fitted equation label is correclty
#' generated for polynomials or quasi-polynomials through the origin. Model
#' formulas can use \code{poly()} or be defined algebraically with terms of
#' powers of increasing magnitude with no missing intermediate terms, except
#' possibly for the intercept indicated by "- 1" or "-1" in the formula. The
#' validity of the \code{formula} is not checked in the current implementation,
#' and for this reason the default aesthetics sets R^2 as label for the
#' annotation. This stat only generates the label, the predicted values need
#' to be sepearately added to the plot, so to make sure that the same model
#' formula is used in all steps it is best to save the formula as an object
#' and supply this object as argument to the different statistics.
#'
#' @section Computed variables:
#'   \describe{ \item{x}{x position for left edge}
#'   \item{y}{y position near upper edge}
#'   \item{eq.label}{equation for the
#'   fitted polynomial as a character string to be parsed}
#'   \item{rr.label}{\eqn{R^2} of the fitted model as a character string to be parsed}
#'   \item{adj.rr.label}{Adjusted \eqn{R^2} of the fitted model as a character string
#'   to be parsed}
#'   \item{hjust}{Set to zero to override the default of the "text" geom.}}
#'
#' @examples
#' library(ggplot2)
#' # generate artificial data
#' set.seed(4321)
#' x <- 1:100
#' y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
#' my.data <- data.frame(x, y, group = c("A", "B"), y2 = y * c(0.5,2))
#' # give a name to a formula
#' formula <- y ~ poly(x, 3, raw = TRUE)
#' # plot
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", formula = formula) +
#'   stat_poly_eq(formula = formula, parse = TRUE)
#'
#' @export
#'
stat_poly_eq <- function(mapping = NULL, data = NULL, geom = "text",
                         formula = NULL, eq.with.lhs = TRUE,
                         position = "identity",
                         na.rm = FALSE, show.legend = FALSE,
                         inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatPolyEq, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(formula = formula, eq.with.lhs = eq.with.lhs,
                  na.rm = na.rm,
                  ...)
  )
}

# Put here to avoid a note in check as the import from 'polynom' is not seen
# when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
compute_group_fun <- function(data,
                              scales,
                              formula,
                              eq.with.lhs) {
  mf <- lm(formula, data)
  coefs <- coef(mf)
  formula.rhs.chr <- as.character(formula)[3]
  if (grepl("-1", formula.rhs.chr) || grepl("- 1", formula.rhs.chr)) {
    coefs <- c(0, coefs)
  }
  rr <- summary(mf)$r.squared
  adj.rr <- summary(mf)$adj.r.squared
  eq.char <- as.character(signif(polynom::as.polynomial(coefs), 3))
  if (eq.with.lhs) {
    eq.char <- paste("italic(y)", eq.char, sep = "~`=`~")
  }
  rr.char <- format(rr, digits = 2)
  adj.rr.char <- format(adj.rr, digits = 2)
  data.frame(x = min(data$x),
             y = max(data$y) - 0.1 * diff(range(data$y)),
             eq.label = gsub("x", "~italic(x)", eq.char, fixed = TRUE),
             rr.label = paste("italic(R)^2", rr.char, sep = "~`=`~"),
             adj.rr.label = paste("italic(R)[adj]^2",
                                  adj.rr.char, sep = "~`=`~"),
             hjust = 0)
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPolyEq <-
  ggplot2::ggproto("StatPolyEq", ggplot2::Stat,
                   compute_group = compute_group_fun,
                   default_aes =
                     ggplot2::aes(label = ..rr.label.., hjust = ..hjust..),
                   required_aes = c("x", "y")
  )

