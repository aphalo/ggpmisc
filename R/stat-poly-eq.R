#' Add a label for a fitted linear model to a plot.
#'
#' \code{stat_poly_eq} fits a polynomial and generates several labela with
#'   an equation and/or coefficient of determination (R^2) and other estimates.
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
#' @param eq.with.lhs If \code{character} the string is pasted to the front
#'   of the equation label before parsing or a \code{logical} (see note).
#' @param eq.x.rhs \code{character} this string will be used as replacement
#'   for \code{"x"} in the model equation when generating the label before
#'   parsing it.
#' @param label.x.npc,label.y.npc \code{numeric} with range 0..1 or character.
#'   Coordinates to be used for positioning the output, expresed in "normalized
#'   parent coordinates" or character string. If too short they will be recycled.
#' @param label.x,label.y \code{numeric} Coordinates (in data units) to be used
#'   for absolute positioning of the output. If too short they will be recycled.
#' @param output.type character One of "expression", "LaTex" or "text".
#'
#' @note For backward compatibility a logical is accepted as argument for
#'   \code{eq.with.lhs}, giving the same output than the current default
#'   character value. By default "x" is retained as independent variable as
#'   this is the name of the aesthetic. However, it can be substituted by
#'   providing a suitable replacement character string through \code{eq.x.rhs}.
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
#'   \item{AIC.label}{AIC for the fitted model.}
#'   \item{BIC.label}{BIC for the fitted model.}
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
                         formula = NULL,
                         eq.with.lhs = "italic(y)~`=`~",
                         eq.x.rhs = NULL,
                         label.x.npc = "left", label.y.npc = "top",
                         label.x = NULL, label.y = NULL,
                         output.type = "expression",
                         position = "identity",
                         na.rm = FALSE, show.legend = FALSE,
                         inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatPolyEq, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(formula = formula,
                  eq.with.lhs = eq.with.lhs,
                  eq.x.rhs = eq.x.rhs,
                  label.x.npc = label.x.npc,
                  label.y.npc = label.y.npc,
                  label.x = label.x,
                  label.y = label.y,
                  output.type = output.type,
                  na.rm = na.rm,
                  ...)
  )
}

# Defined here to avoid a note in check --as-cran as the import from 'polynom'
# is not seen when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
poly_eq_compute_group_fun <- function(data,
                                     scales,
                                     formula,
                                     eq.with.lhs,
                                     eq.x.rhs,
                                     label.x.npc,
                                     label.y.npc,
                                     label.x,
                                     label.y,
                                     output.type) {
  output.type = tolower(output.type)
  if (is.null(eq.x.rhs) && output.type == "expression") {
    eq.x.rhs = "~italic(x)"
  } else {
    eq.x.rhs = " x"
  }
  if (length(unique(data$x)) < 2) {
    # Not enough data to perform fit
    return(data.frame())
  }
  group.idx <- abs(data$group[1])

  if (length(label.x.npc) >= group.idx) {
    label.x.npc <- label.x.npc[group.idx]
  } else {
    label.x.npc <- label.x.npc[1]
  }
  if (length(label.y.npc) >= group.idx) {
    label.y.npc <- label.y.npc[group.idx]
  } else {
    label.y.npc <- label.y.npc[1]
  }

  if (length(label.x) >= group.idx) {
    label.x <- label.x[group.idx]
  } else {
    label.x <- label.x[1]
  }
  if (length(label.y) >= group.idx) {
    label.y <- label.y[group.idx]
  } else {
    label.y <- label.y[1]
  }

  mf <- stats::lm(formula, data)
  coefs <- stats::coef(mf)
  formula.rhs.chr <- as.character(formula)[3]
  if (grepl("-1", formula.rhs.chr) || grepl("- 1", formula.rhs.chr)) {
    coefs <- c(0, coefs)
  }
  rr <- summary(mf)$r.squared
  AIC <- AIC(mf)
  BIC <- BIC(mf)
  adj.rr <- summary(mf)$adj.r.squared
  eq.char <- as.character(signif(polynom::as.polynomial(coefs), 3))
  eq.char <- gsub("e([+-]?[0-9]*)", "%*%10^\\1", eq.char)
  if (output.type %in% c("latex", "tex", "tikz")) {
    eq.char <- gsub("*", " ", eq.char, fixed = TRUE)
  }
  if (is.character(eq.with.lhs)) {
    lhs <- eq.with.lhs
    eq.with.lhs <- TRUE
  } else if (eq.with.lhs) {
    if (output.type == "expression") {
      lhs <- "italic(y)~`=`~"
     } else if (output.type %in% c("latex", "tex", "tikz", "text")) {
       lhs <- "y = "
     }
  }
  if (eq.with.lhs) {
    eq.char <- paste(lhs, eq.char, sep = "")
  }
  print(eq.char)
  rr.char <- format(rr, digits = 2)
  adj.rr.char <- format(adj.rr, digits = 2)
  AIC.char <- sprintf("%.4g", AIC)
  BIC.char <- sprintf("%.4g", BIC)
  if (output.type == "expression") {
    z <- data.frame(eq.label = gsub("x", eq.x.rhs, eq.char, fixed = TRUE),
                    rr.label = paste("italic(R)^2", rr.char, sep = "~`=`~"),
                    adj.rr.label = paste("italic(R)[adj]^2",
                                         adj.rr.char, sep = "~`=`~"),
                    AIC.label = paste("AIC", AIC.char, sep = "~`=`~"),
                    BIC.label = paste("BIC", BIC.char, sep = "~`=`~"))
  } else if (output.type %in% c("latex", "tex", "text")) {
    z <- data.frame(eq.label = gsub("x", eq.x.rhs, eq.char, fixed = TRUE),
                    rr.label = paste("R^2", rr.char, sep = " = "),
                    adj.rr.label = paste("R_{adj}^2",
                                         adj.rr.char, sep = " = "),
                    AIC.label = paste("AIC", AIC.char, sep = " = "),
                    BIC.label = paste("BIC", BIC.char, sep = " = "))
  }

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
    z$x <- label.y
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
StatPolyEq <-
  ggplot2::ggproto("StatPolyEq", ggplot2::Stat,
                   compute_group = poly_eq_compute_group_fun,
                   default_aes =
                     ggplot2::aes(label = ..rr.label..,
                                  hjust = ..hjust.., vjust = ..vjust..),
                   required_aes = c("x", "y")
  )

