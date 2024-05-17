#' Validate model formula as a polynomial
#'
#' Analyse a model formula to determine if it describes a polynomial with
#' terms in order of increasing powers.
#'
#' @param formula A model formula in \code{x.name}.
#' @param x.name character The name of the explanatory variable in the formula.
#' @param warning.text character string.
#'
#' @details This validation check could fail to validate some valid formulas as
#'   it is difficult to test, or even list all possible variations of valid
#'   formulas. Consequently, this function triggers a warning in case of
#'   failure, not an error. Furthermore, the statistics only fail to build the
#'   correct equation label, but in most cases other output is still usable with
#'   models that are not strictly polynomials.
#'
#'   Model formulas with and without an intercept term are accepted as valid, as
#'   \code{+0}, \code{-1} and \code{+1} are accepted. If a single power term is
#'   included, it is taken as a transformation and any power is accepted. If two
#'   or more terms are powers, they are expected in increasing order with no
#'   missing intermediate terms. If \code{poly()} is used in the model formula,
#'   a single term is expected.
#'
#'   This function checks that all power terms defined using \code{^} are
#'   protected with "as is" \code{I()}, as otherwise they are not powers but
#'   instead part of the formula specification. It also checks that an argument
#'   is passed to parameter \code{raw} of function \code{poly()} if present.
#'
#'   If the warning text is \code{NULL} or \code{character(0)} no warning is
#'   issued. The caller always receives a length-1 logical as return value.
#'
#' @return A logical, TRUE if the formula describes an increasing polynomial,
#'   and FALSE otherwise. As a side-effect a warning is triggered when
#'   validation fails.
#'
#' @export
#'
#' @examples
#' check_poly_formula(y ~ 1)
#' check_poly_formula(y ~ x)
#' check_poly_formula(y ~ x^3)
#' check_poly_formula(y ~ poly(x, 2))
#' check_poly_formula(y ~ x + 0)
#' check_poly_formula(y ~ x - 1)
#' check_poly_formula(y ~ x + 1)
#' check_poly_formula(y ~ x + I(x^2))
#' check_poly_formula(y ~ I(x^2) + x)
#' check_poly_formula(y ~ x + I(x^2) + I(x^3))
#' check_poly_formula(y ~ I(x^2) + I(x^3))
#' check_poly_formula(y ~ x + I(x^3) + I(x^2))
#'
#' check_poly_formula(y ~ poly(x, 2, raw = TRUE)) # use for label
#' check_poly_formula(y ~ poly(x, 2)) # orthogonal polynomial
#'
check_poly_formula <-
  function(formula,
           x.name = "x",
           warning.text = "'formula' is not an increasing polynomial: expect bad/no 'eq.label'!") {
  term.labels <- attr(terms(formula), "term.labels")
  num.terms <- length(term.labels)
  x.terms <- grepl(x.name, term.labels)
  poly.in.terms <- grepl("poly *\\(", as.character(formula)[3L])
  power.terms  <- grepl("\\^ *", term.labels)
  raw.terms  <- grepl("raw *=", term.labels)
  as.is.terms <- grepl("I *\\(", term.labels)

  if (num.terms > 1L && poly.in.terms && sum(power.terms) != 0L) {
    stop("Both 'poly()' and power (^) terms in model formula.")
  }
  if (num.terms > 1L && !all(power.terms == as.is.terms)) {
    warning("Power (^) terms in model formula of a polynomial need to be protected by 'I()'.")
    return(FALSE)
  }
  if (poly.in.terms && !sum(raw.terms)) {
    message("'poly()' in model formula may need to be passed 'raw = TRUE'")
  }
  if (num.terms == 0L || poly.in.terms && num.terms == 1L) {
    polynomial <- TRUE
    increasing <- TRUE
  } else if (sum(power.terms) <= 1L ||
             sum(power.terms) == num.terms - 1L &&
             sum(x.terms) == num.terms) {
    polynomial <- TRUE
    if (sum(x.terms) == 1L || min(which(power.terms)) == 2L) {
      powers <- as.numeric(gsub(".*\\^([0-9]+).*", "\\1", term.labels[power.terms]))
      increasing <- length(powers) <= 1L ||
        !is.unsorted(powers, strictly = TRUE) &&
        max(powers) == length(powers) + 1 # no missing terms
    } else {
      increasing <- FALSE
    }
  } else {
    polynomial = FALSE
  }
  if (!polynomial || !increasing) {
    if (length(warning.text)) {
      warning(warning.text)
    }
    FALSE
  } else {
    TRUE
  }
}
