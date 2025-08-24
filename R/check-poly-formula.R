#' Validate model formula as a polynomial
#'
#' Analyse a model formula to determine if it describes a polynomial with
#' terms in order of increasing powers, and fulfils the expectations of the
#' algorithm used to generate the equation-label.
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
#' # polynomials
#' check_poly_formula(y ~ 1)
#' check_poly_formula(y ~ x)
#' check_poly_formula(y ~ x^3)
#' check_poly_formula(y ~ x + 0)
#' check_poly_formula(y ~ x - 1)
#' check_poly_formula(y ~ x + 1)
#' check_poly_formula(y ~ x + I(x^2))
#' check_poly_formula(y ~ 1 + x + I(x^2))
#' check_poly_formula(y ~ x + I(x^2) + I(x^3))
#' check_poly_formula(y ~ I(x) + I(x^2) + I(x^3))
#'
#' # transformations on x, first degree polynomials
#' check_poly_formula(y ~ sqrt(x))
#' check_poly_formula(y ~ log(x))
#' check_poly_formula(y ~ I(x^2))
#'
#' # incomplete or terms in decreasing/mixed order
#' check_poly_formula(y ~ I(x^2) + x)
#' check_poly_formula(y ~ I(x^2) + I(x^3))
#' check_poly_formula(y ~ I(x^2) + I(x^4))
#' check_poly_formula(y ~ x + I(x^3) + I(x^2))
#'
#' # polynomials using poly()
#' check_poly_formula(y ~ poly(x, 2, raw = TRUE)) # label o.k.
#' check_poly_formula(y ~ poly(x, 2)) # orthogonal polynomial -> bad label
#'
check_poly_formula <-
  function(formula,
           x.name = "x",
           warning.text = "'formula' not an increasing polynomial: 'eq.label' is NA!") {
  rhs <- as.character(formula)[3]
  rhs.terms <- unlist(strsplit(x = rhs, split = c("+", "*"), fixed = TRUE))
  num.terms <- length(rhs.terms)
  x.terms <- grepl(x.name, rhs.terms)
  poly.in.terms <- grepl("poly *\\(", as.character(formula)[3L])
  power.terms  <- grepl("\\^ *", rhs.terms)
  raw.terms  <- grepl("raw *=", rhs.terms)
  as.is.terms <- grepl("I *\\(", rhs.terms)

  if (num.terms > 1L && poly.in.terms && sum(power.terms) != 0L) {
    stop("Both 'poly()' and power (^) terms in model formula.")
  }
  if (num.terms > 1L && !all(which(power.terms) %in% which(as.is.terms))) {
    warning("Power (^) terms in model formula of a polynomial need to be protected by 'I()'.")
    return(FALSE)
  }
  if (poly.in.terms && !sum(raw.terms)) {
    warning("'poly()' in model formula has to be passed 'raw = TRUE'")
  }
  if (sum(x.terms) == 0L || poly.in.terms && num.terms == 1L) {
    polynomial <- TRUE
    increasing <- TRUE
  } else if (sum(power.terms) < 1L ||
             sum(power.terms) == 1L && num.terms == 1L ||
             sum(x.terms) == num.terms ||
             sum(power.terms) == num.terms - 2L &&
             sum(x.terms) == num.terms - 1L) {
    polynomial <- TRUE
    if (sum(x.terms) == 1L || min(which(power.terms)) %in% 2L:3L) {
      powers <- as.numeric(gsub(".*\\^([0-9]+).*", "\\1", rhs.terms[power.terms]))
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

