#' Validate model formula as a polynomial
#'
#' Analyse a model formula to determine if it describes a polynomial with
#' terms in order of increasing powers, and fulfils the expectations of the
#' algorithm used to generate the equation-label.
#'
#' @param formula A model formula in \code{x.name}.
#' @param x.name character The name of the explanatory variable in the formula.
#' @param
#'   warn.incr.poly.text,warn.transf.lhs.txt,warn.transf.rhs.txt,warn.as.is.txt,warn.poly.raw.txt,stop.pow.poly.text
#'   character Text for warnings and errors.
#' @param check.transf.rhs,check.transf.lhs logical flag enabling test for
#'   transformation of variables.
#'
#' @details The assumption is that this function will be called from within a
#'   ggplot2 compatible layer function, and that model formulas will always have
#'   a single explanatory variable, variables will be \code{x} and \code{y}. Its
#'   behaviour is undefined or erroneous in other cases.
#'
#'   This validation check could return a false positive or a false negative
#'   results with some formulas as it is difficult to test, or even list all
#'   possible variations of supported vs. unsupported formulas. This makes
#'   testing difficult. In addition, many valid model formulas that can be
#'   succesfully fitted, are not correctly converted into character labels.
#'   Thus, this function triggers a warning in case of failure, not an error,
#'   and returns a logic value. If this value is \code{FALSE}, the statistics in
#'   'ggpmisc' skip the generation of an equation label, setting it to
#'   \code{NA}. However, if the formula is accepted by the model fit function,
#'   other labels and the numeric estimates of the fitted coefficients remain
#'   usable. The stats can be used also with models that are not polynomials or
#'   containing transformations.
#'
#'   Model formulas with and without an intercept term are accepted as valid, as
#'   \code{+0}, \code{-1} and \code{+1} are accepted. If a single \code{as.is}
#'   power term is included or if arithmetic (\code{sqrt()}, \code{exp()},
#'   \code{log()}), or trigonometric functions (\code{cos()}, \code{sin()},
#'   \code{tan()}, etc.) are encountered a warning is issued about the need to
#'   pass a matching argument to parameter \code{eq.x.rhs} of the statistic.
#'
#'   If two or more terms are \code{as.is} (\code{I( )} protected) powers
#'   (\code{^}), they are expected to be in increasing order with no missing
#'   intermediate power terms. If \code{poly()} is used in the model formula, a
#'   single term is expected. When calling function \code{poly()},
#'   \code{raw = TRUE} must be passed to obtain suitable estimates for the
#'   fitted coefficients, and this is also checked.
#'
#'   When the formula \emph{rhs} contains more than one power term, all power
#'   terms defined using \code{^} must be protected as \code{"as.is"}
#'   \code{I()}, as otherwise they are not powers but instead part of the
#'   formula specification.
#'
#'   If the warning text is \code{NULL} or \code{character(0)} no warning is
#'   issued, but the test is done. In contrast, \code{check.transf.rhs,check =
#'   FALSE} and \code{transf.lhs = FALSE} skip these two tests. The caller
#'   always receives a length-1 logical as returned value.
#'
#' @return A logical, TRUE if the formula describes an increasing polynomial
#'   suitable for conversion into a text label, and FALSE otherwise. When
#'   validation fails, warnings are issued describing the problem encountered.
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
           warn.incr.poly.text = "'formula' not an increasing polynomial: 'eq.label' set to NA!",
           warn.transf.rhs.txt = "rhs includes transformations requiring an argument for 'eq.x.rhs': 'eq.label' set to NA!.",
           warn.transf.lhs.txt = "lhs includes transformations requiring an argument for 'eq.with.lhs': 'eq.label' set to NA!.",
           warn.as.is.txt = "Power (^) terms in model formula of a polynomial need to be protected by 'I()': 'eq.label' set to NA!.",
           warn.poly.raw.txt = "'poly()' in model formula has to be passed 'raw = TRUE': 'eq.label' set to NA!",
           stop.pow.poly.text = "Both 'poly()' and power (^) terms in model formula.",
           check.transf.rhs = TRUE,
           check.transf.lhs = TRUE) {

  transf.ok <- TRUE # flag use below

  # parsing
  lhs <- as.character(formula)[2]
  rhs <- as.character(formula)[3]

  formula.terms <- terms(formula)
  rhs.terms <- attr(formula.terms, "term.labels")
  # replaced as it fails when + or * appear within a function call such as I()
  # rhs.terms <- unlist(strsplit(x = rhs, split = c("+", "*"), fixed = TRUE))
  num.terms <- length(rhs.terms)
  x.terms <- grepl(x.name, rhs.terms)
  poly.in.terms <- grepl("poly *\\(", as.character(formula)[3L])
  power.terms  <- grepl("\\^ *", rhs.terms)
  if (!any(power.terms) && grepl("\\^ *", rhs)) {
    warning("Terms in 'formula' were dropped! Forgotten 'I()'?")
    # even if accepted downstream this indicates an error
    return(FALSE)
  }
  raw.terms  <- grepl("raw *=", rhs.terms)
  as.is.terms <- grepl("I *\\(", rhs.terms)

  # checks
  if (num.terms > 1L && poly.in.terms && sum(power.terms) != 0L) {
    stop(stop.pow.poly.text)
  }
  if (num.terms > 1L && !all(which(power.terms) %in% which(as.is.terms))) {
    if (length(warn.as.is.txt)) {
      warning(warn.as.is.txt)
    }
    return(FALSE)
  }
  if (check.transf.rhs &&
      (num.terms == 1L && any(as.is.terms) ||
      any(grepl("log|exp|sqrt|cos|sin|tan", rhs)))) {
    if (length(warn.transf.rhs.txt)) {
      warning(warn.transf.rhs.txt)
    }
    transf.ok <- FALSE
  }
  if (check.transf.lhs &&
      any(grepl("log|exp|sqrt|cos|sin|tan|[-^/*+]|[^I[:space:]]\\(.*[xy].*\\)", lhs))) {
    if (length(warn.transf.lhs.txt)) {
      warning(warn.transf.lhs.txt)
    }
    transf.ok <- FALSE
  }
  if (!transf.ok) {
    # flag ensures that both lhs and rhs are always checked and warnings issued
    return(FALSE)
  }
  if (length(warn.poly.raw.txt) &&
      poly.in.terms && !sum(raw.terms)) {
    warning(warn.poly.raw.txt)
    return(FALSE)
  }
  if (sum(x.terms) <= 1L || poly.in.terms && num.terms == 1L) {
    polynomial <- TRUE
    increasing <- TRUE
  } else if (sum(power.terms) < 1L ||
             sum(power.terms) == 1L && num.terms == 1L ||
             sum(x.terms) == num.terms ||
             sum(power.terms) == num.terms - 2L &&
             sum(x.terms) == num.terms - 1L) {
    polynomial <- TRUE
    if (sum(x.terms) >= 1L || any(power.terms)) {
      powers <- numeric(length(x.terms)) # filled with zeros
      if (sum(power.terms)) {
        hi.powers.chr <- character(length(x.terms))
        hi.powers.chr[power.terms] <-
          gsub(".*\\^([0-9]+).*", "\\1", rhs.terms[power.terms])
        powers[power.terms] <- as.numeric(hi.powers.chr[power.terms])
      }
      if (!any(powers == 1, na.rm = TRUE)) {
        power1.terms <- x.terms & !power.terms
        powers[power1.terms] <- 1
      }
      increasing <- min(powers) == 1 &&
        (length(powers) == 1 || (length(powers) >= 2 && all(diff(powers) == 1L)))
    } else {
      increasing <- FALSE
    }
  } else {
    polynomial <- FALSE
  }
  if (!polynomial || !increasing) {
    if (length(warn.incr.poly.text)) {
      warning(warn.incr.poly.text)
    }
    FALSE
  } else {
    TRUE
  }
}
