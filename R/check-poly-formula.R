#' Validate model formula as a polynomial
#'
#' Analyse a model formula to determine if it describes a polynomial with
#' terms in order of increasing powers, and fulfils the expectations of the
#' algorithm used to generate the equation-label.
#'
#' @param formula A model formula in \code{x.name}.
#' @param x.name character The name of the explanatory variable in the formula.
#' @param
#'   warn.incr.poly.text,warn.transf.lhs.txt,warn.transf.rhs.txt,warn.spline.rhs.txt,warn.as.is.txt,warn.poly.raw.txt,stop.pow.poly.text
#'   character Text for warnings and errors. If \code{NULL} the warning is not
#'   issued and failure reported silently to the caller.
#' @param check.transf.rhs,check.transf.lhs logical flag enabling test for
#'   transformation of variables.
#'
#' @details The assumption is that this function will be called from within a
#'   'ggplot2' compatible layer function, and that model formulas will always
#'   have a single explanatory variable, variables will be \code{x} and
#'   \code{y}. Its behaviour is undefined or erroneous in other cases. This
#'   validation check cannot be demonstrated to be always correct, as it is
#'   difficult to test, or even list all possible variations of supported vs.
#'   unsupported formulas.
#'
#'   Many valid model formulas that can be successfully fitted, cannot
#'   be automatically converted into correct character labels by functions in
#'   'ggpmisc'. Thus, this function triggers a
#'   warning in case of a test failure, not an error, returning a logic value. If
#'   this value is \code{FALSE}, the statistics in 'ggpmisc' skip the generation
#'   of an equation label, setting it to \code{NA}, while returning other
#'   character labels and the numeric estimates
#'   of the fitted coefficients. Thus, the stats can be used also
#'   with models that are not polynomials or containing transformations, but in
#'   this case the model equation when needed needs to be assembled in user's
#'   code within a call to `aes()`.
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
#'   intermediate power terms.
#'
#'   Spline base functions \code{ns()}, \code{bs()} and \code{lspline()} are
#'   searched for and flagged, while \code{poly()} is accepted. If \code{poly()}
#'   is used in the model formula, a single term is expected. When calling
#'   function \code{poly()}, \code{raw = TRUE} must be passed to obtain suitable
#'   estimates for the fitted coefficients, and this is also checked.
#'
#'   When the formula \emph{rhs} contains more than one power term, all power
#'   terms defined using \code{^} must be protected as \code{"as.is"}
#'   \code{I()}, as otherwise they are not powers but instead part of the
#'   formula specification.
#'
#'   If the warning text is \code{NULL} or \code{character(0)} no warning is
#'   issued, but the test is done. In contrast, \code{check.transf.rhs,check =
#'   FALSE} and \code{transf.lhs = FALSE} skip these two tests on with
#'   \code{raw = TRUE}.
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
#' # spline terms not accepted
#' check_poly_formula(y ~ ns(x))
#' check_poly_formula(y ~ bs(x))
#' check_poly_formula(y ~ lspline(x))
#'
check_poly_formula <-
  function(formula,
           x.name = "x",
           warn.incr.poly.text =
             "'formula' not an increasing polynomial: 'eq.label' set to NA!",
           warn.transf.rhs.txt =
             paste0("'formula' rhs includes transformations requiring an argument for ",
                    "'eq.x.rhs': 'eq.label' set to NA!."),
           warn.transf.lhs.txt =
             paste0("'formula' lhs includes transformations requiring an argument for ",
                    "'eq.with.lhs': 'eq.label' set to NA!."),
           warn.spline.rhs.txt =
             paste0("'formula' rhs includes a spline base function: ",
                    " 'eq.label' set to NA!."),
           warn.as.is.txt =
             paste0("Power (^) terms in 'formula' for a polynomial need to ",
                    "be protected by 'I()': 'eq.label' set to NA!."),
           warn.poly.raw.txt =
             paste0("'poly()' in 'formula' has to be passed 'raw = TRUE': ",
                    "'eq.label' set to NA!"),
           stop.pow.poly.text =
             "Both 'poly()' and power (^) terms in 'formula', use one or the other.",
           check.transf.rhs = TRUE,
           check.transf.lhs = TRUE) {

  transf.ok <- TRUE # flag use below

  # parsing
  lhs <- as.character(formula)[2]
  rhs <- as.character(formula)[3]

  if (grepl("ns\\([xyz].*\\)|bs\\([xyz].*\\)|lspline\\([xyz].*\\)", rhs)) {
    if (length(warn.spline.rhs.txt)) {
      warning(warn.spline.rhs.txt)
    }
    return(FALSE)
  }

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
      any(grepl("log|exp|sqrt|cos|sin|tan|[-^/*+]|[^I[:space:]]\\(.*[xy].*\\)",
                lhs))) {
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
        (length(powers) == 1 ||
           (length(powers) >= 2 && all(diff(powers) == 1L)))
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
