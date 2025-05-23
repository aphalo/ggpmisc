#' Tidy, glance or augment an object keeping a trace of its origin
#'
#' Methods implemented in package 'broom' to tidy, glance and augment the output
#' from model fits return a consistently organized tibble with generic column
#' names. Although this simplifies later steps in the data analysis and
#' reporting, it drops key information needed for interpretation.
#' \code{keep_tidy()} makes it possible to retain fields from the model fit
#' object passed as argument to parameter \code{x} in the attribute \code{"fm"}.
#' The class of \code{x} is always stored, and by default also fields
#' \code{"call"}, \code{"terms"}, \code{"formula"}, \code{"fixed"} and
#' \code{"random"} if available.
#'
#' @param x An object for which \code{tidy()}, \code{glance} and/or
#'   \code{augment} method is available.
#' @param ... Other named arguments passed along to \code{tidy()}, \code{glance}
#'   or \code{augment}.
#' @param to.keep character vector of field names in \code{x} to copy to
#'   attribute \code{"fm"} of the tibble returned by \code{tidy()},
#'   \code{glance} or \code{augment}.
#'
#' @details Functions \code{keep_tidy()}, \code{keep_glance} or
#'   \code{keep_augment} are simple wrappers of the generic methods which make
#'   it possible to add to the returned values an attribute named \code{"fm"}
#'   preserving user selected fields and class of the model fit object. Fields
#'   names in \code{to.keep} missing in \code{x} are silently ignored.
#'
#' @export
#'
#' @examples
#'
#' # these examples can only be run if package 'broom' is available
#'
#' if (requireNamespace("broom", quietly = TRUE)) {
#'
#'   library(broom)
#'
#'   mod <- lm(mpg ~ wt + qsec, data = mtcars)
#'
#'   attr(keep_tidy(mod), "fm")[["class"]]
#'   attr(keep_glance(mod), "fm")[["class"]]
#'   attr(keep_augment(mod), "fm")[["class"]]
#'
#'   attr(keep_tidy(summary(mod)), "fm")[["class"]]
#'
#'   library(MASS)
#'   rmod <- rlm(mpg ~ wt + qsec, data = mtcars)
#'   attr(keep_tidy(rmod), "fm")[["class"]]
#'
#' }
#'
keep_tidy <-
  function(x,
           ...,
           to.keep = c("call", "terms", "formula", "fixed", "random")) {
    z <- generics::tidy(x, ...)
    # avoid error if fields are missing to allow a default suitable for
    # different model fit functions
    keep <- intersect(names(x), to.keep)
    attr(z, "fm") <- c(x[keep], list(class = class(x)))
    z
  }

#' @rdname keep_tidy
#'
#' @export
#'
keep_glance <-
  function(x,
           ...,
           to.keep = c("call", "terms", "formula", "fixed", "random")) {
    z <- generics::glance(x, ...)
    # avoid error if fields are missing to allow a default suitable for
    # different model fit functions
    keep <- intersect(names(x), to.keep)
    attr(z, "fm") <- c(x[keep], list(class = class(x)))
    z
  }

#' @rdname keep_tidy
#'
#' @export
#'
keep_augment <-
  function(x,
           ...,
           to.keep = c("call", "terms", "formula", "fixed", "random")) {
    z <- generics::augment(x, ...)
    # avoid error if fields are missing to allow a default suitable for
    # different model fit functions
    keep <- intersect(names(x), to.keep)
    attr(z, "fm") <- c(x[keep], list(class = class(x)))
    z
  }
