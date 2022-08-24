## Copied from 'ggrepl'
#'
#' Name ggplot grid object
#' Convenience function to name grid objects
#'
#' @noRd
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

.pt <- 72.27 / 25.4

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

#' Return a boolean vector of non-empty items.
#'
#' @param xs Vector with a mix of "expression" items, "character" items,
#'  and items from other classes.
#' @return Boolean vector indicating which items are not empty.
#' @noRd
not_empty <- function(xs) {
  sapply(seq_along(xs), function(i) {
    if (is.expression(xs[i])) {
      return(length(nchar(xs[i])) > 0)
    } else {
      return(xs[i] != "")
    }
  })
}

#' Return a unit version of the argument.
#'
#' @param x Number or unit object.
#' @return unit(x, "lines") if number or the unchanged argument if it's already
#'  a unit object.
#' @noRd
to_unit <- function(x) {
  # don't change arg if already unit
  if (grid::is.unit(x)) {
    return(x)
  }

  # NA used to exclude points from repulsion calculations
  if (length(x) == 1 && is.na(x)) {
    return(NA)
  }

  grid::unit(x, "lines")
}

#' Parse takes a vector of n lines and returns m expressions.
#' See https://github.com/tidyverse/ggplot2/issues/2864 for discussion.
#'
#' parse(text = c("alpha", "", "gamma"))
#' #> expression(alpha, gamma)
#'
#' parse_safe(text = c("alpha", "", "gamma"))
#' #> expression(alpha, NA, gamma)
#'
#' @noRd
parse_safe <- function(text) {
  stopifnot(is.character(text))
  out <- vector("expression", length(text))
  for (i in seq_along(text)) {
    expr <- parse(text = text[[i]])
    out[[i]] <- if (length(expr) == 0) NA else expr[[1]]
  }
  out
}

#' Safely extract the formula from an object
#'
#' @param fm Fitted model object or a call object.
#' @param method.args List of arguments to check for the formula.
#' @param verbose logical If \code{TRUE} message triggered if call to
#'   \code{formula()} fails.
#'
#' @details Method \code{\link{formula}} is not implemented for all fitted
#'   model objects, while the default method triggers an error and stops
#'   exectution. Function \code{fail_safe_formula()} wraps the call to
#'   \code{formula()} and handles the error conditions by attempting to
#'   extract the formula from a list of arguments. If this fails, it returns
#'   \code{NA}, with a message.
#'
#' @return A named list with objects of class formula or NA as member(s).
#'
#' @keywords internal
#'
fail_safe_formula <- function(fm,
                              method.args = list(),
                              verbose = TRUE) {
  withCallingHandlers({
    withRestarts({
      z <- stats::formula(fm)
      if (!is.list(z)) {
        z <- list(formula = z)
      }
    }, handleError = function(cond) {
      selector <- intersect(names(method.args),
                            c("formula", "fixed", "random", "model"))
      if (length(selector)) {
        if (verbose) message("'formula' extracted from arguments")
        z <- method.args[selector]
      } else {
        if (verbose) message("'formula' not available")
        z <- NA
      }
      if (!is.list(z)) {
        z <- list(formula = z)
      }
    })
  }, error = function(cond) {
    invokeRestart("handleError")
  })
}

