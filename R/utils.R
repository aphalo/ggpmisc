#' Swap x and y in a formula
#'
#' By default a formula of x on y is converted into a formula of y
#' on x, while the reverse swap is done only if \code{backward = TRUE}.
#'
#' @param f formula An R model formula
#' @param backwards logical
#'
#' @details
#' This function is meant to be used only as a helper within 'ggplot2'
#' statistics. Normally together with geometries supporting orientation when
#' we want to automate the change in orientation based on a user-supplied
#' formula. Only \code{x} and \code{y} are changed, and in other respects
#' the formula is rebuilt copying the environment from \code{f}.
#'
#' @return A copy of \code{f} with \code{x} and \code{y} swapped by each other
#'   in the lhs and rhs.
#'
swap_xy <- function(f, backwards = FALSE) {
  f.chr <- as.character(f)
  if (backwards) {
    # lhs
    f.chr[2] <- gsub("\\by\\b", "x", f.chr[2])
    # rhs
    f.chr[-c(1, 2)] <- gsub("\\bx\\b", "y", f.chr[-c(1, 2)])
  } else {
    # lhs
    f.chr[2] <- gsub("\\bx\\b", "y", f.chr[2])
    # rhs
    f.chr[-c(1, 2)] <- gsub("\\by\\b", "x", f.chr[-c(1, 2)])
  }
  # reassemble
  f.chr <- paste(f.chr[2], f.chr[3], sep = f.chr[1])
  # define new formula in the same environment as original
  stats::as.formula(f.chr, env = environment(f))
}

#' Guess the orientation from model formula
#'
#' @param orientation character \code{"x"} or \code{"y"}.
#' @param formula model formula based on x and y.
#' @param formula.on.x logical Flip x and y in formula, used when the x
#'   and y in data are not flipped in the compute function.
#'
#' @return Named list with members formula and orientation.
#'
#' @details Set defaults for both \code{orientation} and \code{formula},
#'   ensuring consistency both when x and y and swapped in data and when they
#'   are not.
#'
#' @keywords internal
#'
guess_orientation <- function(orientation = NULL,
                              formula = NULL,
                              default.formula = y ~ x,
                              formula.on.x = FALSE) {
  if (is.null(formula) && is.null(default.formula)) {
    stop("'formula' missing with no default")
  }
  # as.character(formula)[2] is the lhs of the formula
  if (formula.on.x) {
    # if we flip x and y in data, the formula should be with x as explanatory
    if (is.null(formula)) {
      formula = default.formula
      if (is.na(orientation)) {
        orientation = "x"
      }
    } else {
       if (is.null(orientation) || is.na(orientation)) {
        # we guess orientation from formula
        if (grepl("y", as.character(formula)[2])) {
          orientation <- "x"
        } else if (grepl("x", as.character(formula)[2])) {
          orientation <- "y"
        }
      }
      if (!grepl("y", as.character(formula)[2])){
        formula <- swap_xy(formula)
      }
    }
  } else {
    # if we do not flip x and y in data, the formula should match orientation
    # for explanatory.
    # we guess formula from orientation
    if (is.null(formula)) {
      if (is.null(orientation) || is.na(orientation) || orientation == "x") {
        formula <- default.formula
      } else if (orientation == "y") {
        formula <- swap_xy(default.formula)
      }
    }
    # we guess orientation from formula
    if (is.null(orientation) || is.na(orientation)) {
      # we look for a term CONTAINING x or y
      if (grepl("x", as.character(formula)[2])) {
        orientation <- "y"
      } else if (grepl("y", as.character(formula)[2]))
        orientation <- "x"
    } else if (!grepl("x|y", as.character(formula)[2])) {
      stop("'formula' must refer to aesthetics 'x' and 'y', not names in 'data'")
    }
  }
  list(orientation = orientation,
       formula = formula)
}





