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
    } else {
      stop("'formula' must refer to aesthetics 'x' and 'y', not names in 'data'")
    }
  }
  list(orientation = orientation,
       formula = formula)
}





