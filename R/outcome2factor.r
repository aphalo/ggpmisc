#' Convert numeric ternary outcomes into a factor
#'
#' @param x a numeric vector of -1, 0, and +1 values, indicating down
#'   regulation, uncertain response or upregulation.
#' @param n.levels numeric Number of levels to create.
#'
#' @details This function converts the numerically encoded values into a factor
#'   with the three levels \code{"down"}, \code{"uncertain"} and \code{"up"}, or
#'   into a factor with two levels \code{de} and \code{uncertain} as expected
#'   by scales \code{scale_colour_outcome()}, \code{scale_fill_outcome()} and
#'   \code{scale_shape_outcome()}.
#'
#' @note When \code{n.levels = 2} both -1 and +1 are merged to the same level
#'   with label \code{"de"}.
#'
#' @export
#'
#' @examples
#'
#' outcome2factor(c(-1, 1, 0, 1))
#' outcome2factor(c(-1, 1, 0, 1), n.levels = 2L)
#'
#' @family scales for omics data
#'
outcome2factor <- function(x, n.levels = 3L) {
  stopifnot(all(unique(stats::na.omit(x)) %in% -1:1))
  if (n.levels == 3L) {
    fct.labels <- c( "up", "uncertain","down")
  } else if (n.levels == 2) {
    fct.labels <- c("de", "uncertain", "de")
  }
  factor(x, levels = c(+1, 0, -1), labels = fct.labels)
}


#' Convert two numeric ternary outcomes into a factor
#'
#' @param x,y numeric vectors of -1, 0, and +1 values, indicating down
#'   regulation, uncertain response or upregulation.
#'
#' @details This function converts the numerically encoded values into a factor
#'   with the four levels \code{"xy"}, \code{"x"}, \code{"y"} and \code{"none"}, or
#'   into a factor with two levels \code{de} and \code{uncertain} as expected
#'   by scales \code{scale_colour_outcome()}, \code{scale_fill_outcome()} and
#'   \code{scale_shape_outcome()}.
#'
#' @note When \code{n.levels = 2} both -1 and +1 are merged to the same level
#'   with label \code{"de"}.
#'
#' @export
#'
#' @examples
#'
#' xy_outcomes2factor(c(-1, 0, 0, 1, -1), c(0, 1, 0, 1, -1))
#'
#' @family scales for omics data
#'
xy_outcomes2factor <- function(x, y) {
  stopifnot(all(unique(stats::na.omit(x)) %in% -1:1) &&
              all(unique(stats::na.omit(y)) %in% -1:1))
  stopifnot(length(x) == length(y))
  factor(
    ifelse(x & y, "xy",
           ifelse(x, "x",
                  ifelse(y, "y", "none"))),
    levels = c("xy", "x", "y", "none")
  )
}
