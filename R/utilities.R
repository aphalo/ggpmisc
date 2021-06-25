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

#' author:
#'
#' @noRd
#'
polynomial2character <- function (x, decreasing = FALSE, digits = 2, nsmall = 2) {
  p <- format(unclass(x), digits = digits, nsmall = nsmall)
  lp <- length(p) - 1
  names(p) <- 0:lp
  p <- p[as.numeric(p) != 0]
  if (length(p) == 0)
    return("0")
  if (decreasing)
    p <- rev(p)
  signs <- ifelse(as.numeric(p) < 0, "- ", "+")
  signs[1] <- if (signs[1] == "- ") "-" else ""
  np <- names(p)
  pow <- paste("x^", np, sep = "")
  pow[np == "0"] <- ""
  pow[np == "1"] <- "x"
  stars <- rep.int("*", length(p))
  stars[p == "" | pow == ""] <- ""
  paste0(signs, p, stars, pow, collapse = " ")
}

# based on idea in answer by slamballais to Stackoverflow question
# at https://stackoverflow.com/questions/67942485/
#
# This is an edit of the code in package 'polynom'
#
as.character.polynomial <- function (x,
                                     decreasing = FALSE,
                                     digits = 3,
                                     keep.zeros = TRUE) {
  if (keep.zeros) {
    p <- sprintf("%.*#g", digits, x)
  } else {
    p <- sprintf("%.*g", digits, x)
  }
  lp <- length(p) - 1
  names(p) <- 0:lp
  p <- p[as.numeric(p) != 0]
  if (length(p) == 0)
    return("0")
  if (decreasing)
    p <- rev(p)
  signs <- ifelse(as.numeric(p) < 0, "- ", "+ ")
  signs[1] <- if (signs[1] == "- ") "-" else ""
  np <- names(p)
  pow <- paste("x^", np, sep = "")
  pow[np == "0"] <- ""
  pow[np == "1"] <- "x"
  stars <- rep.int("*", length(p))
  stars[p == "" | pow == ""] <- ""
  p <- gsub("^-", "", p)
  paste0(signs, p, stars, pow, collapse = " ")
}

