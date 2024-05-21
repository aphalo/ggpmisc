#' Left and right hand sides of model equations
#'
#' @param output.type character One of "expression", "latex", "tex", "text",
#'   "tikz", "markdown".
#' @param orientation character \code{"x"} or \code{"y"}, indicating
#'   the aesthetic onto which the explanatory variable is mapped.
#'
#' @return A \code{character} string.
#'
#' @keywords internal
#'
build_eq.x.rhs <- function(output.type = "expression",
                           orientation = "x") {
  if (orientation == "x") {
    if (output.type == "expression") {
      "~italic(x)"
    } else if (output.type == "markdown") {
      "_x_"
    } else{
      " x"
    }
  } else if (orientation == "y") {
    if (output.type == "expression") {
      "~italic(y)"
    } else if (output.type == "markdown") {
      "_y_"
    } else{
      " y"
    }
  }
}

#' @rdname build_eq.x.rhs
#'
#' @return A \code{character} string.
#'
build_lhs <- function(output.type = "expression",
                      orientation = "x") {
  if (orientation == "x") {
    if (output.type == "expression") {
      "italic(y)~`=`~"
    } else if (output.type %in% c("latex", "tex", "tikz", "text")) {
      "y = "
    } else if (output.type == "markdown") {
      "_y_ = "
    }
  } else if (orientation == "y") {
    if (output.type == "expression") {
      "italic(x)~`=`~"
    } else if (output.type %in% c("latex", "tex", "tikz", "text")) {
      "x = "
    } else if (output.type == "markdown") {
      "_x_ = "
    }
  }
}

#' Convert a polynomial into character string
#'
#' Differs from \code{polynom::as.character.polynomial()} in that trailing zeros
#' are preserved.
#'
#' @note This is an edit of the code in package 'polynom' so that trailing zeros are
#'  retained during the conversion. It is not defined using a different name
#'  so as not to interfere with the original.
#' @param x a \code{polynomial} object.
#' @param decreasing logical It specifies the order of the terms; in increasing
#'   (default) or decreasing powers.
#' @param digits integer Giving the number of significant digits to use for
#'   printing.
#' @param keep.zeros logical It indicates if zeros are to be retained in the
#'   formatted coefficients.
#'
#' @return A \code{character} string.
#'
#' @examples
#' poly2character(1:3)
#' poly2character(1:3, decreasing = TRUE)
#'
#' @export
#'
poly2character <- function (x,
                            decreasing = getOption("ggpmisc.decreasing.poly.eq", FALSE),
                            digits = 3,
                            keep.zeros = TRUE) {
  if (keep.zeros) {
    p <- sprintf("%#.*g", digits, x)
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


#' Typeset/format numbers preserving trailing zeros
#'
#' @param eq.char character A polynomial model equation as a character string.
#' @param output.type character One of "expression", "latex", "tex", "text",
#'   "tikz", "markdown".
#'
#' @note exponential number notation to typeset equivalent: Protecting trailing
#'   zeros in negative numbers is more involved than I would like. Not only we
#'   need to enclose numbers in quotations marks but we also need to replace
#'   dashes with the minus character. I am not sure we can do the replacement
#'   portably, but that recent R supports UTF gives some hope.
#'
#' @return A \code{character} string.
#'
typeset_numbers <- function(eq.char, output.type) {
  if (output.type == "markdown") {
    eq.char <- gsub("e([+-]?)[0]([1-9]*)", "&times;10<sup>\\1\\2</sup>", eq.char)
    eq.char <- gsub("[:^]([0-9]*)", "<sup>\\1</sup>", eq.char)
    eq.char <- gsub("*", "&nbsp;", eq.char, fixed = TRUE)
    eq.char <- gsub(" ", "", eq.char, fixed = TRUE)
  } else {
    eq.char <- gsub("e([+-]?[0-9]*)", "%*% 10^{\\1}", eq.char)
    # muliplication symbol
    if (output.type %in% c("latex", "tikz")) {
      eq.char <- gsub("%*%", "\\times{}", eq.char, fixed = TRUE)
      eq.char <- gsub("*", "", eq.char, fixed = TRUE)
    } else if (output.type == "text") {
      eq.char <- gsub("[{]|[}]", "", eq.char, fixed = FALSE)
      eq.char <- gsub("%*%", "", eq.char, fixed = TRUE)
      eq.char <- gsub("*", " ", eq.char, fixed = TRUE)
      eq.char <- gsub("  ", " ", eq.char, fixed = TRUE)
    }
  }
  eq.char
}

#' Format a polynomial as an equation
#'
#' Uses a vector of coefficients from a model fit of a polynomial to build
#' the fitted model equation with embedded coefficient estimates.
#'
#' @param coefs numeric Terms always sorted by increasing powers.
#' @param coef.digits integer
#' @param coef.keep.zeros logical This flag refers to trailing zeros.
#' @param decreasing logical It specifies the order of the terms in the
#'   returned character string; in increasing (default) or decreasing powers.
#' @param eq.x.rhs character
#' @param lhs character
#' @param output.type character One of "expression", "latex", "tex", "text",
#'   "tikz", "markdown".
#' @param decimal.mark character
#'
#' @note Terms with zero-valued coefficients are dropped from the polynomial.
#'
#' @return A \code{character} string.
#'
#' @examples
#' coefs2poly_eq(c(1, 2, 0, 4, 5, 2e-5))
#' coefs2poly_eq(c(1, 2, 0, 4, 5, 2e-5), output.type = "latex")
#' coefs2poly_eq(0:2)
#' coefs2poly_eq(0:2, decreasing = TRUE)
#' coefs2poly_eq(c(1, 2, 0, 4, 5), coef.keep.zeros = TRUE)
#' coefs2poly_eq(c(1, 2, 0, 4, 5), coef.keep.zeros = FALSE)
#'
#' @export
#'
coefs2poly_eq <- function(coefs,
                          coef.digits = 3L,
                          coef.keep.zeros = TRUE,
                          decreasing = getOption("ggpmisc.decreasing.poly.eq", FALSE),
                          eq.x.rhs = "x",
                          lhs = "y~`=`~",
                          output.type = "expression",
                          decimal.mark = ".") {
  # build equation as a character string from the coefficient estimates
  stopifnot(coef.digits > 0)
  if (coef.digits < 3) {
    warning("'coef.digits < 3' Likely information loss!")
  }
  eq.char <- poly2character(polynom::as.polynomial(coefs),
                            decreasing = decreasing,
                            digits = coef.digits,
                            keep.zeros = coef.keep.zeros)
  eq.char <- typeset_numbers(eq.char, output.type)
  if (output.type != "expression") { # parse() does the conversion
    if (decimal.mark == ".") {
      eq.char <- gsub(",", decimal.mark, eq.char, fixed = TRUE)
    } else {
      eq.char <- gsub(".", decimal.mark, eq.char, fixed = TRUE)
    }
  }

  if (eq.x.rhs != "x") {
    eq.char <- gsub("x", eq.x.rhs, eq.char, fixed = TRUE)
  }
  if (length(lhs)) {
    eq.char <- paste(lhs, eq.char, sep = "")
  }

  eq.char
}

