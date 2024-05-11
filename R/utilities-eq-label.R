### Utility functions shared between stat_poly_eq() and stat_quant_eq()
# when stable will be exported
#
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

coefs2poly_eq <- function(coefs,
                          coef.digits = 3L,
                          coef.keep.zeros = TRUE,
                          eq.x.rhs = "x",
                          lhs = "y~`=`~",
                          output.type = "expression",
                          decimal.mark = ".") {
  # build equation as a character string from the coefficient estimates
  stopifnot(coef.digits > 0)
  if (coef.digits < 3) {
    warning("'coef.digits < 3' Likely information loss!")
  }
  eq.char <- as.character(polynom::as.polynomial(coefs),
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

# based on idea in answer by slamballais to Stackoverflow question
# at https://stackoverflow.com/questions/67942485/
#
# This is an edit of the code in package 'polynom' so that trailing zeros are
# retained during the conversion
#' @noRd
#' @noMd
#' @export
#' @method as.character polynomial
#'
as.character.polynomial <- function (x,
                                     decreasing = FALSE,
                                     digits = 3,
                                     keep.zeros = TRUE,
                                     ...) {
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

# exponential number notation to typeset equivalent: Protecting trailing zeros
# in negative numbers is more involved than I would like. Not only we need to
# enclose numbers in quotations marks but we also need to replace dashes with
# the minus character. I am not sure we can do the replacement portably, but
# that recent R supports UTF gives some hope.
#
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
