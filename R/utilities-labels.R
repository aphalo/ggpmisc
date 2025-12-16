#' Format numeric values as strings
#'
#' Using \code{\link{sprintf}} flexibly format numbers as character strings
#' encoded for parsing into R expressions or using LaTeX or markdown
#' notation.
#'
#' @param fmt character as in \code{sprintf()}.
#' @param ... as in \code{sprintf()}.
#' @param decimal.mark character If \code{NULL} or \code{NA} no substitution is
#'   attempted and the value returned by \code{sprintf()} is returned as is.
#'
#' @details These functions are used to format the character strings returned,
#'   which can be used as labels in plots. Encoding used for the formatting is
#'   selected by the argument passed to \code{output.type}, thus, supporting
#'   different R graphic devices.
#'
#' @seealso \code{\link[base]{sprintf}}
#'
#' @examples
#'
#' sprintf_dm("%2.3f", 2.34)
#' sprintf_dm("%2.3f", 2.34, decimal.mark = ",")
#'
#' @export
#'
sprintf_dm <- function(fmt,
                       ...,
                       decimal.mark = getOption("OutDec", default = ".")) {
  if (is.null(decimal.mark) || is.na(decimal.mark)) {
    sprintf(fmt, ...)
  } else if (decimal.mark != ".") {
    gsub(".", decimal.mark, sprintf(fmt, ...), fixed = TRUE)
  } else {
    # in case OS locale uses ","
    gsub(",", ".", sprintf(fmt, ...), fixed = TRUE)
  }
}

#' @rdname sprintf_dm
#'
#' @param value numeric The value of the estimate.
#' @param digits integer Number of digits to which numeric values are formatted.
#' @param format character One of "e", "f" or "g" for exponential, fixed, or
#' significant digits formatting.
#' @param output.type character One of "expression", "latex", "tex", "text",
#'   "tikz", "markdown".
#'
#' @examples
#'
#' value2char(2.34)
#' value2char(2.34, digits = 3, format = "g")
#' value2char(2.34, digits = 3, format = "f")
#' value2char(2.34, output.type = "text")
#' value2char(2.34, output.type = "text", format = "f")
#' value2char(2.34, output.type = "text", format = "g")
#'
#' @export
#'
value2char <- function(value,
                       digits = Inf,
                       format = "g",
                       output.type = "expression",
                       decimal.mark = getOption("OutDec", default = ".")) {

  stopifnot("Bad 'format' argument" = format %in% c("f", "g", "e"))

  format <- paste("%#.*", format, sep = "")
  protected.format <- paste("\"", format, "\"", sep = "")

  if (output.type == "expression") {

    if (digits == Inf) {
      temp.char <- sprintf_dm(protected.format,
                              2L, value, decimal.mark = decimal.mark)
    } else {
      temp.char <- sprintf_dm(protected.format,
                              digits, value, decimal.mark = decimal.mark)
    }
    if (grepl("e", temp.char)) {
      # dash -> minus
      paste(gsub("e", "\" %*% 10^{\"", gsub("-", "\u2212", temp.char)), "}", sep = "")
    } else {
      temp.char
    }
  } else {
    if (digits == Inf) {
      temp.char <- sprintf_dm(format, 2L, value, decimal.mark = decimal.mark)
    } else {
      temp.char <- sprintf_dm(format,
                              digits, value, decimal.mark = decimal.mark)
    }
    if (grepl("^latex", output.type) && grepl("e", temp.char)) {
      paste(gsub("e", " \\\\times 10^{", temp.char), "}", sep = "")
    } else {
      temp.char
    }
  }
}

#' Format numbers as character labels
#'
#' These functions format numeric values as character labels including the
#' symbol for statistical parameter estimates suitable for adding to plots. The
#' labels can be formatted as strings to be parsed as plotmath expressions,
#' or encoded using LaTeX or Markdown.
#'
#' @param value numeric vector The value of the estimate(s), accepted vector
#'   length depends on the function.
#' @param value.name character The symbol used to represent the value, or its
#'   name.
#' @param df,df1,df2 numeric The degrees of freedom of the estimate.
#' @param small.p,small.r logical If \code{TRUE} use lower case (\eqn{p} and
#'   \eqn{r},  \eqn{r^2}) instead of upper case (\eqn{P} and
#'   \eqn{R}, \eqn{R^2}),
#' @param digits integer Number of digits to which numeric values are formatetd.
#' @param fixed logical Interpret \code{digits} as indicating a number of
#'   digits after the decimal mark or as the number of significant digits.
#' @param output.type character One of "expression", "latex", "tex", "text",
#'   "tikz", "markdown".
#' @param decimal.mark character Defaults to the value of R option
#'   \code{"OutDec"}.
#'
#' @return A character string with formatting, encoded to be parsed as an R
#'   plotmath expression, as plain text, as markdown or to be used with
#'   \eqn{LaTeX} within \strong{math mode}.
#'
#' @seealso \code{\link{sprintf_dm}}
#'
#' @export
#'
#' @examples
#' plain_label(value = 123, value.name = "n", output.type = "expression")
#' plain_label(value = 123, value.name = "n", output.type = "markdown")
#' plain_label(value = 123, value.name = "n", output.type = "latex")
#' italic_label(value = 123, value.name = "n", output.type = "expression")
#' italic_label(value = 123, value.name = "n", output.type = "markdown")
#' italic_label(value = 123, value.name = "n", output.type = "latex")
#' bold_label(value = 123, value.name = "n", output.type = "expression")
#' bold_label(value = 123, value.name = "n", output.type = "markdown")
#' bold_label(value = 123, value.name = "n", output.type = "latex")
#'
#' plain_label(value = NA, value.name = "n", output.type = "expression")
#' plain_label(value = c(123, NA), value.name = "n", output.type = "latex")
#'
#' plain_label(value = c(123, 1.2), value.name = "n", output.type = "expression")
#' plain_label(value = c(123, 1.2), value.name = "n", output.type = "markdown")
#' plain_label(value = c(123, 1.2), value.name = "n", output.type = "latex")

plain_label <- function(value,
                        value.name,
                        digits = 3,
                        fixed = FALSE,
                        output.type = "expression",
                        decimal.mark = getOption("OutDec", default = ".")) {

  if (length(value) == 0 || all(is.na(value))) {
    return(NA_character_)
  }

  stopifnot("Negative value of 'digits'" = digits >= 0)

  if (is.integer(value)) {
    value.char <- as.character(value)
  } else {
    value.char <- sapply(value,
                         value2char,
                         digits = digits,
                         output.type = output.type,
                         decimal.mark = decimal.mark,
                         format = ifelse(fixed, "f", "g")
                     )
  }

  if (output.type == "expression") {
    z <- paste("plain(", value.name, ")~`=`~", value.char, sep = "")
    if (length(z) > 1L) {
      paste(z, collapse = "*\"; \"*")
    } else {
      z
    }
  } else if (grepl("^latex", output.type)) {
    z <- paste("\\mathrm{", value.name, "} = ", value.char, sep = "")
    if (length(z) > 1L) {
      z <- paste(z, collapse = "\\mathrm{; }")
    }
    if (output.type == "latex.eqn") {
      paste("$", z, "$")
    } else if (output.type == "latex.deqn") {
      paste("$$", z, "$$")
    } else {
      z
    }
  } else if (output.type %in% c("text", "markdown")) {
    z <- paste(value.name, " = ", value.char, sep = "")
    if (length(z) > 1L) {
      paste(z, collapse = "; ")
    } else {
      z
    }
  }
}

#' @rdname plain_label
#'
#' @export
#'
italic_label <- function(value,
                         value.name,
                         digits = 3,
                         fixed = FALSE,
                         output.type = "expression",
                         decimal.mark = getOption("OutDec", default = ".")) {

  if (length(value) == 0 || all(is.na(value))) {
    return(NA_character_)
  }

  stopifnot("Negative value of 'digits'" = digits >= 0)

  if (is.integer(value)) {
    value.char <- as.character(value)
  } else {
    if (is.integer(value)) {
      value.char <- as.character(value)
    } else {
      value.char <- sapply(value,
                           value2char,
                           digits = digits,
                           output.type = output.type,
                           decimal.mark = decimal.mark,
                           format = ifelse(fixed, "f", "g")
      )
    }
  }

  if (output.type == "expression") {
    z <- paste("italic(", value.name, ")~`=`~", value.char, sep = "")
    if (length(z) > 1L) {
      paste(z, collapse = "*\"; \"*")
    } else {
      z
    }
  } else if (grepl("^latex", output.type)) {
    z <- paste(value.name, " = ", value.char, sep = "")
    if (length(z) > 1L) {
      z <- paste(z, collapse = "; ")
    }
    if (output.type == "latex.eqn") {
      paste("$", z, "$")
    } else if (output.type == "latex.deqn") {
      paste("$$", z, "$$")
    } else {
      z
    }
  } else if (output.type == "markdown") {
    z <- paste("_", value.name, "_ = ", value.char, sep = "")
    if (length(z) > 1L) {
      paste(z, collapse = "; ")
    } else {
      z
    }
  } else {
    z <- paste(value.name, " = ", value.char, sep = "")
    if (length(z) > 1L) {
      paste(z, collapse = "; ")
    } else {
      z
    }
  }
}

#' @rdname plain_label
#'
#' @export
#'
bold_label <- function(value,
                       value.name,
                       digits = 3,
                       fixed = FALSE,
                       output.type = "expression",
                       decimal.mark = getOption("OutDec", default = ".")) {

  if (length(value) == 0 || all(is.na(value))) {
    return(NA_character_)
  }

  stopifnot("Negative value of 'digits'" = digits >= 0)

  if (is.integer(value)) {
    value.char <- as.character(value)
  } else {
    value.char <- value2char(value = value,
                           digits = digits,
                           output.type = output.type,
                           decimal.mark = decimal.mark,
                           format = ifelse(fixed, "f", "g"))
  }

  if (output.type == "expression") {
    z <- paste("bold(", value.name, ")~`=`~", value.char, sep = "")
    if (length(z) > 1L) {
      paste(z, collapse = "*\"; \"*")
    } else {
      z
    }
  } else if (grepl("^latex", output.type)) {
    z <- paste("\\mathbf{", value.name, "} = ", value.char, sep = "")
    if (length(z) > 1L) {
      z <- paste(z, collapse = "\\mathrm{; }")
    }
    if (output.type == "latex.eqn") {
      paste("$", z, "$")
    } else if (output.type == "latex.deqn") {
      paste("$$", z, "$$")
    } else {
      z
    }
  } else if (output.type == "markdown") {
    z <- paste("**", value.name, "** = ", value.char, sep = "")
    if (length(z) > 1L) {
      paste(z, collapse = "; ")
    } else {
      z
    }
  } else {
    z <- paste(value.name, " = ", value.char, sep = "")
    if (length(z) > 1L) {
      paste(z, collapse = "; ")
    } else {
      z
    }
  }
}

#' @rdname plain_label
#'
#' @param subscript,superscript character Text for a subscript and superscript
#'   to \emph{P} symbol.
#'
#' @examples
#' p_value_label(value = 0.345, digits = 2, output.type = "expression")
#' p_value_label(value = 0.345, digits = Inf, output.type = "expression")
#' p_value_label(value = 0.345, digits = 6, output.type = "expression")
#' p_value_label(value = 0.345, output.type = "markdown")
#' p_value_label(value = 0.345, output.type = "latex")
#' p_value_label(value = 0.345, subscript = "Holm")
#' p_value_label(value = 1e-25, digits = Inf, output.type = "expression")
#'
#' @export
#'
p_value_label <- function(value,
                          small.p = getOption("ggpmisc.small.p", default = FALSE),
                          subscript = "",
                          superscript = "",
                          digits = 4,
                          fixed = NULL,
                          output.type = "expression",
                          decimal.mark = getOption("OutDec", default = ".")) {

  if (length(value) == 0 || all(is.na(value))) {
    return(NA_character_)
  }

  stopifnot(length(value) == 1L,
            "Negative value of 'digits'" = digits >= 0)

  if (is.null(fixed)) {
    if (digits == Inf) {
      format <- "g"
      digits <- 3
    } else if (digits > 4) {
      format <- "e"
    } else {
      format <- "f"
    }
  } else {
    format <- ifelse(fixed, "f", "g")
  }

  # we accept and trim slightly off-range values
  if (value < 0 && value > -1e-12) {
    value <- 0
  } else if (value > 1 && value < 1 + 1e-12) {
    value <- 1
  } else if (value < 0 || value > 1) {
    warning("Out of range P-value (", value, ") replaced by NA")
    return(NA_character_)
  }

  if (digits < 2) {
    warning("'digits < 2' Likely information loss!")
  }
  p.value <- value

  if (is.null(subscript) | is.na(subscript) |
      !is.character(subscript) | length(subscript) != 1L) {
    subscript <- ""
  }
  if (is.null(superscript) | is.na(superscript) |
      !is.character(superscript) | length(superscript) != 1L) {
    subscript <- ""
  }

  p.value.char <- value2char(value = p.value,
                             digits = digits,
                             output.type = output.type,
                             decimal.mark = decimal.mark,
                             format = format)

  format <- paste("%#.*", format, sep = "")
  protected.format <- paste("\"", format, "\"", sep = "")

  if (output.type == "expression") {
    paste(paste(ifelse(small.p, "italic(p)",  "italic(P)"),
                ifelse(subscript != "",
                       paste("[", subscript, "]", sep = ""),
                       ""),
                ifelse(superscript != "",
                       paste("^{", superscript, "}", sep = ""),
                       ""),
                sep = ""),
          ifelse(p.value < 10^(-digits),
                 sprintf_dm(protected.format, digits, 10^(-digits),
                            decimal.mark = decimal.mark),
                 p.value.char),
          sep = ifelse(p.value < 10^(-digits),
                       "~`<`~",
                       "~`=`~"))
  } else if (grepl("^latex", output.type) || output.type == "text") {
    z <- paste(paste(ifelse(small.p, "p",  "P"),
                     ifelse(subscript != "",
                            paste("_{", subscript, "}", sep = ""),
                            ""),
                     ifelse(superscript != "",
                            paste("^{", superscript, "}", sep = ""),
                            ""),
                     sep = ""),
               ifelse(p.value < 10^(-digits),
                      sprintf_dm(format, digits, 10^(-digits),
                                 decimal.mark = decimal.mark),
                      p.value.char),
               sep = ifelse(p.value < 10^(-digits),
                            " < ",
                            " = "))
    if (output.type == "latex.eqn") {
      paste("$", z, "$")
    } else if (output.type == "latex.deqn") {
      paste("$$", z, "$$")
    } else {
      z
    }
  } else if (output.type == "markdown") {
    paste(paste(ifelse(small.p, "_p_",  "_P_"),
                ifelse(subscript != "",
                       paste("<sub>", subscript, "</sub>", sep = ""),
                       ""),
                ifelse(superscript != "",
                       paste("<sup>", superscript, "</sup>", sep = ""),
                       ""),
                sep = ""),
          ifelse(p.value < 10^(-digits),
                 sprintf_dm(format, digits, 10^(-digits),
                            decimal.mark = decimal.mark),
                 p.value.char),
          sep = ifelse(p.value < 10^(-digits),
                       " < ",
                       " = "))
  }
}

#' @rdname plain_label
#'
#' @examples
#' f_value_label(value = 123.4567, digits = 2, output.type = "expression")
#' f_value_label(value = 123.4567, digits = Inf, output.type = "expression")
#' f_value_label(value = 123.4567, digits = 6, output.type = "expression")
#' f_value_label(value = 123.4567, output.type = "markdown")
#' f_value_label(value = 123.4567, output.type = "latex")
#' f_value_label(value = 123.4567, df1 = 3, df2 = 123,
#'               digits = 2, output.type = "expression")
#' f_value_label(value = 123.4567, df1 = 3, df2 = 123,
#'               digits = 2, output.type = "latex")
#'
#' @export
#'
f_value_label <- function(value,
                          df1 = NULL,
                          df2 = NULL,
                          digits = 4,
                          fixed = FALSE,
                          output.type = "expression",
                          decimal.mark = getOption("OutDec", default = ".")) {

  if (length(value) == 0 || all(is.na(value))) {
    return(NA_character_)
  }

  stopifnot(length(value) == 1L,
            "Negative value of 'digits'" = digits >= 0)

  if (value < 0 && value > -1e-12) {
    value <- 0
  } else if (value < 0) {
    warning("Out of range F-value (", value, ") replaced by NA")
    return(NA_character_)
  }

  f.value <- value

  if (is.null(df1) || is.null(df2)) {
    return(italic_label(value = f.value,
                        value.name = "F",
                        digits = digits,
                        fixed = fixed,
                        output.type = output.type,
                        decimal.mark = decimal.mark)
           )
  }

  f.value.char <- value2char(value = f.value,
                             digits = digits,
                             output.type = output.type,
                             decimal.mark = decimal.mark,
                             format = ifelse(fixed, "f", "g"))

  df1.char <- as.character(df1)
  df2.char <- as.character(df2)

  if (output.type == "expression") {
    paste("italic(F)[", df1.char,
          "*\",\"*", df2.char,
          "]~`=`~", f.value.char, sep = "")
  } else if (grepl("^latex", output.type)) {
    z <- paste("F_{", df1.char, ",", df2.char,
               "} = ", f.value.char, sep = "")
    if (output.type == "latex.eqn") {
      paste("$", z, "$")
    } else if (output.type == "latex.deqn") {
      paste("$$", z, "$$")
    } else {
      z
    }
  } else if (output.type == "markdown") {
    paste("_F_<sub>", df1.char, ",", df2.char,
          "</sub> = ", f.value.char, sep = "")
  } else {
    paste("F(", df1.char, ",", df2.char,
          ") = ", f.value.char, sep = "")
  }
}

#' @rdname plain_label
#'
#' @examples
#' t_value_label(value = 123.4567, digits = 2, output.type = "expression")
#' t_value_label(value = 123.4567, digits = Inf, output.type = "expression")
#' t_value_label(value = 123.4567, digits = 6, output.type = "expression")
#' t_value_label(value = 123.4567, output.type = "markdown")
#' t_value_label(value = 123.4567, output.type = "latex")
#' t_value_label(value = 123.4567, df = 12,
#'               digits = 2, output.type = "expression")
#' t_value_label(value = 123.4567, df = 123,
#'               digits = 2, output.type = "latex")
#'
#' @export
#'
t_value_label <- function(value,
                          df = NULL,
                          digits = 4,
                          fixed = FALSE,
                          output.type = "expression",
                          decimal.mark = getOption("OutDec", default = ".")) {

  if (length(value) == 0 || all(is.na(value))) {
    return(NA_character_)
  }

  stopifnot(length(value) == 1L)
  t.value <- value

  if (is.null(df)) {
    return(italic_label(value = t.value,
                        value.name = "t",
                        digits = digits,
                        fixed = fixed,
                        output.type = output.type,
                        decimal.mark = decimal.mark)
    )
  }

  t.value.char <- value2char(value = t.value,
                             digits = digits,
                             output.type = output.type,
                             decimal.mark = decimal.mark,
                             format = ifelse(fixed, "f", "g"))
  df.char <- as.character(df)

  if (output.type == "expression") {
    paste("italic(t)[", df.char,
          "]~`=`~", t.value.char, sep = "")
  } else if (grepl("^latex", output.type)) {
    z <- paste("t_{", df.char, "} = ", t.value.char, sep = "")
    if (output.type == "latex.eqn") {
      paste("$", z, "$")
    } else if (output.type == "latex.deqn") {
      paste("$$", z, "$$")
    } else {
      z
    }
  } else if (output.type == "markdown") {
    paste("_t_<sub>", df.char,
          "</sub> = ", t.value.char, sep = "")
  } else {
    paste("t(", df.char, ") = ", t.value.char, sep = "")

  }
}

#' @rdname plain_label
#'
#' @export
#'
z_value_label <- function(value,
                          digits = 4,
                          fixed = FALSE,
                          output.type = "expression",
                          decimal.mark = getOption("OutDec", default = ".")) {

  italic_label(value = value,
               value.name = "z",
               digits = digits,
               fixed = fixed,
               output.type = output.type,
               decimal.mark = decimal.mark)
}

#' @rdname plain_label
#'
#' @export
#'
S_value_label <- function(value,
                          digits = 4,
                          fixed = FALSE,
                          output.type = "expression",
                          decimal.mark = getOption("OutDec", default = ".")) {

  italic_label(value = value,
               value.name = "S",
               digits = digits,
               fixed = fixed,
               output.type = output.type,
               decimal.mark = decimal.mark)
}

#' @rdname plain_label
#'
#' @export
#'
mean_value_label <- function(value,
                             digits = 4,
                             fixed = FALSE,
                             output.type = "expression",
                             decimal.mark = getOption("OutDec", default = ".")) {

  value.name <- if (output.type == "expression") {
    "bar(x)"
  } else if (output.type %in% c("latex", "tex", "tikz")) {
    "\\bar{x}"
  } else if (output.type == "markdown") {
    "mean(x)"
  } else {
    "mean(x)"
  }

  italic_label(value = value,
               value.name = value.name,
               digits = digits,
               fixed = fixed,
               output.type = output.type,
               decimal.mark = decimal.mark)
}

#' @rdname plain_label
#'
#' @export
#'
var_value_label <- function(value,
                             digits = 4,
                             fixed = FALSE,
                             output.type = "expression",
                             decimal.mark = getOption("OutDec", default = ".")) {

  value.name <- if (output.type == "expression") {
    "sigma^2"
  } else if (output.type %in% c("latex", "tex", "tikz")) {
    "\\sigma^2"
  } else if (output.type == "markdown") {
    "&sigma;<sup>2</sup>"
  } else {
    "s^2"
  }

  italic_label(value = value,
               value.name = value.name,
               digits = digits,
               fixed = fixed,
               output.type = output.type,
               decimal.mark = decimal.mark)
}

#' @rdname plain_label
#'
#' @export
#'
sd_value_label <- function(value,
                           digits = 4,
                           fixed = FALSE,
                           output.type = "expression",
                           decimal.mark = getOption("OutDec", default = ".")) {

  value.name <- if (output.type == "expression") {
    "sigma"
  } else if (grepl("^latex", output.type)) {
    "\\sigma"
  } else if (output.type == "markdown") {
    "&sigma;"
  } else {
    "s.d."
  }

  italic_label(value = value,
               value.name = value.name,
               digits = digits,
               fixed = fixed,
               output.type = output.type,
               decimal.mark = decimal.mark)
}

#' @rdname plain_label
#'
#' @export
#'
se_value_label <- function(value,
                           digits = 4,
                           fixed = FALSE,
                           output.type = "expression",
                           decimal.mark = getOption("OutDec", default = ".")) {

  italic_label(value = value,
               value.name = "s.e.",
               digits = digits,
               fixed = fixed,
               output.type = output.type,
               decimal.mark = decimal.mark)
}

#' @rdname plain_label
#'
#' @param method character The method used to estimate correlation, which
#'   selects the symbol used for the value.
#'
#' @examples
#' r_label(value = 0.95, digits = 2, output.type = "expression")
#' r_label(value = -0.95, digits = 2, output.type = "expression")
#' r_label(value = 0.0001, digits = 2, output.type = "expression")
#' r_label(value = -0.0001, digits = 2, output.type = "expression")
#' r_label(value = 0.1234567890, digits = Inf, output.type = "expression")
#' r_label(value = 0.95, digits = 2, method = "pearson")
#' r_label(value = 0.95, digits = 2, method = "kendall")
#' r_label(value = 0.95, digits = 2, method = "spearman")
#'
#' @export
#'
r_label <- function(value,
                    method = "pearson",
                    small.r = getOption("ggpmisc.small.r", default = FALSE),
                    digits = 3,
                    fixed = TRUE,
                    output.type = "expression",
                    decimal.mark = getOption("OutDec", default = ".")) {

  if (length(value) == 0 || all(is.na(value))) {
    return(NA_character_)
  }

  stopifnot(length(value) == 1L,
            "Negative value of 'digits'" = digits >= 0)

  # we accept and trim slightly off-range values
  if (method == "pearson") {
    if (value < -1 && value > -1 - 1e-12) {
      value <- -1
    } else if (value > 1 && value < 1 + 1e-12) {
      value <- 1
    } else if (value < -1 || value > 1) {
      warning("Out of range R-value (", value, ") replaced by 'NA'")
      return(NA_character_)
    }
  }


  if (digits < 2) {
    warning("'digits < 2' Likely information loss!")
  }
  format <- ifelse(fixed, "f", "g")

  r.value <- value

  r.value.char <- value2char(value = r.value,
                             digits = digits,
                             output.type = output.type,
                             decimal.mark = decimal.mark,
                             format = format)

  format <- paste("%#.*", format, sep = "")
  protected.format <- paste("\"", format, "\"", sep = "")

  if (output.type == "expression") {

    r.symbol <-
      if (method == "pearson") {
        ifelse(small.r, "italic(r)", "italic(R)")
      } else if (method == "kendall") {
        "italic(tau)"
      } else if (method == "spearman") {
        "italic(rho)"
      } else {
        method
      }

    if (abs(r.value) < 10^(-digits) & r.value != 0) {
      paste("|", r.symbol, "|", "~ < ~",
            sprintf_dm(protected.format,
                       digits, 10^(-digits), decimal.mark = decimal.mark),
            sep = "")
    } else {
      paste(r.symbol, "~`=`~", r.value.char, sep = "")
    }

  } else if (grepl("^latex", output.type) || output.type == "text") {

    r.symbol <-
      if (method == "pearson") {
        ifelse(small.r, "r", "R")
      } else if (method == "kendall") {
        ifelse(output.type == "text", "tau", "\\tau")
      } else if (method == "spearman") {
        ifelse(output.type == "text", "rho", "\\rho")
      } else {
        method
      }

    if (abs(r.value) < 10^(-digits) & r.value != 0) {
      z <- paste("|", r.symbol, "|", " < ",
                 sprintf_dm(format,
                            digits, 10^(-digits), decimal.mark = decimal.mark),
                 sep = "")
    } else {
      z <- paste(r.symbol, " = ", r.value.char, sep = "")
    }
    if (output.type == "latex.eqn") {
      paste("$", z, "$")
    } else if (output.type == "latex.deqn") {
      paste("$$", z, "$$")
    } else {
      z
    }

  } else if (output.type == "markdown") {

    r.symbol <-
      if (method == "pearson") {
        ifelse(small.r, "_r_", "_R_")
      } else if (method == "kendall") {
        "_&rho;_"
      } else if (method == "spearman") {
        "_&tau;_"
      } else {
        method
      }

    if (abs(r.value) < 10^(-digits) & r.value != 0) {
      paste("|", r.symbol, "|", " < ",
            sprintf_dm(format,
                       digits, 10^(-digits), decimal.mark = decimal.mark),
            sep = "")
    } else {
      paste(r.symbol, " = ", r.value.char, sep = "")
    }

  }
}

#' @rdname plain_label
#'
#' @examples
#' rr_label(value = 0.95, digits = 2, output.type = "expression")
#' rr_label(value = 0.0001, digits = 2, output.type = "expression")
#' rr_label(value = 1e-17, digits = Inf, output.type = "expression")
#'
#' @export
#'
rr_label <- function(value,
                     small.r = getOption("ggpmisc.small.r", default = FALSE),
                     digits = 3,
                     fixed = TRUE,
                     output.type = "expression",
                     decimal.mark = getOption("OutDec", default = ".")) {

  if (length(value) == 0 || all(is.na(value))) {
    return(NA_character_)
  }

  stopifnot(length(value) <= 1L,
            "Negative value of 'digits'" = digits >= 0)

  # we accept and trim slightly off-range values
  if (value < 0 && value > -1e-12) {
    value <- 0
  } else if (value > 1 && value < 1 + 1e-12) {
    value <- 1
  } else if (value < 0 || value > 1) {
    warning("Out of range R^2-value (", value, ") replaced by 'NA'")
    return(NA_character_)
  }

  if (digits < 2) {
    warning("'digits < 2' Likely information loss!")
  }
  format <- ifelse(fixed, "f", "g")

  rr.value <- value

  rr.value.char <- value2char(value = rr.value,
                              digits = digits,
                              output.type = output.type,
                              decimal.mark = decimal.mark,
                              format = format)

  format <- paste("%#.*", format, sep = "")
  protected.format <- paste("\"", format, "\"", sep = "")

  if (output.type == "expression") {
    rr.symbol <- ifelse(small.r, "italic(r)^2", "italic(R)^2")
    if (rr.value < 10^(-digits) & rr.value != 0) {
      paste(rr.symbol,
            sprintf_dm(protected.format,
                       digits, 10^(-digits), decimal.mark = decimal.mark),
            sep = "~`<`~")
    } else {
      paste(rr.symbol, rr.value.char, sep = "~`=`~")
    }
  } else if (grepl("^latex", output.type) || output.type == "text") {
    rr.symbol <- ifelse(small.r, "r^2", "R^2")
    if (rr.value < 10^(-digits) & rr.value != 0) {
      z <- paste(rr.symbol,
                 sprintf_dm(format,
                            digits, 10^(-digits), decimal.mark = decimal.mark),
                 sep = " < ")
    } else {
      z <- paste(rr.symbol, rr.value.char, sep = " = ")
    }
    if (output.type == "latex.eqn") {
      paste("$", z, "$")
    } else if (output.type == "latex.deqn") {
      paste("$$", z, "$$")
    } else {
      z
    }
  } else if (output.type == "markdown") {
    rr.symbol <- ifelse(small.r, "_r_<sup>2</sup>", "_R_<sup>2</sup>")
    if (rr.value < 10^(-digits) & rr.value != 0) {
      paste(rr.symbol,
            sprintf_dm(format,
                       digits, 10^(-digits), decimal.mark = decimal.mark),
            sep = " < ")
    } else {
      paste(rr.symbol, rr.value.char, sep = " = ")
    }
  }
}

#' @rdname plain_label
#'
#' @examples
#' adj_rr_label(value = 0.95, digits = 2, output.type = "expression")
#' adj_rr_label(value = 0.0001, digits = 2, output.type = "expression")
#'
#' @export
#'
adj_rr_label <- function(value,
                         small.r = getOption("ggpmisc.small.r", default = FALSE),
                         digits = 3,
                         fixed = TRUE,
                         output.type = "expression",
                         decimal.mark = getOption("OutDec", default = ".")) {

  if (length(value) == 0 || all(is.na(value))) {
    return(NA_character_)
  }

  stopifnot(length(value) == 1L,
            "Negative value of 'digits'" = digits >= 0)

  # we accept and trim slightly off-range values
  # adjusted R^2 can have values < 0!
  if (value > 1 && value < 1 + 1e-12) {
    value <- 1
  } else if (value > 1) {
    warning("Out of range adjusted R^2-value (", value, ") replaced by 'NA'")
    return(NA_character_)
  }

  if (digits < 2) {
    warning("'digits < 2' Likely information loss!")
  }
  format <- ifelse(fixed, "f", "g")

  adj.rr.value <- value

  adj.rr.value.char <- value2char(value = adj.rr.value,
                                  digits = digits,
                                  output.type = output.type,
                                  decimal.mark = decimal.mark,
                                  format = format)

  format <- paste("%#.*", format, sep = "")
  protected.format <- paste("\"", format, "\"", sep = "")

  if (output.type == "expression") {
    paste(ifelse(small.r, "italic(r)[adj]^2", "italic(R)[adj]^2"),
          ifelse(adj.rr.value < 10^(-digits) & adj.rr.value != 0,
                 sprintf_dm(protected.format, digits, 10^(-digits), decimal.mark = decimal.mark),
                 adj.rr.value.char),
          sep = ifelse(adj.rr.value < 10^(-digits) & adj.rr.value != 0,
                       "~`<`~",
                       "~`=`~"))
  } else if (grepl("^latex", output.type)) {
    z <- paste(ifelse(small.r, "r_\\mathrm{adj}^2", "R_\\mathrm{adj}^2"),
               ifelse(adj.rr.value < 10^(-digits),
                      sprintf_dm(format, digits, 10^(-digits), decimal.mark = decimal.mark),
                      adj.rr.value.char),
               sep = ifelse(adj.rr.value < 10^(-digits), " < ", " = "))
    if (output.type == "latex.eqn") {
      paste("$", z, "$")
    } else if (output.type == "latex.deqn") {
      paste("$$", z, "$$")
    } else {
      z
    }
  } else if (output.type == "text") {
    paste(ifelse(small.r, "r_adj^2", "R_adj^2"),
          ifelse(adj.rr.value < 10^(-digits),
                 sprintf_dm(format, digits, 10^(-digits), decimal.mark = decimal.mark),
                 adj.rr.value.char),
          sep = ifelse(adj.rr.value < 10^(-digits), " < ", " = "))
  } else if (output.type == "markdown") {
    paste(ifelse(small.r, "_r_<sup>2</sup><sub>adj</sub>", "_R_<sup>2</sup><sub>adj</sub>"),
          ifelse(adj.rr.value < 10^(-digits),
                 sprintf_dm(protected.format, digits, 10^(-digits), decimal.mark = decimal.mark),
                 adj.rr.value.char),
          sep = ifelse(adj.rr.value < 10^(-digits), " < ", " = "))
  }
}

#' @rdname plain_label
#'
#' @param conf.level numeric critical \emph{P}-value expressed as fraction in
#'   [0..1].
#' @param range.brackets,range.sep character Strings used to format a range.
#'
#' @examples
#' rr_ci_label(value = c(0.3, 0.4), conf.level = 0.95)
#' rr_ci_label(value = c(0.3, 0.4), conf.level = 0.95, output.type = "text")
#' rr_ci_label(value = c(0.3, 0.4), conf.level = 0.95, range.sep = ",")
#'
#' @export
#'
rr_ci_label <- function(value,
                        conf.level,
                        range.brackets = c("[", "]"),
                        range.sep = NULL,
                        digits = 2,
                        fixed = TRUE,
                        output.type = "expression",
                        decimal.mark = getOption("OutDec", default = ".")) {

  if (length(value) < 2L || anyNA(value)) {
    return(NA_character_)
  }

  stopifnot("A CI label needs 'value' of length two" = length(value) == 2L,
            "Negative value of 'digits'" = digits >= 0)

  if (!any(is.na(value)) && is.unsorted(value)) {
    warning("Found unsorted CI limits; sorting them")
    value <- sort(value)
  }

  # we accept and trim slightly off-range values
  if (value[1] < 0 && value[1] > -1e-12) {
    value[1] <- 0
  }
  if (value[2] > 1 && value[2] < 1 + 1e-12) {
    value[2] <- 1
  }
  if (any(value < 0 | value > 1)) {
    warning("Out of range R^2-values (", value, ") replaced by 'NA's")
    value[value < 0 | value > 1] <- NA_real_
  }

  stopifnot(length(value) == 2L,
            "Negative value of 'digits'" = digits >= 0)
  if (digits < 2) {
    warning("'digits < 2' Likely information loss!")
  }
  rr.ci.value <- value

  if (is.null(range.sep)) {
    range.sep <- c("." = ", ", "," = "; ")[decimal.mark]
  }

  rr.ci.char <- character(2)
  rr.ci.char[1] <- value2char(value = rr.ci.value[1],
                              digits = digits,
                              output.type = "text",
                              decimal.mark = decimal.mark,
                              format = ifelse(fixed, "f", "g"))
  rr.ci.char[2] <- value2char(value = rr.ci.value[2],
                              digits = digits,
                              output.type = "text",
                              decimal.mark = decimal.mark,
                              format = ifelse(fixed, "f", "g"))
  rr.ci.char <- paste(rr.ci.char[1], rr.ci.char[2], sep = range.sep)
  if (as.logical((conf.level * 100) %% 1)) {
    conf.level.digits = 1L
  } else {
    conf.level.digits = 0L
  }
  conf.level.char <- as.character(conf.level * 100)

  if (output.type == "expression") {
    paste("\"", conf.level.char, "% CI ",
          range.brackets[1], rr.ci.char, range.brackets[2], "\"", sep = "")
  } else if (grepl("^latex", output.type)) {
    z <- paste(conf.level.char, "\\% \\mathrm{CI} ",
               range.brackets[1], rr.ci.char, range.brackets[2], sep = "")
    if (output.type == "latex.eqn") {
      paste("$", z, "$")
    } else if (output.type == "latex.deqn") {
      paste("$$", z, "$$")
    } else {
      z
    }
  } else if (output.type %in% c("text", "markdown")) {
    paste(conf.level.char, "% CI ",
          range.brackets[1], rr.ci.char, range.brackets[2], sep = "")
  }
}

#' @rdname plain_label
#'
#' @examples
#' r_ci_label(value = c(-0.3, 0.4), conf.level = 0.95)
#' r_ci_label(value = c(-0.3, 0.4), conf.level = 0.95, output.type = "text")
#' r_ci_label(value = c(-0.3, 0.4), conf.level = 0.95, range.sep = ",")
#' r_ci_label(value = c(-1.0, 0.4), conf.level = 0.95, range.sep = ",")
#'
#' @export
#'
r_ci_label <- function(value,
                       conf.level,
                       small.r = getOption("ggpmisc.small.r", default = FALSE),
                       range.brackets = c("[", "]"),
                       range.sep = NULL,
                       digits = 2,
                       fixed = TRUE,
                       output.type = "expression",
                       decimal.mark = getOption("OutDec", default = ".")) {

  if (length(value) == 0 || anyNA(value)) {
    return(NA_character_)
  }

  stopifnot("A CI label needs a 'value' of length two" = length(value) == 2L,
            "Negative value of 'digits'" = digits >= 0)

  if (!any(is.na(value)) && is.unsorted(value)) {
    warning("Found unsorted CI limits; sorting them")
    value <- sort(value)
  }

  # we accept and trim slightly off-range values
  if (value[1] < -1 && value[1] > -1 - 1e-12) {
    value[1] <- -1
  }
  if (value[2] > 1 && value[2] < 1 + 1e-12) {
    value[2] <- 1
  }
  if (any(value < -1 | value > 1)) {
    warning("Out of range R-values (", value, ") replaced by 'NA'")
    value[value < -1 | value > 1] <- NA_real_
  }

  if (digits < 2) {
    warning("'digits < 2' Likely information loss!")
  }
  r.ci.value <- value

  if (is.null(range.sep)) {
    range.sep <- c("." = ", ", "," = "; ")[decimal.mark]
  }

  r.ci.char <- character(2)
  r.ci.char[1] <- value2char(value = r.ci.value[1],
                             digits = digits,
                             output.type = "text",
                             decimal.mark = decimal.mark,
                             format = ifelse(fixed, "f", "g"))
  r.ci.char[2] <- value2char(value = r.ci.value[2],
                             digits = digits,
                             output.type = "text",
                             decimal.mark = decimal.mark,
                             format = ifelse(fixed, "f", "g"))
  r.ci.char <- paste(r.ci.char[1], r.ci.char[2], sep = range.sep)
  if (as.logical((conf.level * 100) %% 1)) {
    conf.level.digits = 1L
  } else {
    conf.level.digits = 0L
  }
  conf.level.char <- as.character(conf.level * 100)

  if (output.type == "expression") {
    paste("\"", conf.level.char, "% CI ",
          range.brackets[1], r.ci.char, range.brackets[2], "\"", sep = "")
  } else if (grepl("^latex", output.type)) {
    z <- paste(conf.level.char, "\\% \\mathrm{CI} ",
               range.brackets[1], r.ci.char, range.brackets[2], sep = "")
    if (output.type == "latex.eqn") {
      paste("$", z, "$")
    } else if (output.type == "latex.deqn") {
      paste("$$", z, "$$")
    } else {
      z
    }
  } else if (output.type %in% c("text", "markdown")) {
    z <- paste(conf.level.char, "% CI ",
               range.brackets[1], r.ci.char, range.brackets[2], sep = "")
  }
}

#' Validate output type
#'
#' Replace \code{NULL} \code{output.type} based on \code{geom} and validate
#' other values. Convert synonyms and change into lower case mal-formed
#' input.
#'
#' @param output.type character User-set argument or default from stat
#' @param geom character The name of the geom that will be used to render the
#'   labels.
#' @param supported.types character vector of accepted values for user input.
#'
#' @return If \code{output.type} is \code{NULL} a suitable value based on the
#'   name of the geom is returned, defaulting to "expression". If not
#'   \code{NULL}, the value is passed through unchanged.
#'
#' @section Output types: The formatting of character strings to be displayed
#' in plots are marked as mathematical equations. Depending on the geom used,
#' the mark-up needs to be encoded differently, or in some cases mark-up not
#' applied.
#' \describe{
#'   \item{\code{"expression"}}{The labels are encoded as character strings to be parsed into R's plotmath expressions.}
#'   \item{\code{"LaTeX", "TeX", "tikz", "latex"}}{The labels are encode as \eqn{\LaTeX} maths equations, without the "fences" for switching in math mode.}
#'   \item{\code{"latex.eqn"}}{Same as \code{"latex"} but enclosed in single \code{$}, i.e., as in-line maths.}
#'   \item{\code{"latex.deqn"}}{Same as \code{"latex"} but enclosed in double \code{$$}, i.e., as display maths.}
#'   \item{\code{"markdown"}}{The labels are encoded as character strings using markdown syntax, with some embedded HTML.}
#'   \item{\code{"text"}}{The labels are plain ASCII character strings.}
#'   \item{\code{"numeric"}}{No labels are generated. This value is accepted by the statistics, but not by the label formatting functions.}
#'   \item{\code{NULL}}{The value used, \code{expression}, \code{latex.eqn} or \code{markup} depends on the argument passed to \code{geom}.}}
#'
#' If \code{geom = "latex"} (package 'xdvir') the output type used is
#' \code{"latex.eqn"}. If \code{geom = "richtext"} (package 'ggtext') or
#' \code{geom = "textbox"} (package 'ggtext') the output type used is
#' \code{"markdown"}. If \code{geom = "marquee"} (package 'marquee') the output
#' type used is \code{"text"}. For all other values of \code{geom} the default
#' is \code{"expression"} unless the user passes an argument. Invalid values as
#' argument trigger an Error.
#'
#' @examples
#' check_output_type(NULL)
#' check_output_type("text")
#' check_output_type(NULL, geom = "text")
#' check_output_type(NULL, geom = "latex")
#'
#' @export
#'
check_output_type <-
  function(output.type,
           geom = "text",
           supported.types =
             c("expression", "text", "markdown", "numeric",
               "latex", "latex.eqn", "latex.deqn")) {
    if (is.null(output.type)) {
      if (geom %in% c("richtext", "textbox")) { # , "marquee" needs different markup
        output.type <- "markdown"
      } else if (geom == "latex") { # package 'xdvir'
        output.type <- "latex.eqn"
      } else if (geom == "marquee") { # package 'marquee'
        message("Currently Markdown is supported with 'ggtext', not 'Marquee'")
        output.type <- "text"
      } else {
        output.type <- "expression"
      }
    } else {
      output.type <- tolower(output.type)
      # simplify tests elsewhere
      if (output.type %in% c("tex", "tikz")) {
        output.type <- "latex"
      }
    }
    if (!output.type %in% supported.types) {
      stop("'", output.type, "' not supported!",
           " Expected one of: ", paste(supported.types, collapse = ", "), ".")
    } else {
      output.type
    }
  }

