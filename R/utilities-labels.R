#' Format numeric values as strings
#'
#' Using \code{\link{sprintf}} flexibly format numbers as character strings
#' encoded for parsing into R expressions or using \eqn{\LaTeX} or markdown
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
#' @param digits integer Number of digits to which numeric values are formatetd.
#' @param fixed logical Interpret \code{digits} as indicating a number of
#'   digits after the decimal mark or as the number of significant digits.
#' @param output.type character One of "expression", "latex", "tex", "text",
#'   "tikz", "markdown".
#'
#' @examples
#'
#' value2char(2.34)
#' value2char(2.34, digits = 3, fixed = FALSE)
#' value2char(2.34, digits = 3, fixed = TRUE)
#' value2char(2.34, output.type = "text")
#'
#' @export
#'
value2char <- function(value,
                       digits = Inf,
                       fixed = FALSE,
                       output.type = "expression",
                       decimal.mark = getOption("OutDec", default = ".")) {
  if (output.type == "expression") {
    if (digits == Inf) {
      temp.char <- sprintf_dm("\"%#.2e\"", value, decimal.mark = decimal.mark)
    } else {
      temp.char <- sprintf_dm(ifelse(fixed, "\"%#.*f\"", "\"%#.*g\""),
                              digits, value, decimal.mark = decimal.mark)
    }
    if (grepl("e", temp.char)) {
      paste(gsub("e", "\" %*% 10^{\"", temp.char), "}", sep = "")
    } else {
      temp.char
    }
  } else {
    if (digits == Inf) {
      temp.char <- sprintf_dm("%#.2e", value, decimal.mark = decimal.mark)
    } else {
      temp.char <- sprintf_dm(ifelse(fixed, "%#.*f", "%#.*g"),
                              digits, value, decimal.mark = decimal.mark)
    }
    if (output.type %in% c("latex", "tex", "tikz") && grepl("e", temp.char)) {
      paste(gsub("e", " \times 10^{", temp.char), "}", sep = "")
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
#' @param value numeric The value of the estimate.
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
plain_label <- function(value,
                        value.name,
                        digits = 3,
                        fixed = FALSE,
                        output.type = "expression",
                        decimal.mark = getOption("OutDec", default = ".")) {

  stopifnot(length(value) == 1L)

  if (is.na(value) || is.nan(value)) {
    return(NA_character_)
  }

  if (is.integer(value)) {
    value.char <- as.character(value)
  } else {
    value.char <- value2char(value = value,
                             digits = digits,
                             output.type = output.type,
                             decimal.mark = decimal.mark,
                             fixed = fixed)
  }

  if (output.type == "expression") {
    paste("plain(", value.name, ")~`=`~", value.char, sep = "")
  } else if (output.type %in% c("latex", "tex", "tikz")) {
    paste("\\mathrm{", value.name, "} = ", value.char, sep = "")
  } else if (output.type %in% c("text", "markdown")) {
    paste(value.name, " = ", value.char, sep = "")
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

  stopifnot(length(value) == 1L)

  if (is.na(value) || is.nan(value)) {
    return(NA_character_)
  }

  if (is.integer(value)) {
    value.char <- as.character(value)
  } else {
    value.char <- value2char(value = value,
                           digits = digits,
                           output.type = output.type,
                           decimal.mark = decimal.mark,
                           fixed = fixed)
  }

  if (output.type == "expression") {
    paste("italic(", value.name, ")~`=`~", value.char, sep = "")
  } else if (output.type %in% c("latex", "tex", "tikz")) {
    paste("\\mathit{", value.name, "} = ", value.char, sep = "")
  } else if (output.type %in% c("text", "markdown")) {
    paste("_", value.name, "_ = ", value.char, sep = "")
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

  stopifnot(length(value) == 1L)

  if (is.na(value) || is.nan(value)) {
    return(NA_character_)
  }

  if (is.integer(value)) {
    value.char <- as.character(value)
  } else {
    value.char <- value2char(value = value,
                           digits = digits,
                           output.type = output.type,
                           decimal.mark = decimal.mark,
                           fixed = fixed)
  }

  if (output.type == "expression") {
    paste("bold(", value.name, ")~`=`~", value.char, sep = "")
  } else if (output.type %in% c("latex", "tex", "tikz")) {
    paste("\\mathbf{", value.name, "} = ", value.char, sep = "")
  } else if (output.type %in% c("text", "markdown")) {
    paste("**", value.name, "** = ", value.char, sep = "")
  }
}

#' @rdname plain_label
#'
#' @param subscript character Text for a subscript to P symbol
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
                          small.p = FALSE,
                          subscript = "",
                          digits = 4,
                          fixed = TRUE,
                          output.type = "expression",
                          decimal.mark = getOption("OutDec", default = ".")) {

  stopifnot(length(value) == 1L,
            "Out of range P-value" = is.na(value) | (value >= 0 & value <= 1),
            "Negative value of 'digits'" = digits > 0)
  if (digits < 2) {
    warning("'digits < 2' Likely information loss!")
  }
  p.value <- value

  if (is.na(subscript) | !is.character(subscript) | length(subscript) != 1L) {
    subscript <- ""
  }

  if (is.na(p.value) || is.nan(p.value)) {
    return(NA_character_)
  }

  p.value.char <- value2char(value = p.value,
                             digits = digits,
                             output.type = output.type,
                             decimal.mark = decimal.mark,
                             fixed = fixed)

  if (output.type == "expression") {
    paste(paste(ifelse(small.p, "italic(p)",  "italic(P)"),
                ifelse(subscript != "",
                       paste("[", subscript, "]", sep = ""),
                       ""),
                sep = ""),
          ifelse(p.value < 10^(-digits),
                 sprintf_dm("\"%.*f\"", digits, 10^(-digits),
                            decimal.mark = decimal.mark),
                 p.value.char),
          sep = ifelse(p.value < 10^(-digits),
                       "~`<`~",
                       "~`=`~"))
  } else if (output.type %in% c("latex", "tex", "text", "tikz")) {
    paste(paste(ifelse(small.p, "p",  "P"),
                subscript <- ifelse(subscript != "",
                                    paste("_{", subscript, "}", sep = ""),
                                    character()),
                sep = ""),
          ifelse(p.value < 10^(-digits),
                 sprintf_dm("\"%.*f\"", digits, 10^(-digits),
                            decimal.mark = decimal.mark),
                 p.value.char),
          sep = ifelse(p.value < 10^(-digits),
                       " < ",
                       " = "))
  } else if (output.type == "markdown") {
    paste(paste(ifelse(small.p, "_p_",  "_P_"),
                subscript <- ifelse(subscript != "",
                                    paste("<sub>", subscript, "</sub>", sep = ""),
                                    character()),
                sep = ""),
          ifelse(p.value < 10^(-digits),
                 sprintf_dm("\"%.*f\"", digits, 10^(-digits),
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

  stopifnot(length(value) == 1L,
            "Out of range F-value" = is.na(value) | value >= 0)
  f.value <- value

  if (is.na(f.value) || is.nan(f.value)) {
    return(NA_character_)
  }

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
                             fixed = fixed)

  df1.char <- as.character(df1)
  df2.char <- as.character(df2)

  if (output.type == "expression") {
    paste("italic(F)[", df1.char,
          "*\",\"*", df2.char,
          "]~`=`~", f.value.char, sep = "")
  } else if (output.type %in% c("latex", "tex", "text", "tikz")) {
    paste("F_{", df1.char, ",", df2.char,
          "} = ", f.value.char, sep = "")
  } else if (output.type == "markdown") {
    paste("_F_<sub>", df1.char, ",", df2.char,
          "</sub> = ", f.value.char, sep = "")
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

  stopifnot(length(value) == 1L)
  t.value <- value

  if (is.na(t.value) || is.nan(t.value)) {
    return(NA_character_)
  }

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
                             fixed = fixed)
  df.char <- as.character(df)

  if (output.type == "expression") {
    paste("italic(t)[", df.char,
          "]~`=`~", t.value.char, sep = "")
  } else if (output.type %in% c("latex", "tex", "text", "tikz")) {
    paste("t_{", df.char,
          "} = ", t.value.char, sep = "")
  } else if (output.type == "markdown") {
    paste("_t_<sub>", df.char,
          "</sub> = ", t.value.char, sep = "")
  }
}

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

sd_value_label <- function(value,
                           digits = 4,
                           fixed = FALSE,
                           output.type = "expression",
                           decimal.mark = getOption("OutDec", default = ".")) {
  value.name <- if (output.type == "expression") {
    "sigma"
  } else if (output.type %in% c("latex", "tex", "tikz")) {
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
                    small.r = FALSE,
                    digits = 3,
                    fixed = TRUE,
                    output.type = "expression",
                    decimal.mark = getOption("OutDec", default = ".")) {

  if (method == "pearson") {
    stopifnot(length(value) == 1L,
              "Out of range R" = is.na(value) || abs(value) <= 1,
              "Negative value of 'digits'" = digits > 0)
  } else {
    stopifnot(length(value) == 1L,
              "Negative value of 'digits'" = digits > 0)
  }

  if (digits < 2) {
    warning("'digits < 2' Likely information loss!")
  }
  r.value <- value

  if (is.na(r.value) || is.nan(r.value)) {
    return(NA_character_)
  }

  r.value.char <- value2char(value = r.value,
                             digits = digits,
                             output.type = output.type,
                             decimal.mark = decimal.mark,
                             fixed = fixed)

  if (output.type == "expression") {

    r.symbol <-
      if (method == "pearson") {
        ifelse(small.r, "italic(r)", "italic(R)")
      } else if (method == "kendall") {
        "italic(tau)"
      } else if (method == "spearman") {
        "italic(rho)"
      } else {
        character(0)
      }

    if (abs(r.value) < 10^(-digits) & r.value != 0) {
      paste("|", r.symbol, "|", "~ < ~",
            sprintf_dm("\"%.*f\"", digits, 10^(-digits), decimal.mark = decimal.mark),
            sep = "")
    } else {
      paste(r.symbol, "~`=`~", r.value.char)
    }

  } else if (output.type %in% c("latex", "tex", "text", "tikz")) {

    r.symbol <-
      if (method == "pearson") {
        ifelse(small.r, "r", "R")
      } else if (method == "kendall") {
        ifelse(output.type == "text", "tau", "\tau")
      } else if (method == "spearman") {
        ifelse(output.type == "text", "rho", "\rho")
      } else {
        character(0)
      }

    if (abs(r.value) < 10^(-digits) & r.value != 0) {
      paste("|", r.symbol, "|", " < ",
            sprintf_dm("%.*f", digits, 10^(-digits), decimal.mark = decimal.mark),
            sep = "")
    } else {
      paste(r.symbol, " = ", r.value.char)
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
        character(0)
      }

    if (abs(r.value) < 10^(-digits) & r.value != 0) {
      paste("|", r.symbol, "|", " < ",
            sprintf_dm("%.*f", digits, 10^(-digits), decimal.mark = decimal.mark),
            sep = "")
    } else {
      paste(r.symbol, " = ", r.value.char)
    }

  }
}

#' @rdname plain_label
#'
#' @examples
#' rr_label(value = 0.95, digits = 2, output.type = "expression")
#' rr_label(value = 0.0001, digits = 2, output.type = "expression")
#'
#' @export
#'
rr_label <- function(value,
                     small.r = FALSE,
                     digits = 3,
                     fixed = TRUE,
                     output.type = "expression",
                     decimal.mark = getOption("OutDec", default = ".")) {

  stopifnot(length(value) == 1L,
            "Out of range R^2" = is.na(value) | (value >= 0 & value <= 1),
            "Negative value of 'digits'" = digits > 0)
  if (digits < 2) {
    warning("'digits < 2' Likely information loss!")
  }
  rr.value <- value

  if (is.na(rr.value) || is.nan(rr.value)) {
    return(NA_character_)
  }

  rr.value.char <- value2char(value = rr.value,
                              digits = digits,
                              output.type = output.type,
                              decimal.mark = decimal.mark,
                              fixed = fixed)

  if (output.type == "expression") {
    rr.symbol <- ifelse(small.r, "italic(r)^2", "italic(R)^2")
    if (rr.value < 10^(-digits) & rr.value != 0) {
      paste(rr.symbol,
            sprintf_dm("\"%.*f\"", digits, 10^(-digits), decimal.mark = decimal.mark),
            sep = "~`<`~")
    } else {
      paste(rr.symbol, rr.value.char, sep = "~`=`~")
    }
  } else if (output.type %in% c("latex", "tex", "text", "tikz")) {
    rr.symbol <- ifelse(small.r, "r^2", "R^2")
    if (rr.value < 10^(-digits) & rr.value != 0) {
      paste(rr.symbol,
            sprintf_dm("%.*f", digits, 10^(-digits), decimal.mark = decimal.mark),
            sep = " < ")
    } else {
      paste(rr.symbol, rr.value.char, sep = " = ")
    }
  } else if (output.type == "markdown") {
    rr.symbol <- ifelse(small.r, "_r_<sup>2</sup>", "_R_<sup>2</sup>")
    if (rr.value < 10^(-digits) & rr.value != 0) {
      paste(rr.symbol,
            as.character(10^(-digits)),
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
                         small.r = FALSE,
                         digits = 3,
                         fixed = TRUE,
                         output.type = "expression",
                         decimal.mark = getOption("OutDec", default = ".")) {

  stopifnot(length(value) == 1L,
            "Negative value of 'digits'" = digits > 0)
  if (digits < 2) {
    warning("'digits < 2' Likely information loss!")
  }
  adj.rr.value <- value

  if (is.na(adj.rr.value) || is.nan(adj.rr.value)) {
    return(NA_character_)
  }

  adj.rr.value.char <- value2char(value = adj.rr.value,
                                  digits = digits,
                                  output.type = output.type,
                                  decimal.mark = decimal.mark,
                                  fixed = fixed)

  if (output.type == "expression") {
    paste(ifelse(small.r, "italic(r)[adj]^2", "italic(R)[adj]^2"),
          ifelse(adj.rr.value < 10^(-digits) & adj.rr.value != 0,
                 sprintf_dm("\"%.*f\"", digits, 10^(-digits), decimal.mark = decimal.mark),
                 adj.rr.value.char),
          sep = ifelse(adj.rr.value < 10^(-digits) & adj.rr.value != 0,
                       "~`<`~",
                       "~`=`~"))
  } else if (output.type %in% c("latex", "tex", "text", "tikz")) {
    paste(ifelse(small.r, "r_{adj}^2", "R_{adj}^2"),
          ifelse(adj.rr.value < 10^(-digits), as.character(10^(-digits)), adj.rr.value.char),
          sep = ifelse(adj.rr.value < 10^(-digits), " < ", " = "))
  } else if (output.type == "markdown") {
    paste(ifelse(small.r, "_r_<sup>2</sup><sub>adj</sub>", "_R_<sup>2</sup><sub>adj</sub>"),
          ifelse(adj.rr.value < 10^(-digits), as.character(10^(-digits)), adj.rr.value.char),
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

  stopifnot(length(value) == 2L,
            "Out of range R^2-value" = all(is.na(value) | (value >= 0 & value <= 1)),
            "Negative value of 'digits'" = digits > 0)
  if (digits < 2) {
    warning("'digits < 2' Likely information loss!")
  }
  rr.ci.value <- sort(value)

  if (is.null(range.sep)) {
    range.sep <- c("." = ", ", "," = "; ")[decimal.mark]
  }

  if (any(is.na(rr.ci.value) | is.nan(rr.ci.value))) {
    return(NA_character_)
  }

  rr.ci.char <- character(2)
  rr.ci.char[1] <- value2char(value = rr.ci.value[1],
                              digits = digits,
                              output.type = "text",
                              decimal.mark = decimal.mark,
                              fixed = fixed)
  rr.ci.char[2] <- value2char(value = rr.ci.value[2],
                              digits = digits,
                              output.type = "text",
                              decimal.mark = decimal.mark,
                              fixed = TRUE)
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
  } else if (output.type %in% c("latex", "tex", "text", "tikz", "markdown")) {
    paste(conf.level.char, "% CI ",
          range.brackets[1], rr.ci.char, range.brackets[2], sep = "")
  }
}
