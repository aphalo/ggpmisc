#' replace decimal mark used by sprintf() if needed
#'
sprintf_dm <- function(fmt, ..., decimal.mark = ".") {
  if (decimal.mark != ".") {
    gsub(".", decimal.mark, sprintf(fmt, ...), fixed = TRUE)
  } else {
    # in case OS locale uses ","
    gsub(",", ".", sprintf(fmt, ...), fixed = TRUE)
  }
}

#' Format numeric value as character
#'
value2char <- function(value, digits, output.type, decimal.mark, fixed = FALSE) {
  if (output.type == "expression") {
    if (digits > Inf) {
      temp.char <- sprintf_dm("%#.2e", value, decimal.mark = decimal.mark)
      paste(gsub("e", " %*% 10^{", temp.char), "}", sep = "")
    } else {
      sprintf_dm(ifelse(fixed, "\"%#.*f\"", "\"%#.*g\""),
                 digits, value, decimal.mark = decimal.mark)
    }
  } else {
    if (p.digits == Inf) {
      sprintf_dm("%#.2e", value, decimal.mark = decimal.mark)
    } else {
      sprintf_dm(ifelse(fixed, "%#.*f", "%#.*g"),
                 digits, value, decimal.mark = decimal.mark)
    }
  }
}

plain_label <- function(value, value.name, fixed = FALSE,
                          digits, output.type, decimal.mark) {

  if (is.na(value) || is.nan(value)) {
    # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
    return(character(0L))
  }

  value.char <- value2char(value = value,
                           digits = digits,
                           output.type = output.type,
                           decimal.mark = decimal.mark,
                           fixed = fixed)

  if (output.type == "expression") {
    paste(value.name, value.char, sep = "~`=`~")
  } else if (output.type %in% c("latex", "tex", "text", "tikz", "markdown")) {
    paste(value.name, value.char, sep = " = ")
  }
}

italic_label <- function(value, value.name, fixed = FALSE,
                        digits, output.type, decimal.mark) {

  if (is.na(value) || is.nan(value)) {
    # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
    return(character(0L))
  }

  value.char <- value2char(value = value,
                           digits = digits,
                           output.type = output.type,
                           decimal.mark = decimal.mark,
                           fixed = fixed)

  if (output.type == "expression") {
    paste("italic(", value.name, ")~`=`~", value.char, sep = "")
  } else if (output.type %in% c("latex", "tex", "text", "tikz", "markdown")) {
    paste(value.name, value.char, sep = " = ")
  }
}

bold_label <- function(value, value.name, fixed = FALSE,
                         digits, output.type, decimal.mark) {

  if (is.na(value) || is.nan(value)) {
    # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
    return(character(0L))
  }

  value.char <- value2char(value = value,
                           digits = digits,
                           output.type = output.type,
                           decimal.mark = decimal.mark,
                           fixed = fixed)

  if (output.type == "expression") {
    paste("bold(", value.name, ")~`=`~", value.char, sep = "")
  } else if (output.type %in% c("latex", "tex", "text", "tikz", "markdown")) {
    paste(value.name, value.char, sep = " = ")
  }
}

p_value_label <- function(p.value, small.p,
                          digits, output.type, decimal.mark) {

  if (is.na(p.value) || is.nan(p.value)) {
    # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
    return(character(0L))
  }

  p.value.char <- value2char(value = p.value,
                             digits = digits,
                             output.type = output.type,
                             decimal.mark = decimal.mark,
                             fixed = TRUE)

  if (output.type == "expression") {
    paste(ifelse(small.p, "italic(p)",  "italic(P)"),
          ifelse(p.value < 10^(-digits),
                 sprintf_dm("\"%.*f\"", digits, 10^(-digits),
                            decimal.mark = decimal.mark),
                 p.value.char),
          sep = ifelse(p.value < 10^(-digits),
                       "~`<`~",
                       "~`=`~"))
  } else if (output.type %in% c("latex", "tex", "text", "tikz")) {
    paste(ifelse(small.p, "p",  "P"),
          ifelse(p.value < 10^(-digits),
                 sprintf_dm("\"%.*f\"", digits, 10^(-digits),
                            decimal.mark = decimal.mark),
                 p.value.char),
          sep = ifelse(p.value < 10^(-digits),
                       " < ",
                       " = "))
  } else if (output.type == "markdown") {
    paste(ifelse(small.p, "_p_",  "_P_"),
          ifelse(p.value < 10^(.digits),
                 sprintf_dm("\"%.*f\"", digits, 10^(-digits),
                            decimal.mark = decimal.mark),
                 p.value.char),
          sep = ifelse(p.value < 10^(-digits),
                       " < ",
                       " = "))
  }
}

f_value_label <- function(f.value, df1, df2,
                          digits, output.type, decimal.mark) {

  if (is.na(f.value) || is.nan(f.value)) {
    # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
    return(character(0L))
  }

  f.value.char <- value2char(value = f.value,
                             digits = digits,
                             output.type = output.type,
                             decimal.mark = decimal.mark,
                             fixed = FALSE)
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

t_value_label <- function(t.value, df,
                          digits, output.type, decimal.mark) {

  if (is.na(t.value) || is.nan(t.value)) {
    # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
    return(character(0L))
  }

  t.value.char <- value2char(value = t.value,
                             digits = digits,
                             output.type = output.type,
                             decimal.mark = decimal.mark,
                             fixed = FALSE)
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

r_label <- function(correlation, small.r = FALSE,
                     digits, output.type, decimal.mark) {

  if (is.na(correlation) || is.nan(correlation)) {
    # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
    return(character(0L))
  }

  r.char <- value2char(value = correlation,
                        digits = digits,
                        output.type = output.type,
                        decimal.mark = decimal.mark,
                        fixed = TRUE)
  df.char <- as.character(df)

  if (output.type == "expression") {
    paste(ifelse(small.r, "italic(r)^2", "italic(R)^2"),
          ifelse(rr < 10^(-digits) & rr != 0,
                 sprintf_dm("\"%.*f\"", digits, 10^(-digits), decimal.mark = decimal.mark),
                 rr.char),
          sep = ifelse(rr < 10^(-digits) & rr != 0,
                       "~`<`~",
                       "~`=`~"))
  } else if (output.type %in% c("latex", "tex", "text", "tikz")) {
    paste(ifelse(small.r, "r^2", "R^2"),
          ifelse(rr < 10^(-digits), as.character(10^(-digits)), rr.char),
          sep = ifelse(rr < 10^(-rr.digits), " < ", " = "))
  } else if (output.type == "markdown") {
    paste(ifelse(small.r, "_r_<sup>2</sup>", "_R_<sup>2</sup>"),
          ifelse(rr < 10^(-digits), as.character(10^(-digits)), rr.char),
          sep = ifelse(rr < 10^(-digits), " < ", " = "))
  }
}

rr_label <- function(rr, small.r = FALSE,
                     digits, output.type, decimal.mark) {

  if (is.na(rr) || is.nan(rr)) {
    # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
    return(character(0L))
  }

  rr.char <- value2char(value = rr,
                        digits = digits,
                        output.type = output.type,
                        decimal.mark = decimal.mark,
                        fixed = TRUE)
  df.char <- as.character(df)

  if (output.type == "expression") {
    paste(ifelse(small.r, "italic(r)^2", "italic(R)^2"),
          ifelse(rr < 10^(-digits) & rr != 0,
                 sprintf_dm("\"%.*f\"", digits, 10^(-digits), decimal.mark = decimal.mark),
                 rr.char),
          sep = ifelse(rr < 10^(-digits) & rr != 0,
                       "~`<`~",
                       "~`=`~"))
  } else if (output.type %in% c("latex", "tex", "text", "tikz")) {
    paste(ifelse(small.r, "r^2", "R^2"),
          ifelse(rr < 10^(-digits), as.character(10^(-digits)), rr.char),
          sep = ifelse(rr < 10^(-rr.digits), " < ", " = "))
  } else if (output.type == "markdown") {
    paste(ifelse(small.r, "_r_<sup>2</sup>", "_R_<sup>2</sup>"),
          ifelse(rr < 10^(-digits), as.character(10^(-digits)), rr.char),
          sep = ifelse(rr < 10^(-digits), " < ", " = "))
  }
}

rr.adj_label <- function(rr, small.r = FALSE,
                         digits, output.type, decimal.mark) {

  if (is.na(rr) || is.nan(rr)) {
    # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
    return(character(0L))
  }

  rr.char <- value2char(value = rr,
                        digits = digits,
                        output.type = output.type,
                        decimal.mark = decimal.mark,
                        fixed = TRUE)
  df.char <- as.character(df)

  if (output.type == "expression") {
    paste(ifelse(small.r, "italic(r)[adj]^2", "italic(R)[adj]^2"),
          ifelse(adj.rr < 10^(-digits) & adj.rr != 0,
                 sprintf_dm("\"%.*f\"", rr.digits, 10^(-digits), decimal.mark = decimal.mark),
                 adj.rr.char),
          sep = ifelse(adj.rr < 10^(-digits) & adj.rr != 0,
                       "~`<`~",
                       "~`=`~"))
  } else if (output.type %in% c("latex", "tex", "text", "tikz")) {
    paste(ifelse(small.r, "r_{adj}^2", "R_{adj}^2"),
          ifelse(adj.rr < 10^(-digits), as.character(10^(-digits)), adj.rr.char),
          sep = ifelse(adj.rr < 10^(-digits), " < ", " = "))
  } else if (output.type == "markdown") {
    paste(ifelse(small.r, "_r_<sup>2</sup><sub>adj</sub>", "_R_<sup>2</sup><sub>adj</sub>"),
          ifelse(adj.rr < 10^(-digits), as.character(10^(-digits)), adj.rr.char),
          sep = ifelse(adj.rr < 10^(-digits), " < ", " = "))
  }
}

rr.ci_label <- function(rr.ci, conf.level,
                        CI.brackets = c("[", "]"), range.sep = "..",
                        digits, output.type, decimal.mark) {

  if (any(is.na(rr.ci) || is.nan(rr.ci))) {
    # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
    return(character(0L))
  }

  rr.ci.char <- character(2)
  rr.ci.char[1] <- value2char(value = rr.ci[1],
                        digits = digits,
                        output.type = output.type,
                        decimal.mark = decimal.mark,
                        fixed = TRUE)
  rr.ci.char[2] <- value2char(value = rr.ci[2],
                              digits = digits,
                              output.type = output.type,
                              decimal.mark = decimal.mark,
                              fixed = TRUE)
  rr.confint.chr <- paste(rr.ci.char, collapse = range.sep)
  if (as.logical((conf.level * 100) %% 1)) {
    conf.level.digits = 1L
  } else {
    conf.level.digits = 0L
  }
  conf.level.char <- value2char(conf.level * 100,
                                digits = conf.level.digits,
                                output.type = output.type,
                                decimal.mark = decimal.mark,
                                fixed = TRUE)

  if (output.type == "expression") {
    paste("\"", conf.level.char, "% CI ",
          CI.brackets[1], rr.ci.char, CI.brackets[2], "\"", sep = "")
  } else if (output.type %in% c("latex", "tex", "text", "tikz", "markdown")) {
    paste(conf.level.char, "% CI ",
          CI.brackets[1], rr.ci.chr, CI.brackets[2], sep = "")
  }
}

