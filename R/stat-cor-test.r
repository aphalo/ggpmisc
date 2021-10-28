#' @title Annotate plot with correlation test
#'
#' @description \code{stat_cor_test} applies \code{stats::cor.test()} respecting grouping
#' with \code{method = "pearson"} default but
#' alternatively using \code{"kendall"} or \code{"spearman"} methods. It
#' generates labels for correlation coefficients and p-value, coefficient of
#' determination (R^2) for method "pearson" and number of observations.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset, only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points on this
#'   layer
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical indicating whether NA values should be stripped before
#'   the computation proceeds.
#' @param formula a formula object. Using aesthetic names \code{x} and \code{y}
#'   instead of original variable names.
#' @param method character One of "pearson", "kendall" or "spearman".
#' @param small.r,small.p logical Flags to switch use of lower case r and p for
#'   coefficient of correlation (only for \code{method = "pearson}) and p-value.
#' @param coef.keep.zeros logical Keep or drop trailing zeros when formatting
#'   the fitted coefficients and F-value.
#' @param r.digits,rr.digits,p.digits integer Number of digits after the decimal point to
#'   use for R, r.squared, tau or rho and P-value in labels.
#' @param label.x,label.y \code{numeric} with range 0..1 "normalized parent
#'   coordinates" (npc units) or character if using \code{geom_text_npc()} or
#'   \code{geom_label_npc()}. If using \code{geom_text()} or \code{geom_label()}
#'   numeric in native data units. If too short they will be recycled.
#' @param hstep,vstep numeric in npc units, the horizontal and vertical step
#'   used between labels for different groups.
#' @param output.type character One of "expression", "LaTeX", "text",
#'   "markdown" or "numeric".
#' @param orientation character Either "x" or "y" controlling the default for
#'   \code{formula}.
#' @param parse logical Passed to the geom. If \code{TRUE}, the labels will be
#'   parsed into expressions and displayed as described in \code{?plotmath}.
#'   Default is \code{TRUE} if \code{output.type = "expression"} and
#'   \code{FALSE} otherwise.
#'
#' @note For backward compatibility a logical is accepted as argument for
#'   \code{eq.with.lhs}. If \code{TRUE}, the default is used, either
#'   \code{"x"} or \code{"y"}, depending on the argument passed to \code{formula}.
#'   Parameter \code{orientation} is redundant as it only affects the default
#'   for \code{formula} but is included for consistency with
#'   \code{ggplot2::stat_smooth()}.
#'
#' @details This statistic can be used to annotate a plot with the correlation
#'   coefficient and the outcome of its test of significance. It supports
#'   Pearson, Kendall and Spearman methods to compute correlation. This stat
#'   generates labels as R expressions by default but LaTeX (use TikZ device),
#'   markdown (use package 'ggtext') and plain text are also supported, as well
#'   as numeric values for user-generated text labels. The value of \code{parse}
#'   is set automatically based on \code{output-type}, but if you assemble
#'   labels that need parsing from \code{numeric} output, the default needs to
#'   be overridden.
#'
#'   A ggplot statistic receives as \code{data} a data frame that is not the one
#'   passed as argument by the user, but instead a data frame with the variables
#'   mapped to aesthetics. Similarly to  \code{stat_smooth()} the correlation
#'   tests respect grouping, so the scales used for \code{x} and \code{y} should
#'   both be continuous scales rather than discrete.
#'
#' @note This function is similar to \code{ggpubr::stat_cor()} but
#'    rewritten to match functionality and interface used in recent versions of
#'   'ggpmisc'.
#'
#' @section Aesthetics: \code{stat_poly_eq} understands \code{x} and \code{y},
#'   to be referenced in the \code{formula} and \code{weight} passed as argument
#'   to parameter \code{weights}. All three must be mapped to
#'   \code{numeric} variables. In addition, the aesthetics understood by the geom
#'   (\code{"text"} is the default) are understood and grouping respected.
#'
#' @section Computed variables:
#' If \code{output.type} is other than \code{"numeric"} the returned tibble contains
#' the columns listed below. If the model fit function used does not return a given estimate
#' the label is set to \code{character(0L)}.
#' \describe{
#'   \item{x,npcx}{x position}
#'   \item{y,npcy}{y position}
#'   \item{eq.label}{equation for the fitted polynomial as a character string to be parsed}
#'   \item{rr.label}{\eqn{R^2} of the fitted model as a character string to be parsed}
#'   \item{adj.rr.label}{Adjusted \eqn{R^2} of the fitted model as a character string to be parsed}
#'   \item{f.value.label}{F value and degrees of freedom for the fitted model as a whole.}
#'   \item{p.value.label}{P-value for the F-value above.}
#'   \item{AIC.label}{AIC for the fitted model.}
#'   \item{BIC.label}{BIC for the fitted model.}
#'   \item{n.label}{Number of observations used in the fit.}
#'   \item{grp.label}{Set according to mapping in \code{aes}.}
#'   \item{r.squared, adj.r.squared, p.value, n}{numeric values, from the model fit object}}
#'
#' If \code{output.type} is \code{"numeric"} the returned tibble contains the columns
#' listed below. If the model fit function used does not return a given estimate
#' its value is set to \code{NA_real_}.
#' \describe{
#'   \item{x,npcx}{x position}
#'   \item{y,npcy}{y position}
#'   \item{coef.ls}{list containing the "coefficients" matrix from the summary of the fit object}
#'   \item{r.squared, adj.r.squared, f.value, f.df1, f.df2, p.value, AIC, BIC, n}{numeric values, from the model fit object}
#'   \item{grp.label}{Set according to mapping in \code{aes}.}
#'   \item{b_0.constant}{TRUE is polynomial is forced through the origin}
#'   \item{b_i}{One or columns with the coefficient estimates}}
#'
#' To explore the computed values returned for a given input we suggest the use
#' of \code{\link[gginnards]{geom_debug}} as shown in the last examples below.
#'
#' @export
#'
#' @examples
#' # generate artificial data
#' set.seed(4321)
#' x <- (1:100) / 10
#' y <- x + rnorm(length(x))
#' my.data <- data.frame(x = x, y = y,
#'                       group = c("A", "B"))
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_cor_test()
#'
stat_cor_test <- function(mapping = NULL,
                          data = NULL,
                          geom = "text_npc",
                          position = "identity",
                          ...,
                          method = "pearson",
                          formula = NULL,
                          alternative = "two.sided",
                          exact = NULL,
                          conf.level = 0.95,
                          continuity = FALSE,
                          small.r = FALSE,
                          small.p = FALSE,
                          coef.keep.zeros = TRUE,
                          r.digits = 2,
                          t.digits = 3,
                          p.digits = 3,
                          label.x = "left",
                          label.y = "top",
                          hstep = 0,
                          vstep = NULL,
                          output.type = NULL,
                          na.rm = FALSE,
                          orientation = NA,
                          parse = NULL,
                          show.legend = FALSE,
                          inherit.aes = TRUE) {
  if (is.null(output.type)) {
    if (geom %in% c("richtext", "textbox")) {
      output.type <- "markdown"
    } else {
      output.type <- "expression"
    }
  }
  if (is.null(parse)) {
    parse <- output.type == "expression"
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatCorTest,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(method = method,
                  formula = formula,
                  alternative = alternative,
                  exact = exact,
                  conf.level = conf.level,
                  continuity = continuity,
                  small.r = small.r,
                  small.p = small.p,
                  coef.keep.zeros = coef.keep.zeros,
                  r.digits = r.digits,
                  t.digits = t.digits,
                  p.digits = p.digits,
                  label.x = label.x,
                  label.y = label.y,
                  hstep = hstep,
                  vstep = ifelse(is.null(vstep),
                                 ifelse(grepl("label", geom),
                                        0.10,
                                        0.05),
                                 vstep),
                  npc.used = grepl("_npc", geom),
                  output.type = output.type,
                  na.rm = na.rm,
                  orientation = orientation,
                  parse = parse,
                  ...)
  )
}

# Defined here to avoid a note in check --as-cran as the import from 'polynom'
# is not seen when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
cor_test_compute_fun <- function(data,
                                 scales,
                                 method,
                                 alternative,
                                 exact,
                                 conf.level,
                                 continuity,
                                 formula,
                                 small.r,
                                 small.p,
                                 coef.keep.zeros,
                                 r.digits,
                                 t.digits,
                                 p.digits,
                                 label.x,
                                 label.y,
                                 hstep,
                                 vstep,
                                 npc.used,
                                 output.type,
                                 na.rm,
                                 orientation) {
  force(data)

  # we guess formula from orientation
  if (is.null(formula)) {
    if (is.na(orientation) || orientation == "x") {
      formula = y ~ x
    } else if (orientation == "y") {
      formula = x ~ y
    }
  }
  # we guess orientation from formula
  if (is.na(orientation)) {
    orientation <- unname(c(x = "y", y = "x")[as.character(formula)[2]])
  }

  output.type <- tolower(output.type)
  stopifnot(output.type %in%
              c("expression", "text", "markdown", "numeric", "latex", "tex", "tikz"))

  if (exists("grp.label", data)) {
    if (length(unique(data[["grp.label"]])) > 1L) {
      warning("Non-unique value in 'data$grp.label' for group.")
      grp.label <- ""
    } else {
      grp.label <- data[["grp.label"]][1]
    }
  } else {
    grp.label <- ""
  }

  group.idx <- abs(data$group[1])
  if (length(label.x) >= group.idx) {
    label.x <- label.x[group.idx]
  } else if (length(label.x) > 0) {
    label.x <- label.x[1]
  }
  if (length(label.y) >= group.idx) {
    label.y <- label.y[group.idx]
  } else if (length(label.y) > 0) {
    label.y <- label.y[1]
  }

  data.ok <- TRUE
  if (orientation == "x") {
    if (length(unique(data$x)) < 2) {
      data.ok <- FALSE
      warning("Not enough data to compute correlation",
              group.idx, "; returning NA instead.",
              call. = FALSE)
    }
  } else if (orientation == "y") {
    if (length(unique(data$y)) < 2) {
      data.ok <- FALSE
      warning("Not enough data to perform fit for group ",
              group.idx, "; computing mean instead.",
              call. = FALSE)
      formula = x ~ 1
    }
  }

  htest.ls <- do.call(cor.test,
                      args = list(formula = formula,
                                  alternative = alternative,
                                  method = method,
                                  exact = exact,
                                  conf.level = conf.level,
                                  continuity = continuity,
                                  data = data,
                                  na.action = na.action))

  idx.map <- list(pearson = c(statistic.t = "t.value",
                              parameter.df = "df",
                              p.value = "p.value",
                              estimate.cor = "cor",
                              alternative = "test",
                              conf.int1 = "cor.low",
                              conf.int2 = "cor.hi"),
                  kendall = c(statistic.z = "z.value",
                              p.value = "p.value",
                              estimate.tau = "tau",
                              alternative = "test"),
                  spearman = c(statistic.S = "S.value",
                               p.value = "p.value",
                               estimate.rho = "rho",
                               alternative = "test"))

  z <- tibble::as_tibble_row(unlist(htest.ls[idx.map[[method]]]))
  z[["n"]] <- nrow(na.omit(data[[c("x", "y")]]))
  z[["method"]] <- method
  if (method == "pearson") {
    z[["conf.level"]] <- conf.level
  }

  if (output.type != "numeric") {
    # warn if too narrow formats requested
    stopifnot(r.digits > 0)
    if (r.digits < 2) {
      warning("'r.digits < 2' Likely information loss!")
    }
    stopifnot(t.digits > 0)
    if (t.digits < 2) {
      warning("'t.digits < 2' Likely information loss!")
    }
    stopifnot(p.digits > 0)
    if (p.digits < 2) {
      warning("'p.digits < 2' Likely information loss!")
    }

    # build the character strings
    if (output.type == "expression") {
      r <- z[[c(pearson = "cor", kendall = "tau", spearman = "rho")[method]]]
      r.char <- sprintf("\"%#.*f\"", r.digits, r)
      if (method == "pearson") {
        t.value.char <- sprintf("\"%#.*g\"", t.digits, z[["t.value"]])
        df.char <- as.character(df)
      } else if (method == "kendall") {
        z.value.char <- sprintf("\"%#.*g\"", t.digits, z[["z.value"]])
      } else if (method == "spearman") {
        S.value.char <- sprintf("\"%#.*g\"", t.digits, z[["S.value"]])
      }
      p.value.char <- sprintf("\"%#.*f\"", p.digits, z[["p.value"]])
    } else {
      r <- z[[c(pearson = "cor", kendall = "tau", spearman = "rho")[method]]]
      r.char <- sprintf("%#.*f", r.digits, r)
      if (method == "pearson") {
        t.value.char <- sprintf("%#.*g", t.digits, z[["t.value"]])
        df.char <- as.character(df)
      } else if (method == "kendall") {
        z.value.char <- sprintf("%#.*g", t.digits, z[["z.value"]])
      } else if (method == "spearman") {
        S.value.char <- sprintf("%#.*g", t.digits, z[["S.value"]])
      }
      p.value.char <- sprintf("%#.*f", p.digits, z[["p.value"]])
    }

    # add labels to data.frame z.labels
    if (output.type == "expression") {

      # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
      z[["p.value.label"]] <-
        ifelse(is.na(z[["p.value"]]), character(0L),
               paste(ifelse(small.p, "italic(p)",  "italic(P)"),
                     ifelse(z[["p.value"]] < 10^(-p.digits),
                            sprintf("\"%.*f\"", p.digits, 10^(-p.digits)),
                            p.value.char),
                     sep = ifelse(z[["p.value"]] < 10^(-p.digits),
                                  "~`<`~",
                                  "~`=`~")))
      z[["n.label"]] <-
        paste("italic(n)~`=`~\"", z[["n"]], "\"", sep = "")
      z[["grp.label"]] <- grp.label
      if (method == "pearson") {
        z[["cor.label"]] <-
          ifelse(is.na(z[["cor"]]), character(0L),
                 paste(ifelse(small.r, "italic(r)", "italic(R)"),
                       ifelse(abs(z[["cor"]]) < 10^(-r.digits),
                              sprintf("\"%.*f\"", r.digits, 10^(-r.digits) * sign(z[["cor"]])),
                              r.char),
                       sep = ifelse(abs(z[["cor"]]) < 10^(-r.digits),
                                    c("~`>`~", "~`<`~", "~`=`~")[sign(z[["cor"]]) + 2],
                                    "~`=`~")))
        z[["t.value.label"]] <- ifelse(is.na(z[["t.value"]]), character(0L),
                                       paste("italic(t)[", df.char, "]~`=`~", t.value.char, sep = ""))
      } else if (method == "kendal") {
        z[["tau.label"]] <- ifelse(is.na(z[["tau"]]), character(0L),
                                   paste("italic(tau)",
                                         ifelse(abs(z[["tau"]]) < 10^(-r.digits),
                                                sprintf("\"%.*f\"", r.digits, 10^(-r.digits) * sign(z[["tau"]])),
                                                r.char),
                                         sep = ifelse(abs(z[["tau"]]) < 10^(-r.digits),
                                                      c("~`>`~", "~`<`~", "~`=`~")[sign(z[["tau"]]) + 2],
                                                      "~`=`~")))
        z[["z.value.label"]] <- ifelse(is.na(z[["t.value"]]), character(0L),
                                       paste("italic(z)~`=`~", z.value.char, sep = ""))

      } else if (method == "spearman") {
        z[["rho.label"]] <- ifelse(is.na(z[["rho"]]), character(0L),
                                   paste("italic(rho)",
                                         ifelse(abs(z[["rho"]]) < 10^(-r.digits),
                                                sprintf("\"%.*f\"", r.digits, 10^(-r.digits) * sign(z[["rho"]])),
                                                r.char),
                                         sep = ifelse(abs(z[["rho"]]) < 10^(-r.digits),
                                                      c("~`>`~", "~`<`~", "~`=`~")[sign(z[["rho"]]) + 2],
                                                      "~`=`~")))
        z[["S.value.label"]] <- ifelse(is.na(z[["S.value"]]), character(0L),
                                       paste("italic(S)~`=`~", S.value.char, sep = ""))
      }

    } else if (output.type %in% c("latex", "tex", "text", "tikz")) {
      # character(0) instead of "" avoids in paste() the insertion of sep for missing labels
      z[["p.value.label"]] <-
        ifelse(is.na(z[["p.value"]]), character(0L),
               paste(ifelse(small.p, "p",  "P"),
                     ifelse(z[["p.value"]] < 10^(-p.digits),
                            sprintf("\"%.*f\"", p.digits, 10^(-p.digits)),
                            p.value.char),
                     sep = ifelse(z[["p.value"]] < 10^(-p.digits),
                                  " < ",
                                  " = ")))
      z[["n.label"]] <-
        paste("n = ", z[["n"]], sep = "")
      z[["grp.label"]] <- grp.label
      if (method == "pearson") {
        z[["cor.label"]] <-
          ifelse(is.na(z[["cor"]]), character(0L),
                 paste(ifelse(small.r, "r", "R"),
                       ifelse(abs(z[["cor"]]) < 10^(-r.digits),
                              sprintf("\"%.*f\"", r.digits, 10^(-r.digits) * sign(z[["cor"]])),
                              r.char),
                       sep = ifelse(abs(z[["cor"]]) < 10^(-r.digits),
                                    c(" > ", " = ", " < ")[sign(z[["cor"]]) + 2],
                                    " = ")))
        z[["t.value.label"]] <- ifelse(is.na(z[["t.value"]]), character(0L),
                                       paste("t_{", df.char, "} = ", t.value.char, sep = ""))
      } else if (method == "kendal") {
        z[["tau.label"]] <- ifelse(is.na(z[["tau"]]), character(0L),
                                   paste(ifelse(output.type == "text",
                                                "tau", "\tau"),
                                         ifelse(abs(z[["tau"]]) < 10^(-r.digits),
                                                sprintf("\"%.*f\"", r.digits, 10^(-r.digits) * sign(z[["tau"]])),
                                                r.char),
                                         sep = ifelse(abs(z[["tau"]]) < 10^(-r.digits),
                                                      c(" > ", " = ", " < ")[sign(z[["tau"]]) + 2],
                                                      " = ")))
        z[["z.value.label"]] <- ifelse(is.na(z[["z.value"]]), character(0L),
                                       paste("z = ", z.value.char, sep = ""))

      } else if (method == "spearman") {
        z[["rho.label"]] <- ifelse(is.na(z[["rho"]]), character(0L),
                                   paste(ifelse(output.type == "text",
                                                "rho", "\rho"),
                                         ifelse(abs(z[["rho"]]) < 10^(-r.digits),
                                                sprintf("\"%.*f\"", r.digits, 10^(-r.digits) * sign(z[["rho"]])),
                                                r.char),
                                         sep = ifelse(abs(z[["rho"]]) < 10^(-r.digits),
                                                      c(" > ", " = ", " < ")[sign(z[["rho"]]) + 2],
                                                      " = ")))
        z[["S.value.label"]] <- ifelse(is.na(z[["S.value"]]), character(0L),
                                       paste("S = ", S.value.char, sep = ""))
      }
    } else if (output.type == "markdown") {
      z[["p.value.label"]] <-
        ifelse(is.na(z[["p.value"]]), character(0L),
               paste(ifelse(small.p, "_p_",  "_P_"),
                     ifelse(z[["p.value"]] < 10^(-p.digits),
                            sprintf("\"%.*f\"", p.digits, 10^(-p.digits)),
                            p.value.char),
                     sep = ifelse(z[["p.value"]] < 10^(-p.digits),
                                  " < ",
                                  " = ")))
      z[["n.label"]] <-
        paste("_n_ = ", z[["n"]], sep = "")
      z[["grp.label"]] <- grp.label
      if (method == "pearson") {
        z[["cor.label"]] <-
          ifelse(is.na(z[["cor"]]), character(0L),
                 paste(ifelse(small.r, "_r_", "_R_"),
                       ifelse(abs(z[["cor"]]) < 10^(-r.digits),
                              sprintf("\"%.*f\"", r.digits, 10^(-r.digits) * sign(z[["cor"]])),
                              r.char),
                       sep = ifelse(abs(z[["cor"]]) < 10^(-r.digits),
                                    c(" > ", " = ", " < ")[sign(z[["cor"]]) + 2],
                                    " = ")))
        z[["t.value.label"]] <- ifelse(is.na(z[["t.value"]]), character(0L),
                                       paste("_t_<sub>", df.char, "</sub> = ", t.value.char, sep = ""))
      } else if (method == "kendal") {
        z[["tau.label"]] <- ifelse(is.na(z[["tau"]]), character(0L),
                                   paste("_&tau;_",
                                         ifelse(abs(z[["tau"]]) < 10^(-r.digits),
                                                sprintf("\"%.*f\"", r.digits, 10^(-r.digits) * sign(z[["tau"]])),
                                                r.char),
                                         sep = ifelse(abs(z[["tau"]]) < 10^(-r.digits),
                                                      c(" > ", " = ", " < ")[sign(z[["tau"]]) + 2],
                                                      " = ")))
        z[["z.value.label"]] <- ifelse(is.na(z[["t.value"]]), character(0L),
                                       paste("_z_ = ", z.value.char, sep = ""))

      } else if (method == "spearman") {
        z[["rho.label"]] <- ifelse(is.na(z[["rho"]]), character(0L),
                                   paste("_&rho;_",
                                         ifelse(abs(z[["rho"]]) < 10^(-r.digits),
                                                sprintf("\"%.*f\"", r.digits, 10^(-r.digits) * sign(z[["rho"]])),
                                                r.char),
                                         sep = ifelse(abs(z[["rho"]]) < 10^(-r.digits),
                                                      c(" > ", " = ", " < ")[sign(z[["rho"]]) + 2],
                                                      " = ")))
        z[["S.value.label"]] <- ifelse(is.na(z[["S.value"]]), character(0L),
                                       paste("_S_ = ", S.value.char, sep = ""))
      }
    } else {
      warning("Unknown 'output.type' argument: ", output.type)
    }
  }

  # set column to map by default to label
  z[["r.label"]] <- switch(method,
                           pearson =  z[["co.label"]],
                           kendall =  z[["tau.label"]],
                           spearman = z[["rho.label"]])

  # Compute label positions
  if (is.character(label.x)) {
    if (npc.used) {
      margin.npc <- 0.05
    } else {
      # margin set by scale
      margin.npc <- 0
    }
    label.x <- ggpp::compute_npcx(x = label.x, group = group.idx, h.step = hstep,
                                  margin.npc = margin.npc)
    if (!npc.used) {
      x.expanse <- abs(diff(range(data$x)))
      x.min <- min(data$x)
      label.x <- label.x * x.expanse + x.min
    }
  }
  if (is.character(label.y)) {
    if (npc.used) {
      margin.npc <- 0.05
    } else {
      # margin set by scale
      margin.npc <- 0
    }
    label.y <- ggpp::compute_npcy(y = label.y, group = group.idx, v.step = vstep,
                                  margin.npc = margin.npc)
    if (!npc.used) {
      y.expanse <- abs(diff(range(data$y)))
      y.min <- min(data$y)
      label.y <- label.y * y.expanse + y.min
    }
  }

  if (npc.used) {
    z$npcx <- label.x
    z$x <- NA_real_
    z$npcy <- label.y
    z$y <- NA_real_
  } else {
    z$x <- label.x
    z$npcx <- NA_real_
    z$y <- label.y
    z$npcy <- NA_real_
  }

  z
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatCorTest <-
  ggplot2::ggproto("StaCorTest",
                   ggplot2::Stat,
                   extra_params = c("na.rm", "parse"),
                   compute_group = cor_test_compute_fun,
                   default_aes =
                     ggplot2::aes(npcx = after_stat(npcx),
                                  npcy = after_stat(npcy),
                                  label = after_stat(r.label),
                                  hjust = "inward", vjust = "inward"),
                   required_aes = c("x", "y")
  )
