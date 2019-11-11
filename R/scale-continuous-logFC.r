#' Formatter for fold change tick labels
#'
#' Generate tick labels discounting a log transformation in data, and
#' optionally applying a different log transformation or no transformation.
#'
#' @return a function with single parameter x, a numeric vector, that
#'   returns a character vector.
#'
#' @param log.base.data of data.
#' @param log.base.labels of labels.
#' @param digits number of significant digits to show.
#' @param ... other arguments passed on to \code{\link{format}}.
#' @param x a numeric vector to format.
#'
#' @keywords internal
#'
#' @family Functions for quadrant and volcano plots
#'
#' @export
#'
#' @examples
#'
#' FC_format(2, 10)(1:10)
#' FC_format(2, FALSE)(c(1/4, 1/2, 1,2,4,8))
#' FC_format(10, FALSE)(c(1, 10, 100, 1000))
#' FC_format(FALSE, 10)(c(-1, 1, 2, 3))
#'
FC_format <- function(log.base.data = 2, log.base.labels = 10, digits = 3, ...) {
  function(x) FC_plain(x,
                       log.base.data = log.base.data,
                       log.base.labels = log.base.labels,
                       digits = digits, ...)
}

#' @export
#'
#' @rdname FC_format
#'
FC_plain <- function(x, log.base.data = 2, log.base.labels = 10, digits = 3, ...) {
  if (log.base.data != log.base.labels) {
    if (log.base.data %in% c(2, exp(1), 10)) {
      x <- log.base.data^x
    } else if (log.base.data) {
      stop("Unsuported logarithm base ", log.base.data)
    }
    if (log.base.labels %in% c(2, exp(1), 10)) {
      x <- log(x, base = log.base.labels)
    } else if (log.base.labels) {
      stop("Unsuported logarithm base ", log.base.labels)
    }
  }
  if (log.base.labels) { # log fold change
    x <- signif(x, digits)
    format(x, trim = TRUE, scientific = FALSE, ...)
  } else { # fold change
    ifelse(x < 1,
           paste("1/", format(1 / x, trim = TRUE, scientific = FALSE, digits = 0, ...), sep = ""),
           format(x, trim = TRUE, scientific = FALSE, digits = 0, ...))
  }
}

#' Fold change- axis labels
#'
#' Generate axis labels for different log-fold change on different log bases.
#'
#' @param format character string, "R", "R.expresion", "R.character", or
#'   "LaTeX".
#' @param unit.exponent integer.
#'
#' @return a character string or an R expression.
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#'
#' FC_name()
#' FC_name(format = "R.expression")
#' FC_name(format = "LaTeX")
#' FC_name(name = "Metabolite concentration")
#' FC_name(log.base = 10)
#' FC_name(log.base = 0)
#'
FC_name <- function(name = "Abundance%unit",
                    log.base = FALSE,
                    format = getOption("photobiology.math",
                                       default = "R.expression")) {
  if (!grepl("%unit$", name)) {
    return(name)
  } else {
    name <- sub("%unit$", "", name)
  }
  log.base <- as.integer(log.base)
  stopifnot(log.base %in% c(0L, 2L, 10L))
  if (tolower(format) == "latex") {
    if (!log.base) {
      paste(name, "(fold change)", sep = "")
    } else {
      paste(name, "($log_", log.base, "$ fold change)", sep = "")
    }
  } else if (format %in% c("R.expression")) {
    if (!log.base) {
      bquote(plain(.(name))~~(plain("fold change")))
    } else {
      bquote(plain(.(name))~~(log[.(log.base)]~~plain("fold change")))
    }
  } else if (format == "R.character") {
    if (!log.base) {
      paste(name, "(fold change)", sep = "")
    } else {
      paste(name, "(log", log.base, " fold change)", sep = "")
    }
  } else {
    warning("'format = ", format,
            "' not implemented for fold change")
  }
}

#' Expand limits to be symetric
#'
#' A simple function to expand scale limits to be symetric around zero. Can be
#' passed as argument to parameter \code{limits} of continuous scales from
#' packages 'ggplot2' or 'scales'.
#'
#' @param x numeric The automatic limits
#'
#' @return A numeric vector of length two with the new limits.
#'
#' @export
#'
#' @examples
#'
#' symmetric_limits(c(-1, 1.8))
#'
symmetric_limits <- function(x) {
  max <- max(abs(x))
  c(-max, max)
}

#' Position scales for log fold change data
#'
#' Continuous scales for x and y aesthetics with defaults suitable for values
#' expressed as log2 fold change in \code{data} and fold-change in tick labels.
#' Supports tick labels and data expressed in any combination of fold-change,
#' log2 fold-change and log10 fold-change. Supports addition of units to
#' axis labels passed as argument to the \code{name} formal parameter.
#'
#' @param name The name of the scale without units, used for the axis-label.
#' @param breaks The positions of ticks or a function to generate them. Default
#'   varies depending on argument passed to \code{log.base.labels}. if supplied
#'   as a numeric vector they should be given using the data as passed to
#'   parameter \code{data}.
#' @param labels The tick labels or a function to generate them from the tick
#'   positions. The default is function that uses the arguments passed to
#'   \code{log.base.data} and \code{log.base.labels} to generate suitable
#'   labels.
#' @param limits limits	One of: NULL to use the default scale range from
#'   ggplot2. A numeric
#'   vector of length two providing limits of the scale, using NA to refer to the
#'   existing minimum or maximum. A function that accepts the existing
#'   (automatic) limits and returns new limits. The default is function
#'   \code{symmetric_limits()} which keep 1 at the middle of the axis..
#' @param oob Function that handles limits outside of the scale limits (out of
#'   bounds). The default squishes out-of-bounds values to the boundary.
#' @param expand Vector of range expansion constants used to add some padding
#'   around the data, to ensure that they are placed some distance away from
#'   the axes. The default is to expand the scale by 15\% on each end for
#'   log-fold-data, so as to leave space for counts annotations.
#' @param log.base.labels,log.base.data integer or logical Base of logarithms used to
#'   express fold-change values in tick labels and in \code{data}. Use \code{FALSE}
#'   for no logarithm transformation.
#' @param ... other named arguments passed to \code{scale_y_continuous}.
#'
#' @details These scales only alter default arguments of
#'   \code{scale_x_continuous()} and \code{scale_y_continuous()}. Please, see
#'   documentation for \code{\link[ggplot2]{scale_continuous}} for details. The
#'   name argument supports the use of \code{"\%unit"} at the end of the string to
#'   automatically add a units string, otherwise user-supplied values for
#'   names, breaks, and labels work as usual. Tick labels are built based on
#'   the transformation already applied to the data (log2 by default) and
#'   apossibly different log transformation (default is fold-change with no
#'   transformation).
#'
#' @export
#'
#' @family scales for omics data
#'
#' @examples
#'
#' set.seed(12346)
#' my.df <- data.frame(x = rnorm(50, sd = 4), y = rnorm(50, sd = 4))
#'
#' ggplot(my.df, aes(x, y)) +
#'   geom_point() +
#'   scale_x_logFC() +
#'   scale_y_logFC()
#'
#' ggplot(my.df, aes(x, y)) +
#'   geom_point() +
#'   scale_x_logFC(labels = scales::trans_format(function(x) {log10(2^x)},
#'                          scales::math_format())) +
#'   scale_y_logFC(labels = scales::trans_format(function(x) {log10(2^x)},
#'                          scales::math_format()))
#'
#' ggplot(my.df, aes(x, y)) +
#'   geom_point() +
#'   scale_x_logFC(log.base.labels = 2) +
#'   scale_y_logFC(log.base.labels = 2)
#'
#' ggplot(my.df, aes(x, y)) +
#'   geom_point() +
#'   scale_x_logFC("A concentration%unit", log.base.labels = 10) +
#'   scale_y_logFC("B concentration%unit", log.base.labels = 10)
#'
#' ggplot(my.df, aes(x, y)) +
#'   geom_point() +
#'   scale_x_logFC("A concentration%unit", breaks = NULL) +
#'   scale_y_logFC("B concentration%unit", breaks = NULL)
#'
#' # taking into account that data are expressed as log2 FC.
#' ggplot(my.df, aes(x, y)) +
#'   geom_point() +
#'   scale_x_logFC("A concentration%unit", breaks = log2(c(1/100, 1, 100))) +
#'   scale_y_logFC("B concentration%unit", breaks = log2(c(1/100, 1, 100)))
#'
#' ggplot(my.df, aes(x, y)) +
#'   geom_point() +
#'   scale_x_logFC(labels = scales::trans_format(function(x) {log10(2^x)},
#'                          scales::math_format())) +
#'   scale_y_logFC(labels = scales::trans_format(function(x) {log10(2^x)},
#'                          scales::math_format()))
#'
#' # override "special" default arguments.
#' ggplot(my.df, aes(x, y)) +
#'   geom_point() +
#'   scale_x_logFC("A concentration",
#'                 breaks = waiver(),
#'                 labels = waiver()) +
#'   scale_y_logFC("B concentration",
#'                 breaks = waiver(),
#'                 labels = waiver())
#'
#' ggplot(my.df, aes(x, y)) +
#'   geom_point() +
#'   scale_x_logFC() +
#'   scale_y_logFC() +
#'   geom_quadrant_lines() +
#'   stat_quadrant_counts(size = 3.5)
#'
scale_x_logFC <- function(name = "Abundance of x%unit",
                          breaks = NULL,
                          labels = NULL,
                          limits = symmetric_limits,
                          oob = scales::squish,
                          expand = expand_scale(mult = 0.15, add = 0),
                          log.base.labels = FALSE,
                          log.base.data = 2L,
                          ...) {

  if (is.null(breaks)) {
    if (!log.base.labels) {
      breaks <- 10^seq(from = -4, to = 4, by = 1)
    } else if (log.base.labels == 2L) {
      breaks <- 2^seq(from = -10, to = 10, by = 3)
    } else if (log.base.labels == 10L) {
      breaks <- 10^seq(from = -4, to = 4, by = 1)
    }
    if (log.base.data) {
      breaks <- log(breaks, base = log.base.data)
    }
  }

  if (is.null(labels)) {
    labels = FC_format(log.base.labels = log.base.labels,
                       log.base.data = log.base.data)
  }

  ggplot2::scale_x_continuous(name = FC_name(name = name, log.base = log.base.labels),
                              breaks = breaks,
                              labels = labels,
                              oob = oob,
                              expand = expand,
                              limits = limits,
                              ...)
}

#' @rdname scale_x_logFC
#'
#' @export
#'
scale_y_logFC <- function(name = "Abundance of y%unit",
                          breaks = NULL,
                          labels = NULL,
                          limits = symmetric_limits,
                          oob = scales::squish,
                          expand = expand_scale(mult = 0.15, add = 0),
                          log.base.labels = FALSE,
                          log.base.data = 2L,
                          ...) {

  if (is.null(breaks)) {
    if (!log.base.labels) {
      breaks <- 10^seq(from = -4, to = 4, by = 1)
    } else if (log.base.labels == 2L) {
      breaks <- 2^seq(from = -10, to = 10, by = 3)
    } else if (log.base.labels == 10L) {
      breaks <- 10^seq(from = -4, to = 4, by = 1)
    }
    if (log.base.data) {
      breaks <- log(breaks, base = log.base.data)
    }
  }

  if (is.null(labels)) {
    labels = FC_format(log.base.labels = log.base.labels,
                       log.base.data = log.base.data)
  }

  ggplot2::scale_y_continuous(name = FC_name(name = name, log.base = log.base.labels),
                              breaks = breaks,
                              labels = labels,
                              oob = oob,
                              expand = expand,
                              limits = limits,
                              ...)
}
