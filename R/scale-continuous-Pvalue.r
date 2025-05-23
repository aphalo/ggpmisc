#' Convenience scale for P-values
#'
#' Scales for y aesthetic mapped to P-values as used in volcano plots with
#' transcriptomics and metabolomics data.
#'
#' @param name The name of the scale without units, used for the axis-label.
#' @param transform Either the name of a transformation object, or the object
#'   itself. Use NULL for the default.
#' @param breaks The positions of ticks or a function to generate them. Default
#'   varies depending on argument passed to \code{log.base.labels}.
#' @param labels The tick labels or a function to generate them from the tick
#'   positions. The default is function that uses the arguments passed to
#'   \code{log.base.data} and \code{log.base.labels} to generate suitable
#'   labels.
#' @param limits Use one of: \code{NULL} to use the default scale range, a
#'   numeric vector of length two providing limits of the scale; NA to refer
#'   to the existing minimum or maximum; a function that accepts the existing
#'   (automatic) limits and returns new limits.
#' @param oob Function that handles limits outside of the scale limits (out of
#'   bounds). The default squishes out-of-bounds values to the boundary.
#' @param expand Vector of range expansion constants used to add some padding
#'   around the data, to ensure that they are placed some distance away from
#'   the axes. The default is to expand the scale by 15\% on each end for
#'   log-fold-data, so as to leave space for counts annotations.
#' @param ... other named arguments passed to \code{scale_y_continuous}.
#'
#' @details These scales only alter default arguments of
#'   \code{scale_x_continuous()} and \code{scale_y_continuous()}. Please, see
#'   documentation for \code{\link[ggplot2]{scale_continuous}} for details.
#'
#' @export
#'
#' @family Functions for quadrant and volcano plots
#'
#' @examples
#'
#' set.seed(12346)
#' my.df <- data.frame(x = rnorm(50, sd = 4),
#'                     y = 10^-runif(50, min = 0, max = 20))
#'
#' ggplot(my.df, aes(x, y)) +
#'   geom_point() +
#'   scale_x_logFC() +
#'   scale_y_Pvalue()
#'
#' ggplot(my.df, aes(x, y)) +
#'   geom_point() +
#'   scale_x_logFC() +
#'   scale_y_FDR(limits = c(NA, 1e-20))
#'
scale_y_Pvalue <-
  function(...,
           name = expression(italic(P)-plain(value)), # nolint: infix_spaces_linter, line_length_linter.
           transform = NULL,
           breaks = NULL,
           labels = NULL,
           limits = c(1, 1e-20),
           oob = NULL,
           expand = NULL) {

    default.breaks <-
      c(1, 1e-3, 1e-10, 1e-20, 1e-30, 1e-40, 1e-50, 1e-60, 1e-70, 1e-80)
    default.labels <-
      scales::trans_format("log10", scales::math_format())
    default.expand <-
      ggplot2::expansion(mult = c(0.06, 0.14), add = 0)

    ggplot2::scale_y_continuous(...,
                                name = name,
                                transform = if (is.null(transform))
                                  reverselog_trans(10) else transform,
                                breaks = if (is.null(breaks))
                                  default.breaks else breaks,
                                labels = if (is.null(labels))
                                  default.labels else labels,
                                limits = if (is.null(limits))
                                  c(1, 1e-10) else limits, # axis is reversed!
                                oob = if (is.null(oob))
                                  scales::squish else oob,
                                minor_breaks = NULL,
                                expand = if (is.null(expand))
                                  default.expand else expand)
  }

#' @rdname scale_y_Pvalue
#'
#' @export
#'
scale_y_FDR <- function(...,
                        name = "False discovery rate",
                        transform = NULL,
                        breaks = NULL,
                        labels = NULL,
                        limits = c(1, 1e-10),
                        oob = NULL,
                        expand = NULL) {
  scale_y_Pvalue(...,
                 name = name,
                 transform = transform,
                 breaks = breaks,
                 labels = labels,
                 limits = limits,
                 oob = oob,
                 expand = expand)
}

#' @rdname scale_y_Pvalue
#'
#' @export
#'
scale_x_Pvalue <-
  function(...,
           name = expression(italic(P)-plain(value)), # nolint: infix_spaces_linter, line_length_linter.
           transform = NULL,
           breaks = NULL,
           labels = NULL,
           limits = c(1, 1e-20),
           oob = NULL,
           expand = NULL) {

    default.breaks <-
      c(1, 1e-3, 1e-10, 1e-20, 1e-30, 1e-40, 1e-50, 1e-60, 1e-70, 1e-80)
    default.labels <-
      scales::trans_format("log10", scales::math_format())
    default.expand <-
      ggplot2::expansion(mult = c(0.06, 0.14), add = 0)

    ggplot2::scale_x_continuous(...,
                                name = name,
                                transform = if (is.null(transform))
                                  reverselog_trans(10) else transform,
                                breaks = if (is.null(breaks))
                                  default.breaks else breaks,
                                labels = if (is.null(labels))
                                  default.labels else labels,
                                limits = if (is.null(limits))
                                  c(1, 1e-10) else limits, # axis is reversed!
                                oob = if (is.null(oob))
                                  scales::squish else oob,
                                minor_breaks = NULL,
                                expand = if (is.null(expand))
                                  default.expand else expand)
  }

#' @rdname scale_y_Pvalue
#'
#' @export
#'
scale_x_FDR <- function(...,
                        name = "False discovery rate",
                        transform = NULL,
                        breaks = NULL,
                        labels = NULL,
                        limits = c(1, 1e-10),
                        oob = NULL,
                        expand = NULL) {
  scale_x_Pvalue(...,
                 name = name,
                 transform = transform,
                 breaks = breaks,
                 labels = labels,
                 limits = limits,
                 oob = oob,
                 expand = expand)
}

#' Reverse log transformation
#'
#' @param base numeric Base of logarithm
#'
#' @keywords internal
#'
#' define transformation needed for P-value tick labels
#'
reverselog_trans <- function(base = exp(1)) {
  transform <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  scales::trans_new(paste0("reverselog-", format(base)), transform, inv,
                    scales::log_breaks(base = base),
                    domain = c(1e-100, Inf))
}