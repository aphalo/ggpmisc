# dplyr::select and dplyr::filter -----------------------------------------------------

#' @title Select and slice a tibble nested in \code{data}
#'
#' @description \code{stat_partial_tb} selects columns and/or remanes them
#'   and/or slices rows from a tible nested in \code{data}. This stat is
#'   designed to be used to pre-process \code{tibble} objects mapped to the
#'   \code{label} aesthetic before adding them to a plot with
#'   \code{geom_table}.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
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
#' @param digits integer indicating the number of significant digits to be
#'   retained in data.
#' @param tb.vars character vector, optionally named, used to select and or
#'   rename the columns of the table returned.
#' @param tb.rows integer vector of row indexes of rows to be retained.
#'
#' @section Computed variables: The output of sequentially applying
#'   \code{\link[dplyr]{slice}} with \code{tb.rows} as argument and
#'   \code{\link[dplyr]{select}} with \code{tb.vars} to a list variable
#'   list mapped to \code{label} and containing a single tibble per row
#'   in \code{data}.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' my.df <- tibble::tibble(x = c(1, 2),
#'                     y = c(0, 4),
#'                     group = c("A", "B"),
#'                     tbs = list(a = tibble::tibble(X = 1:6, Y = rep(c("x", "y"), 3)),
#'                                b = tibble::tibble(X = 1:3, Y = "x")))
#'
#' ggplot(my.df, aes(x, y, label = tbs)) +
#'   stat_fmt_tb() +
#'   expand_limits(x = c(0,3), y = c(-2, 6))
#'
#' ggplot(my.df, aes(x, y, label = tbs)) +
#'   stat_fmt_tb(tb.vars = c(value = "X", group = "Y"),
#'                tb.rows = 1:3) +
#'   expand_limits(x = c(0,3), y = c(-2, 6))
#'
stat_fmt_tb <- function(mapping = NULL, data = NULL, geom = "table",
                        tb.vars = NULL,
                        tb.rows = NULL,
                        digits = 3,
                        position = "identity",
                        na.rm = FALSE, show.legend = FALSE,
                        inherit.aes = TRUE,
                        ...) {
  ggplot2::layer(
    stat = StatFmtTb, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(tb.vars = tb.vars,
                  tb.rows = tb.rows,
                  digits = digits,
                  na.rm = na.rm,
                  ...)
  )
}

# Defined here to avoid a note in check --as-cran as the imports from 'dplyr'
# are not seen when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
fmt_tb_compute_group_fun <- function(data,
                                     scales,
                                     tb.vars = NULL,
                                     tb.rows = NULL,
                                     digits = 3) {
  stopifnot(is.list(data$label))

  for (tb.idx in seq_along(data$label)) {
    if (!is.data.frame(data$label[tb.idx][[1]])) {
      next()
    }

    num.cols <- sapply(data$label[tb.idx][[1]], is.numeric)
    data$label[tb.idx][[1]][num.cols] <- signif(data$label[tb.idx][[1]][num.cols], digits = digits)

    if(!is.null(tb.vars)) {
      data$label[tb.idx][[1]] <- dplyr::select(data$label[tb.idx][[1]], !!tb.vars)
    }

    if(!is.null(tb.rows)) {
      data$label[tb.idx][[1]] <- dplyr::slice(data$label[tb.idx][[1]], !!tb.rows)
    }

  }

  data
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFmtTb <-
  ggplot2::ggproto("StatFmtTb", ggplot2::Stat,
                   compute_group = fmt_tb_compute_group_fun,
                   required_aes = c("x", "y", "label")
)
