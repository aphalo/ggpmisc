# Null geom ---------------------------------------------------------------

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomNull <-
  ggplot2::ggproto("GeomNull", ggplot2::Geom,
                   required_aes = c("x", "y"),
                   default_aes = ggplot2::aes(),
                   draw_key = function(...) {grid::nullGrob()},
                   draw_panel = function(data, panel_scales, coord,
                                         summary.fun,
                                         summary.fun.args) {
                     grid::nullGrob()
                   }
  )

#' Null geom
#'
#' The null geom can be used to silence graphic output from a stat, such as
#' stat_debug_group() and stat_debug_panel() defined in this same package. No
#' visible graphical output is returned. An invisible grid::grid_null() grob
#' is returned instead.
#'
#' @param mapping Set of aesthetic mappings created by \code{\link{aes}} or
#'   \code{\link{aes_}}. If specified and \code{inherit.aes = TRUE} (the
#'   default), is combined with the default mapping at the top level of the
#'   plot. You only need to supply \code{mapping} if there isn't a mapping
#'   defined for the plot.
#' @param data A data frame. If specified, overrides the default data frame
#'   defined at the top level of the plot.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param stat The statistical transformation to use on the data for this layer,
#'   as a string.
#' @param na.rm If \code{FALSE} (the default), removes missing values with a
#'   warning.  If \code{TRUE} silently removes missing values.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link{borders}}.
#' @param ... other arguments passed on to \code{\link{layer}}. There are three
#'   types of arguments you can use here:
#'
#'   \itemize{ \item Aesthetics: to set an aesthetic to a fixed value, like
#'   \code{color = "red"} or \code{size = 3}. \item Other arguments to the
#'   layer, for example you override the default \code{stat} associated with the
#'   layer. \item Other arguments passed on to the stat. }
#' @note This _geom_ is very unusual in that it does not produce visible graphic
#'   output. It only returns a \code{grid::grid_null()} grob (graphical object).
#' @export
#'
#' @note Although this geom accepts for consistency all the same parameters as
#'   normal geoms, these have no effect on the output, except for show.legend.
#'
geom_null <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE,
                      show.legend = FALSE,
                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomNull, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  ...)
  )
}

# Debug geom --------------------------------------------------------------

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomDebug <-
  ggplot2::ggproto("GeomDebug", ggplot2::Geom,
                   required_aes = c("x", "y"),
                   default_aes = ggplot2::aes(),
                   draw_key = function(...) {grid::nullGrob()},
                   draw_panel = function(data, panel_scales, coord,
                                         summary.fun,
                                         summary.fun.args) {
                     if (!is.null(summary.fun)) {
                       message("Input 'data' to 'geom_debug()':")
                       print(
                       do.call(summary.fun, c(quote(data), summary.fun.args))
                       )
                     }
                     grid::nullGrob()
                   }
  )

#' Debug, printing to console
#'
#' The debug geom is used to print to the console a summary of the data being
#' received by geoms as input \code{data} data frame.
#'
#' It can be useful when debugging the code of statistics or to learn how the
#' stata and geoms work in 'ggplot2' (>= 2.0.0).
#'
#' @param mapping Set of aesthetic mappings created by \code{\link{aes}} or
#'   \code{\link{aes_}}. If specified and \code{inherit.aes = TRUE} (the
#'   default), is combined with the default mapping at the top level of the
#'   plot. You only need to supply \code{mapping} if there isn't a mapping
#'   defined for the plot.
#' @param data A data frame. If specified, overrides the default data frame
#'   defined at the top level of the plot.
#' @param summary.fun A function used to print the \code{data} object received
#'   as input.
#' @param summary.fun.args A list of additional arguments to be passed to
#'   \code{summary.fun}.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param stat The statistical transformation to use on the data for this layer,
#'   as a string.
#' @param na.rm If \code{FALSE} (the default), removes missing values with a
#'   warning.  If \code{TRUE} silently removes missing values.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link{borders}}.
#' @param ... other arguments passed on to \code{\link{layer}}. There are three
#'   types of arguments you can use here:
#'
#'   \itemize{ \item Aesthetics: to set an aesthetic to a fixed value, like
#'   \code{color = "red"} or \code{size = 3}. \item Other arguments to the
#'   layer, for example you override the default \code{stat} associated with the
#'   layer. \item Other arguments passed on to the stat. }
#' @note This _geom_ is very unusual in that it does not produce visible graphic
#'   output. It only returns a \code{grid::grid_null()} grob (graphical object).
#' @export
#'
geom_debug <- function(mapping = NULL, data = NULL, stat = "identity",
                       summary.fun = tibble::as_data_frame,
                       summary.fun.args = list(),
                       position = "identity", na.rm = FALSE,
                       show.legend = FALSE,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomDebug, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  summary.fun = summary.fun,
                  summary.fun.args = summary.fun.args,
                  ...)
  )
}

