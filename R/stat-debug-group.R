#' @title Diagnosis statistics on data grouping.
#'
#' @description \code{stat_debug} reports \code{group}, \code{PANEL} and
#'   \code{nrow} for each group and panel of a ggplot.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_string}}. Only needs
#'   to be set at the layer level if you are overriding the plot defaults.
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
#' @param na.rm	a logical value indicating whether NA values should be stripped
#'   before the computation proceeds.
#'
#' @section Computed variables:
#' \describe{ \item{x}{x at centre of range}
#'   \item{y}{y at centre of range}
#'   \item{group}{group as passed in \code{data} object}
#'   \item{PANEL}{PANEL as passed in \code{data} object}
#'   \item{nrow}{nrow of \code{data} object}
#'   \item{ncol}{ncol of \code{data} object}
#'   \item{colnames}{\code{colnames()} of \code{data} object}
#'   }
#'
#' @examples
#' library(ggplot2)
#' my.df <- data.frame(x = rep(1:10, 2),
#'                     y = rep(c(1,2), c(10,10)),
#'                     group = rep(c("A","B"), c(10,10)))
#' ggplot(my.df, aes(x,y)) + geom_point() + stat_debug_group()
#' ggplot(my.df, aes(x,y, colour = group)) + geom_point() + stat_debug_group()
#' ggplot(my.df, aes(x,y)) + geom_point() + facet_wrap(~group) + stat_debug_group()
#'
#' @export
#' @family diagnosis functions
#'
stat_debug_group <- function(mapping = NULL, data = NULL, geom = "label",
                       position = "identity", na.rm = FALSE, show.legend = FALSE,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatDebugGroup, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  ...)
  )
}

#' @rdname gg2spectra-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDebugGroup <-
  ggplot2::ggproto("StatDebugGroup", ggplot2::Stat,
                   compute_group = function(data,
                                            scales) {
                     my.diagnostic <- data.frame(x = mean(range(data$x)),
                                                 y = mean(range(data$y)),
                                                 nrow = nrow(data),
                                                 ncol = ncol(data),
                                                 colnames = paste(colnames(data), collapse = ", "),
                                                 group = paste(unique(data$group), sep = ", "),
                                                 PANEL = paste(unique(data$PANEL), sep = ", "))
                     print(my.diagnostic)
                     my.diagnostic
                   },
                   default_aes = ggplot2::aes(label = paste("group: ", ..group.., "; ",
                                                            "PANEL: ", ..PANEL.., "\n",
                                                            "nrow: ", ..nrow.., "; ",
                                                            "ncol: ", ..ncol.., "\n",
                                                            "cols: ", ..colnames..,
                                                            sep = "")
                   ),
                   required_aes = c("x", "y")
  )
