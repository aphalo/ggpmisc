#' @title Print to console data received by the compute group function.
#'
#' @description \code{stat_debug} reports all distinct values in \code{group}
#'   and \code{PANEL}, and \code{nrow}, \code{ncol} and the names of the columns
#'   or variables, and the class of x and y for each group in a ggplot as passed
#'   to the \code{compute_group} function in the \code{ggproto} object.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_string}}. Only needs
#'   to be set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data
#' @param summary.fun A function used to print the \code{data} object received as
#'   input.
#' @param summary.fun.args A list.
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
#' @section Computed variables: \describe{ \item{x}{x at centre of range}
#'   \item{y}{y at centre of range} \item{nrow}{\code{nrow()} of \code{data}
#'   object} \item{ncol}{\code{ncol()} of \code{data} object}
#'   \item{colnames}{\code{colnames()} of \code{data} object}
#'   \item{colclasses}{\code{class()} of \code{x} and \code{y} columns in
#'   \code{data} object} \item{group}{all distinct values in group as passed in
#'   \code{data} object} \item{PANEL}{all distinct values in PANEL as passed in
#'   \code{data} object} }
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
stat_debug_group <-
  function(mapping = NULL, data = NULL, geom = "null",
           summary.fun = tibble::as_tibble, summary.fun.args = list(),
           position = "identity", na.rm = FALSE, show.legend = FALSE,
           inherit.aes = TRUE, ...) {
    ggplot2::layer(
      stat = StatDebugGroup, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm,
                    summary.fun = summary.fun,
                    summary.fun.args = summary.fun.args,
                    ...)
    )
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDebugGroup <-
  ggplot2::ggproto(
    "StatDebugGroup",
    ggplot2::Stat,
    compute_group = function(data, scales, summary.fun, summary.fun.args) {
      force(data)
      if (!is.null(summary.fun)) {
        data.summary <-  do.call(summary.fun, c(quote(data), summary.fun.args))
        print("Input 'data' to 'compute_group()':")
        print(data.summary)
      }
      my.diagnostic <-
        data.frame(x = mean(range(data$x)),
                   y = mean(range(data$y)),
                   nrow = nrow(data),
                   ncol = ncol(data),
                   colnames = paste(colnames(data), collapse = ", "),
                   colclasses = paste("x: ", class(data$x),
                                      "; y: ",  class(data$y),
                                      collapse = ", ", sep = ""),
                   group = paste(unique(data$group), sep = ", "),
                   PANEL = paste(unique(data$PANEL), sep = ", "))
      #                     print(my.diagnostic)
      my.diagnostic
    },
    default_aes = ggplot2::aes(label = paste("group: ", ..group.., "; ",
                                             "PANEL: ", ..PANEL.., "\n",
                                             "nrow: ", ..nrow.., "; ",
                                             "ncol: ", ..ncol.., "\n",
                                             "cols: ", ..colnames.., "\n",
                                             "classes: ", ..colclasses..,
                                             sep = "")
    ),
    required_aes = c("x", "y")
  )
