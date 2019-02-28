#' Inset plots
#'
#' \code{geom_plot} adds ggplot objects as insets to the base ggplot, using syntax
#' similar to that of \code{\link[ggplot2]{geom_label}}.
#'
#' Note the "width" and "height" like of a text element are 0, so stacking and
#' dodging inset plots will not work by default, and axis limits are not
#' automatically expanded to include all inset plots. Obviously, plots do have
#' height and width, but they are physical units, not data units. The amount of
#' space they occupy on the main plot is not constant in data units of the base
#' plot: when you modify scale limits, inset plots stay the same size relative
#' to the physiscal size of the base plot.
#'
#' @section Alignment: You can modify inset plot alignment with the \code{vjust} and
#'   \code{hjust} aesthetics. These can either be a number between 0
#'   (right/bottom) and 1 (top/left) or a character ("left", "middle", "right",
#'   "bottom", "center", "top"). The \code{angle} aesthetics can be used to
#'   rotate the inset plots.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs
#'   to be set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'   the plot defaults.
#' @param stat The statistical transformation to use on the data for this layer,
#'   as a string.
#' @param na.rm If \code{FALSE} (the default), removes missing values with a
#'   warning.  If \code{TRUE} silently removes missing values.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#'
#' @note This geom works only with tibbles as \code{data}, as it expects a list
#'   of ggplots ("gg" objects) to be mapped to the \code{label} aesthetic.
#'   Aesthetics mappings in the inset plot are independent of those in the base
#'   plot. As \code{x} and \code{y}
#'   determine the position of the whole inset plot, similarly to that of a text
#'   label, justification is interpreted as indicating the position of the table
#'   with respect to the $x$ and $y$ coordinates in the data, and \code{angle}
#'   is used to rotate the plot as a whole. \strong{\code{annotate()} cannot be
#'   used with \code{geom = "plot"}}. Use \code{\link[ggplot2]{annotation_custom}} directly
#'   when adding inset plots as annotations.
#'
#' @references The idea of implementing a \code{geom_custom()} for grobs has been discussed as an
#'   issue at \url{https://github.com/tidyverse/ggplot2/issues/1399}.
#'
#' @export
#'
#' @examples
#' # inset plot with enlarged detail from a region of the main plot
#' library(tibble)
#' p <-
#'   ggplot(data = mtcars, aes(wt, mpg)) +
#'   geom_point()
#'
#' df <- tibble(x = 0.01, y = 0.01,
#'              plot = list(p +
#'                          coord_cartesian(xlim = c(3, 4),
#'                                          ylim = c(13, 16)) +
#'                          labs(x = NULL, y = NULL) +
#'                          theme_bw(10)))
#' p +
#'   geom_plot_npc(data = df, aes(x, y, label = plot),
#'                  hjust = 0, vjust = 0)
#'
geom_plot <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPlot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
gplot_draw_panel_fun <-
  function(data, panel_params, coord,
           na.rm = FALSE) {

    if (nrow(data) == 0) {
      return(grid::nullGrob())
    }

    if (!is.ggplot(data$label[[1]])) {
      warning("Skipping as object mapped to 'label' is not a list of \"gg\" or \"ggplot\" objects.")
      return(grid::nullGrob())
    }

    # should be called only once!
    data <- coord$transform(data, panel_params)

    plot.grobs <- grid::gList()

    for (row.idx in 1:nrow(data)) {
      plotGrob <-
        ggplotGrob(x = data$label[[row.idx]])

      plotGrob$vp <- grid::viewport(x = unit(data$x[row.idx], "native"),
                               y = unit(data$y[row.idx], "native"),
                               width = unit(data$vp.width[row.idx], "npc"),
                               height = unit(data$vp.height[row.idx], "npc"),
                               just = c(data$hjust[row.idx],
                                        data$vjust[row.idx]),
                               angle = data$angle[row.idx],
                               name = paste("geom_plot.panel",
                                            data$PANEL[row.idx], "row",
                                            row.idx, sep = "."))

      # give unique name to each plot
      plotGrob$name <- paste("inset.plot", row.idx, sep = ".")

      plot.grobs[[row.idx]] <- plotGrob
    }

    grid.name <- paste("geom_plot.panel",
                       data$PANEL[row.idx], sep = ".")

    grid::gTree(children = plot.grobs, name = grid.name)
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPlot <-
  ggproto("GeomPlot", Geom,
          required_aes = c("x", "y", "label"),

          default_aes = aes(
            colour = "black", angle = 0, hjust = 0.5,
            vjust = 0.5, alpha = NA, family = "", fontface = 1,
            vp.width = 1/2, vp.height = 1/2
          ),

          draw_panel = gplot_draw_panel_fun,
          draw_key = function(...) {
            grid::nullGrob()
          }
  )

#' @rdname geom_plot
#' @export
#'
geom_plot_npc <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPlotNpc,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
gplotnpc_draw_panel_fun <-
  function(data, panel_params, coord,
           na.rm = FALSE) {

    if (nrow(data) == 0) {
      return(grid::nullGrob())
    }

    if (max(data$x) > 1 || min(data$x) < 0) {
      warning("'x' outside valid range of [0..1] for npc units.")
      data <- data[data$x >= 0 & data$x <= 1, ]
    }

    if (max(data$y) > 1 || min(data$y) < 0) {
      warning("'y' outside valid range of [0..1] for npc units.")
      data <- data[data$y >= 0 & data$y <= 1, ]
    }

    ranges <- coord$backtransform_range(panel_params)

    data$x <- ranges$x[1] + data$x * (ranges$x[2] - ranges$x[1])
    data$y <- ranges$y[1] + data$y * (ranges$y[2] - ranges$y[1])

    gplot_draw_panel_fun(data = data,
                         panel_params = panel_params,
                         coord = coord,
                         na.rm = na.rm)
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPlotNpc <-
  ggproto("GeomPlotNpc", Geom,
          required_aes = c("x", "y", "label"),

          default_aes = aes(
            colour = "black", angle = 0, hjust = 0.5,
            vjust = 0.5, alpha = NA, family = "", fontface = 1,
            vp.width = 1/2, vp.height = 1/2
          ),

          draw_panel = gplotnpc_draw_panel_fun,
          draw_key = function(...) {
            grid::nullGrob()
          }
  )
