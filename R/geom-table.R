#' Table
#'
#' \code{geom_table} adds a textual table directly to the plot.
#'
#' Note the the "width" and "height" of a text element are 0, so stacking
#' and dodging text will not work by default, and axis limits are not
#' automatically expanded to include all text. Obviously, labels do have
#' height and width, but they are physical units, not data units. The amount of
#' space they occupy on that plot is not constant in data units: when you
#' resize a plot, labels stay the same size, but the size of the axes changes.
#'
#' @section Alignment:
#' You can modify table alignment with the \code{vjust} and \code{hjust}
#' aesthetics. These can either be a number between 0 (right/bottom) and
#' 1 (top/left) or a character ("left", "middle", "right", "bottom", "center",
#' "top"). There are two special alignments: "inward" and "outward".
#' Inward always aligns text towards the center, and outward aligns
#' it away from the center
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_string}}. Only needs
#'   to be set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'   the plot defaults.
#' @param stat The statistical transformation to use on the data for this layer,
#'   as a string.
#' @param na.rm If \code{FALSE} (the default), removes missing values with a
#'   warning.  If \code{TRUE} silently removes missing values.
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
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in ?plotmath.
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offsetting text from points, particularly on discrete scales.
#' @param check_overlap If \code{TRUE}, text that overlaps previous text in the
#'   same layer will not be plotted.
#'
#' @note This geom works only with tibbles as \code{data}, as it expects a
#'   whole data frame or tibble to be mapped to the \code{label} aesthetic.
#'   In the current version the following aesthetics affect the text within
#'   the table `size`, `colour`, `alpha`. The argument to parameter `parse` is
#'   simply passed forward to `gridExtra::ttheme_default()`. Other aesthetics
#'   are not yet implemented, neither are themes for table formatting.
#'
#' @references This geometry is inspired on the answer to a question in
#' Stackoverflow and a plot in the R graph gallery. In contrast to these
#' earlier examples, the current geom obeys the grammar of graphics, simplifying
#' the building of plots.
#' \url{https://stackoverflow.com/questions/12318120/adding-table-within-the-plotting-region-of-a-ggplot-in-r}
#' \url{http://www.r-graph-gallery.com/115-study-correlations-with-a-correlogram/}
#'
#' @export
geom_table <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      parse = FALSE,
                      nudge_x = 0,
                      nudge_y = 0,
                      check_overlap = FALSE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTable,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTable <-
  ggproto("GeomTable", Geom,
          required_aes = c("x", "y", "label"),

          default_aes = aes(
            colour = "black", size = 3.2, angle = 0, hjust = 0.5,
            vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
          ),

          draw_panel = function(data, panel_params, coord, parse = FALSE,
                                na.rm = FALSE, check_overlap = FALSE) {

            if (nrow(data) > 1) {
              warning("Grouping not supported in current version")
              return(grid::nullGrob())
            }

            lab <- data$label[[1]]

            data <- coord$transform(data, panel_params)
            if (is.character(data$vjust)) {
              data$vjust <- compute_just(data$vjust, data$y)
            }
            if (is.character(data$hjust)) {
              data$hjust <- compute_just(data$hjust, data$x)
            }

            gridExtra::tableGrob(
              lab,
              theme = gridExtra::ttheme_default(base_size = data$size * .pt,
                                                base_colour = ggplot2::alpha(data$colour, data$alpha),
                                                parse = parse),
              vp = grid::viewport(x = unit(data$x[1], "native"),
                                  y = unit(data$y[1], "native"),
                                  name = paste("geom_table.panel", data$PANEL[1], sep = ".")),
              rows = NULL
            )
          },
          draw_key = draw_key_text
  )
