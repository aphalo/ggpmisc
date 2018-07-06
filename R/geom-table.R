#' Tables
#'
#' \code{geom_table} adds a textual table directly to the ggplot using syntax
#' similar to that of \code{\link[ggplot2]{geom_label}}.
#'
#' Note the "width" and "height" like of a text element are 0, so stacking
#' and dodging tables will not work by default, and axis limits are not
#' automatically expanded to include all tables. Obviously, tables do have
#' height and width, but they are physical units, not data units. The amount of
#' space they occupy on that plot is not constant in data units: when you resize
#' a plot, tables stay the same size, but the size of the axes changes.
#'
#' @section Alignment: You can modify table alignment with the \code{vjust} and
#'   \code{hjust} aesthetics. These can either be a number between 0
#'   (right/bottom) and 1 (top/left) or a character ("left", "middle", "right",
#'   "bottom", "center", "top").
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
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in ?plotmath.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param check_overlap If \code{TRUE}, text that overlaps previous text in the
#'   same layer will not be plotted.
#'
#' @note This geom works only with tibbles as \code{data}, as it expects a list
#'   of data frames or a list of tibbles to be mapped to the \code{label}
#'   aesthetic. In the current version the following aesthetics affect the text
#'   within the table \code{size}, \code{colour}, and \code{alpha}. The argument
#'   to parameter \code{parse} is simply passed forward to
#'   \code{gridExtra::ttheme_default()}. As \code{x} and \code{y} determine the
#'   position of the whole table, similarly to that of a text label,
#'   justification is interpreted as indicating the position of the table with
#'   respect to the $x$ and $y$ coordinates in the data, and \code{angle} is
#'   used to rotate the table as a whole. Other aesthetics, including
#'   \code{fill} are not yet implemented, neither are themes for table
#'   formatting. \strong{\code{annotate()} cannot be used with \code{geom =
#'   "table"}}. Use \code{geom_table} directly also for adding annotations.
#'
#' @references This geometry is inspired on answers to two questions in
#'   Stackoverflow. In contrast to these earlier examples, the current geom
#'   obeys the grammar of graphics, and attempts to be consistent with the
#'   behaviour of 'ggplot2' geometries.
#'   \url{https://stackoverflow.com/questions/12318120/adding-table-within-the-plotting-region-of-a-ggplot-in-r}
#'
#'
#'   \url{https://stackoverflow.com/questions/25554548/adding-sub-tables-on-each-panel-of-a-facet-ggplot-in-r?}
#'
#'
#' @seealso function \code{\link[gridExtra]{tableGrob}} as it is used to
#'   construct the table.
#'
#' @export
#'
geom_table <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       parse = FALSE,
                       check_overlap = FALSE,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE)
{
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

# Defined here to avoid a note in check --as-cran as the imports from 'broom'
# are not seen when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
gtb_draw_panel_fun <-
  function(data, panel_params, coord, parse = FALSE,
           na.rm = FALSE, check_overlap = FALSE) {

    if (nrow(data) == 0) {
      return(grid::nullGrob())
    }

    if (!is.data.frame(data$label[[1]])) {
      warning("Skipping as object mapped to 'label' is not a list of \"tibble\" or \"data.frame\" objects.")
      return(grid::nullGrob())
    }

    # should be called only once!
    data <- coord$transform(data, panel_params)

    tb.grobs <- grid::gList()

    for (row.idx in 1:nrow(data)) {
      gtb <-
        gridExtra::tableGrob(
          d = data$label[[row.idx]],
          theme = gridExtra::ttheme_default(base_size = data$size * .pt,
                                            base_colour = ggplot2::alpha(data$colour, data$alpha),
                                            parse = parse),
          rows = NULL
        )

      gtb$vp <- grid::viewport(x = unit(data$x[row.idx], "native"),
                               y = unit(data$y[row.idx], "native"),
                               width = sum(gtb$widths),
                               height = sum(gtb$heights),
                               just = c(data$hjust[row.idx], data$vjust[row.idx]),
                               angle = data$angle[row.idx],
                               name = paste("geom_table.panel", data$PANEL[row.idx], "row", row.idx, sep = "."))

      # give unique name to each table
      gtb$name <- paste("table", row.idx, sep = ".")

      tb.grobs[[row.idx]] <- gtb
    }

    grid.name <- paste("geom_table.panel",
                       data$PANEL[row.idx], sep = ".")

    grid::gTree(children = tb.grobs, name = grid.name)
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

          draw_panel = gtb_draw_panel_fun,
          draw_key = function(...) {grid::nullGrob()}
  )
