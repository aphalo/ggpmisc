#' Inset graphical objects
#'
#' \code{geom_grob} adds a Grob as inset to the ggplot using syntax
#' similar to that of \code{\link[ggplot2]{geom_label}}.
#'
#' Note the "width" and "height" like of a text element are 0, so stacking
#' and dodging Grobs will not work by default, and axis limits are not
#' automatically expanded to include all inset Grobs. Obviously, Grobs do have
#' height and width, but they are in physical units, not data units. The amount of
#' space they occupy on the main plot is constant in data units.
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
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#'
#' @note This geom works only with tibbles as \code{data}, as it expects a list
#'   of Grobs (graphical objects) to be mapped to the \code{label} aesthetic. In
#'   the current version the following aesthetics are ignored within the inset
#'   Grob \code{size}, \code{colour}, and \code{alpha}. As \code{x} and \code{y}
#'   determine the position of the whole inset Grob, similarly to that of a text
#'   label, justification is interpreted as indicating the position of the Grob
#'   with respect to the $x$ and $y$ coordinates in the data, and \code{angle}
#'   is used to rotate the Grob as a whole. \strong{\code{annotate()} cannot be
#'   used with \code{geom = "grob"}}. Use \code{annotation_custom()} directly
#'   also for adding annotations.
#'
#' @references The idea of implementing a \code{geom_custom()} for grobs has
#'   been discussed as an issue at
#'   \url{https://github.com/tidyverse/ggplot2/issues/1399}.
#'
#' @export
#'
#' @examples
#' library(tibble)
#' df <- tibble(x = 2, y = 15, grob = list(grid::circleGrob(r = 0.2)))
#' ggplot(data = mtcars, aes(wt, mpg)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   geom_grob(data = df, aes(x, y, label = grob))
#'
geom_grob <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomGrob,
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
grob_draw_panel_fun <-
  function(data, panel_params, coord,
           na.rm = FALSE) {

    if (nrow(data) == 0) {
      return(grid::nullGrob())
    }

    if (!grid::is.grob(data$label[[1]])) {
      warning("Skipping as object mapped to 'label' is not a list of \"grob\" objects.")
      return(grid::nullGrob())
    }

    # should be called only once!
    data <- coord$transform(data, panel_params)

    user.grobs <- grid::gList()

    for (row.idx in 1:nrow(data)) {
      userGrob <- data$label[[row.idx]]

      userGrob$vp <-
        grid::viewport(x = unit(data$x[row.idx], "native"),
                       y = unit(data$y[row.idx], "native"),
                       width = unit(data$vp.width[row.idx], "npc"),
                       height = unit(data$vp.height[row.idx], "npc"),
                       just = c(data$hjust[row.idx], data$vjust[row.idx]),
                       angle = data$angle[row.idx],
                       name = paste("geom_grob.panel", data$PANEL[row.idx],
                                    "row", row.idx, sep = "."))

      # give unique name to each plot
      userGrob$name <- paste("inset.grob", row.idx, sep = ".")

      user.grobs[[row.idx]] <- userGrob
    }

    grid.name <- paste("geom_grob.panel",
                       data$PANEL[row.idx], sep = ".")

    grid::gTree(children = user.grobs, name = grid.name)
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomGrob <-
  ggproto("GeomGrob", Geom,
          required_aes = c("x", "y", "label"),

          default_aes = aes(
            colour = "black", angle = 0, hjust = 0.5,
            vjust = 0.5, alpha = NA, family = "", fontface = 1,
            vp.width = 1/5, vp.height = 1/5
          ),

          draw_panel = grob_draw_panel_fun,
          draw_key = function(...) {
            grid::nullGrob()
          }
  )

#' @rdname geom_grob
#' @export
#'
geom_grob_npc <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomGrobNpc,
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
grobnpc_draw_panel_fun <-
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

    grob_draw_panel_fun(data = data,
                        panel_params = panel_params,
                        coord = coord,
                        na.rm = na.rm)
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomGrobNpc <-
  ggproto("GeomGrobNpc", Geom,
          required_aes = c("x", "y", "label"),

          default_aes = aes(
            colour = "black", angle = 0, hjust = 0.5,
            vjust = 0.5, alpha = NA, family = "", fontface = 1,
            vp.width = 1/5, vp.height = 1/5
          ),

          draw_panel = grobnpc_draw_panel_fun,
          draw_key = function(...) {
            grid::nullGrob()
          }
  )
