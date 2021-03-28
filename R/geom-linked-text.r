#' @title Linked Text
#'
#' @description Text geoms are useful for labeling plots. `geom_linked_text()`
#'   adds text to the plot and for nudged positions links the original location
#'   to the nudeged text with a segment. `geom_linked_label()` draws a rectangle
#'   behind the text, making it easier to read on a crowded background.
#'
#' @details Note that when you resize a plot, text labels stay the same size,
#'   even though the size of the plot area changes. This happens because the
#'   "width" and "height" of a text element are 0. Obviously, text labels do
#'   have height and width, but they are physical units, not data units. For the
#'   same reason, stacking and dodging text will not work by default, and axis
#'   limits are not automatically expanded to include all text.
#'
#'   `geom_linked_text()` and `geom_linked_label()` add labels for each row in
#'   the data, even if coordinates x, y are set to single values in the call to
#'   `geom_linked_label()` or `geom_linked_text()`.
#'
#'   By default these geoms use `position_nudge_center()` which is backwards
#'   compatible with `position_nudge()` from 'ggplot2' but provides additional
#'   control on the direction of the nudging. In contrast to `position_nudge()`,
#'   `position_nudge_center()` and `position_nudge_line()` preserve the original
#'   coordinates.
#'
#' @section `geom_linked_label()`: Currently `geom_linked_label()` does not support
#'   the `angle` aesthetic and is considerably slower than `geom_linked_text()`. The
#'   `fill` aesthetic controls the background colour of the label.
#'
#' @section Alignment: You can modify text alignment with the `vjust` and
#'   `hjust` aesthetics. These can either be a number between 0 (right/bottom)
#'   and 1 (top/left) or a character (`"left"`, `"middle"`, `"right"`,
#'   `"bottom"`, `"center"`, `"top"`). There are two special alignments:
#'   `"inward"` and `"outward"`. Inward always aligns text towards the center,
#'   and outward aligns it away from the center.
#'
#' @param mapping Set of aesthetic mappings created by
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. If specified and
#'   \code{inherit.aes = TRUE} (the default), is combined with the default
#'   mapping at the top level of the plot. You only need to supply
#'   \code{mapping} if there isn't a mapping defined for the plot.
#' @param data A data frame. If specified, overrides the default data frame
#'   defined at the top level of the plot.
#' @param stat The statistical transformation to use on the data for this layer,
#'   as a string.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param parse If \code{TRUE}, the labels will be parsed into expressions and
#'   displayed as described in \code{?plotmath}.
#' @param na.rm If \code{FALSE} (the default), removes missing values with a
#'   warning.  If \code{TRUE} silently removes missing values.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param check_overlap If \code{TRUE}, text that overlaps previous text in the
#'   same layer will not be plotted. \code{check_overlap} takes place at draw
#'   time and in the order of the data, thus its action depends of the size at
#'   which the plot is drawn.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. There
#'   are three types of arguments you can use here:
#'
#'   \itemize{ \item Aesthetics: to set an aesthetic to a fixed value, like
#'   \code{colour = "red"} or \code{size = 3}. \item Other arguments to the
#'   layer, for example you override the default \code{stat} associated with the
#'   layer. \item Other arguments passed on to the stat. }
#' @param nudge_x,nudge_y Horizontal and vertical adjustments to nudge the
#'   starting position of each text label. The units for \code{nudge_x} and
#'   \code{nudge_y} are the same as for the data units on the x-axis and y-axis.
#' @param min.segment.length Skip drawing segments shorter than this, as unit or
#'   number. Defaults to 0.5. (Default unit is lines, but other units can be
#'   specified by passing \code{unit(x, "units")}).
#' @param arrow specification for arrow heads, as created by
#'   \code{\link[grid]{arrow}}
#'
#' @export
#'
#' @examples
#'
#' my.cars <- mtcars[c(TRUE, FALSE, FALSE, FALSE), ]
#' my.cars$name <- rownames(my.cars)
#' p <- ggplot(my.cars, aes(wt, mpg, label = name))
#'
#' # default behavior is as for geon_text()
#' p + geom_linked_text()
#' # Avoid overlaps
#' p + geom_linked_text(check_overlap = TRUE)
#' # Labels with background
#' p + geom_linked_label()
#' # Change size of the label
#' p + geom_linked_text(size = 2.5)
#'
#' # Use nudging
#' p +
#'   geom_point() +
#'   geom_linked_text(hjust = 0, nudge_x = 0.12) +
#'   expand_limits(x = 6.2)
#' p +
#'   geom_point() +
#'   geom_linked_text(hjust = 0, nudge_x = 0.12,
#'   arrow = arrow(length = grid::unit(1.5, "mm"))) +
#'   expand_limits(x = 6.2)
#' p +
#'   geom_point() +
#'   geom_linked_text(vjust = 0, nudge_y = 0.5)
#' p +
#'   geom_point() +
#'   geom_linked_text(hjust = 0, nudge_x = 0.1,
#'                    vjust = 0, nudge_y = 0.5)
#' p +
#'   geom_point() +
#'   geom_linked_text(angle = 90,
#'                    hjust = 0, nudge_y = 1,
#'                    arrow = arrow(length = grid::unit(1.5, "mm"))) +
#'   expand_limits(y = 40)
#'
#' # Add aesthetic mappings
#' p +
#'   geom_point() +
#'   geom_linked_text(aes(colour = factor(cyl)),
#'                    angle = 90,
#'                    hjust = 0, nudge_y = 1,
#'                    arrow = arrow(length = grid::unit(1.5, "mm"))) +
#'   scale_colour_discrete(l = 40) +
#'   expand_limits(y = 40)
#'
#' p + geom_linked_label(aes(fill = factor(cyl)),
#'                       colour = "white",
#'                       fontface = "bold") +
#'     expand_limits(x = c(2, 6))
#'
#' p + geom_linked_text(aes(size = wt)) +
#'     expand_limits(x = c(2, 6))
#' # Scale height of text, rather than sqrt(height)
#' p +
#'   geom_linked_text(aes(size = wt)) +
#'   scale_radius(range = c(3,6)) +
#'     expand_limits(x = c(2, 6))
#'
#' # You can display expressions by setting parse = TRUE.  The
#' # details of the display are described in ?plotmath, but note that
#' # geom_linked_text uses strings, not expressions.
#' p +
#'   geom_linked_text(
#'     aes(label = paste(wt, "^(", cyl, ")", sep = "")),
#'     parse = TRUE
#'   )
#'
#' # Add a text annotation
#' p +
#'   geom_linked_text() +
#'   annotate(
#'     "linked_text", label = "plot mpg vs. wt",
#'     x = 2, y = 15, size = 3, colour = "red"
#'   )  +
#'  expand_limits(x = c(1.5, 6))
#'
#' # Justification -------------------------------------------------------------
#' df <- data.frame(
#'   x = c(1, 1, 2, 2, 1.5),
#'   y = c(1, 2, 1, 2, 1.5),
#'   text = c("bottom-left", "bottom-right", "top-left", "top-right", "center")
#' )
#' ggplot(df, aes(x, y)) +
#'   geom_linked_text(aes(label = text))
#' ggplot(df, aes(x, y)) +
#'   geom_linked_text(aes(label = text), vjust = "inward", hjust = "inward")
#'
geom_linked_text <- function(mapping = NULL,
                             data = NULL,
                             stat = "identity",
                             position = "identity",
                             ...,
                             parse = FALSE,
                             nudge_x = 0,
                             nudge_y = 0,
                             min.segment.length = 0.5,
                             arrow = NULL,
                             check_overlap = FALSE,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      rlang::abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }

    position <- position_nudge_center(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLinkedText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      min.segment.length = min.segment.length,
      arrow = arrow,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
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
GeomLinkedText <-
  ggplot2::ggproto("GeomLinkedText", Geom,
                    required_aes = c("x", "y", "label"),

                    default_aes = aes(
                      colour = "black", size = 3.88, angle = 0, hjust = 0.5,
                      vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
                    ),

                    draw_panel = function(data, panel_params, coord, parse = FALSE,
                                          na.rm = FALSE, check_overlap = FALSE,
                                          min.segment.length = 0.5,
                                          arrow = NULL,
                                          nudge_x = 0,
                                          nudge_y = 0) {

                      add.links <- all(c("x_orig", "y_orig") %in% colnames(data))

                      data$label <- as.character(data$label)
                      data <- subset(data, !is.na(label) & label != "")
                      if (nrow(data) == 0L) {
                        return(nullGrob())
                      }

                      lab <- data$label
                      if (parse) {
                        lab <- parse_safe(lab)
                      }

                      data <- coord$transform(data, panel_params)
                      if (add.links) {
                        data_orig <- data.frame(x = data$x_orig, y = data$y_orig)
                        data_orig <- coord$transform(data_orig, panel_params)
                      }

                      if (is.character(data$vjust)) {
                        data$vjust <- compute_just(data$vjust, data$y)
                      }
                      if (is.character(data$hjust)) {
                        data$hjust <- compute_just(data$hjust, data$x)
                      }

                      if(add.links) {
                        # create the grobs
                        grobTree(
                          segmentsGrob(x0 = data$x,
                                       y0 = data$y,
                                       x1 = data_orig$x,
                                       y1 = data_orig$y,
                                       arrow = arrow,
                                       gp = grid::gpar(col = "black")),
                          textGrob(
                            lab,
                            data$x, data$y, default.units = "native",
                            hjust = data$hjust, vjust = data$vjust,
                            rot = data$angle,
                            gp = gpar(
                              col = alpha(data$colour, data$alpha),
                              fontsize = data$size * .pt,
                              fontfamily = data$family,
                              fontface = data$fontface,
                              lineheight = data$lineheight
                            ),
                            check.overlap = check_overlap
                          ))
                      } else {
                        textGrob(
                          lab,
                          data$x, data$y, default.units = "native",
                          hjust = data$hjust, vjust = data$vjust,
                          rot = data$angle,
                          gp = gpar(
                            col = alpha(data$colour, data$alpha),
                            fontsize = data$size * .pt,
                            fontfamily = data$family,
                            fontface = data$fontface,
                            lineheight = data$lineheight
                          ),
                          check.overlap = check_overlap
                        )
                      }
                    },

                   draw_key = draw_key_text
  )

#' @export
#' @rdname geom_linked_text
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#' @param label.size Size of label border, in mm.
#'
geom_linked_label <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       parse = FALSE,
                       nudge_x = 0,
                       nudge_y = 0,
                       min.segment.length = 0.5,
                       arrow = NULL,
                       label.padding = grid::unit(0.25, "lines"),
                       label.r = grid::unit(0.15, "lines"),
                       label.size = 0.25,
                       check_overlap = FALSE,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      rlang::abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }

    position <- position_nudge_center(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLinkedLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      min.segment.length = min.segment.length,
      arrow = arrow,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLinkedLabel <-
  ggplot2::ggproto("GeomLinkedLabel", Geom,
                     required_aes = c("x", "y", "label"),

                     default_aes = aes(
                       colour = "black", fill = "white", size = 3.88, angle = 0,
                       hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
                       lineheight = 1.2
                     ),

                     draw_panel = function(self, data, panel_params, coord, parse = FALSE,
                                           na.rm = FALSE,
                                           label.padding = unit(0.25, "lines"),
                                           label.r = unit(0.15, "lines"),
                                           label.size = 0.25,
                                           min.segment.length = 0.5,
                                           arrow = NULL) {

                       add.links <- all(c("x_orig", "y_orig") %in% colnames(data))

                       data$label <- as.character(data$label)
                       data <- subset(data, !is.na(label) & label != "")
                       if (nrow(data) == 0L) {
                         return(nullGrob())
                       }

                       lab <- data$label
                       if (parse) {
                         lab <- parse_safe(lab)
                       }

                       data <- coord$transform(data, panel_params)
                       if (add.links) {
                         data_orig <- data.frame(x = data$x_orig, y = data$y_orig)
                         data_orig <- coord$transform(data_orig, panel_params)
                       }

                       if (is.character(data$vjust)) {
                         data$vjust <- compute_just(data$vjust, data$y)
                       }
                       if (is.character(data$hjust)) {
                         data$hjust <- compute_just(data$hjust, data$x)
                       }

                       grobs <- lapply(1:nrow(data), function(i) {
                         row <- data[i, , drop = FALSE]
                         labelGrob(lab[i],
                                   x = unit(row$x, "native"),
                                   y = unit(row$y, "native"),
                                   just = c(row$hjust, row$vjust),
                                   padding = label.padding,
                                   r = label.r,
                                   text.gp = gpar(
                                     col = row$colour,
                                     fontsize = row$size * .pt,
                                     fontfamily = row$family,
                                     fontface = row$fontface,
                                     lineheight = row$lineheight
                                   ),
                                   rect.gp = gpar(
                                     col = if (isTRUE(all.equal(label.size, 0))) NA else row$colour,
                                     fill = alpha(row$fill, row$alpha),
                                     lwd = label.size * .pt
                                   )
                         )
                       })
                       class(grobs) <- "gList"

                       ggname("geom_linked_label", grobTree(children = grobs))
                     },

                     draw_key = draw_key_label
)

labelGrob <- function(label, x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc"),
                      just = "center", padding = grid::unit(0.25, "lines"),
                      r = grid::unit(0.1, "snpc"),
                      default.units = "npc", name = NULL,
                      text.gp = grid::gpar(), rect.gp = grid::gpar(fill = "white"), vp = NULL) {

  if (length(label) != 1) {
    rlang::abort("label must be of length 1")
  }

  if (!grid::is.unit(x))
    x <- grid::unit(x, default.units)
  if (!grid::is.unit(y))
    y <- grid::unit(y, default.units)

  grid::gTree(label = label, x = x, y = y, just = just, padding = padding, r = r,
        name = name, text.gp = text.gp, rect.gp = rect.gp, vp = vp, cl = "labelgrob")
}

#' @export
makeContent.labelgrob <- function(x) {
  hj <- grid::resolveHJust(x$just, NULL)
  vj <- grid::resolveVJust(x$just, NULL)

  t <- grid::textGrob(
    x$label,
    x$x + 2 * (0.5 - hj) * x$padding,
    x$y + 2 * (0.5 - vj) * x$padding,
    just = c(hj, vj),
    gp = x$text.gp,
    name = "text"
  )

  r <- grid::roundrectGrob(x$x, x$y, default.units = "native",
                     width = grid::grobWidth(t) + 2 * x$padding,
                     height = grid::grobHeight(t) + 2 * x$padding,
                     just = c(hj, vj),
                     r = x$r,
                     gp = x$rect.gp,
                     name = "box"
  )

  grid::setChildren(x, grid::gList(r, t))
}
