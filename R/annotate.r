#' Annotations supporting NPC
#'
#' A revised version of \code{annotate()} from package 'ggplot2' adding support
#' for \code{npcx} and \code{npcy} position aesthetics, allowing use of the
#' geometries defined in the current package such as \code{geom_text_npc()}.
#' When package 'ggpmisc' is loaded this definitions overrides that in package
#' 'ggplot2'.
#'
#' @param geom character Name of geom to use for annotation.
#' @param x,y,xmin,ymin,xmax,ymax,xend,yend,npcx,npcy	numeric Positioning
#'   aesthetics - you must specify at least one of these.
#' @param ...	Other named arguments passed on to \code{layer()}. These are often
#'   aesthetics, used to set an aesthetic to a fixed value, like colour = "red"
#'   or size = 3. They may also be parameters to the paired geom/stat.
#' @param na.rm	logical If \code{FALSE}, the default, missing values are removed
#'   with a warning. If TRUE, missing values are silently removed.
#'
#' @details Note that all position aesthetics are scaled (i.e., they will
#'   expand the limits of the plot so they are visible), but all other
#'   aesthetics are set. This means that layers created with this function will
#'   never affect the legend.
#'
#' @examples
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' p + annotate("text_npc", npcx = .9, npcy = .9, label = "Some text")
#' p + annotate("text_npc", npcx = "right", npcy = "top", label = "Some text")
#' p + annotate("label_npc", npcx = c(.1, .9), npcy = c(.1, .9),
#'              label = c("A", "B"))
#'
annotate <-
  function (geom, x = NULL, y = NULL, xmin = NULL, xmax = NULL,
            ymin = NULL, ymax = NULL, xend = NULL, yend = NULL,
            npcx = NULL, npcy = NULL, ...,
            na.rm = FALSE)
  {
    # functions from ggplot2, needed here but not exported
    compact <- function (x)
    {
      null <- vapply(x, is.null, logical(1))
      x[!null]
    }

    new_data_frame <- function (x = list(), n = NULL) {
      if (length(x) != 0 && is.null(names(x))) {
        rlang::abort("Elements must be named")
      }
      lengths <- vapply(x, length, integer(1))
      if (is.null(n)) {
        n <- if (length(x) == 0 || min(lengths) == 0)
          0
        else max(lengths)
      }
      for (i in seq_along(x)) {
        if (lengths[i] == n)
          next
        if (lengths[i] != 1) {
          rlang::abort("Elements must equal the number of rows or 1")
        }
        x[[i]] <- rep(x[[i]], n)
      }
      class(x) <- "data.frame"
      attr(x, "row.names") <- .set_row_names(n)
      x
    }

    position <- compact(list(x = x,
                             xmin = xmin,
                             xmax = xmax,
                             xend = xend,
                             y = y,
                             ymin = ymin,
                             ymax = ymax,
                             yend = yend,
                             npcx = npcx,
                             npcy = npcy))
    aesthetics <- c(position, list(...))
    lengths <- vapply(aesthetics, length, integer(1))
    n <- unique(lengths)
    if (length(n) > 1L) {
      n <- setdiff(n, 1L)
    }
    if (length(n) > 1L) {
      bad <- lengths != 1L
      details <- paste(names(aesthetics)[bad], " (", lengths[bad],
                       ")", sep = "", collapse = ", ")
      rlang::abort(glue::glue("Unequal parameter lengths: {details}"))
    }
    data <- new_data_frame(position, n = n)
    ggplot2::layer(geom = geom, params = list(na.rm = na.rm, ...), stat = StatIdentity,
                   position = PositionIdentity, data = data, mapping = aes_all(names(data)),
                   inherit.aes = FALSE, show.legend = FALSE)
  }
