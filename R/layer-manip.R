#' Layer manipulation
#'
#' Delete, move or append one or more layers in a ggplot.
#'
#' @param x an object of class \code{gg} to be operated upon.
#' @param match_type The name of the ggproto object class for the geom(s),
#'   position(s) or stat(s) matching that of the layers to be operated upon.
#' @param idx integer vector Index into the list of layers used to select the
#'   layers to be operated upon.
#'
#' @return An edited copy of \code{x} for \code{delete_layers},
#'   \code{append_layers} and \code{move_layers}. An integer vector of indexes
#'   giving the positions of the matching layers in the list of layers contained
#'   in \code{x} in the case of \code{which_layers}.
#'
#' @references
#' \url{https://stackoverflow.com/questions/13407236/remove-a-layer-from-a-ggplot2-chart}
#'
#' @details These functions must be used with care as they
#'   select all layers matching the provided geom, position or stat ggproto
#'   object class. Layers added with a stat do use a geom, and vice versa.
#'
#'   One and only one of \code{match_type} and \code{idx} must be passed
#'   a non-null argument.
#'
#'   In plots with several layers, it is possible that more than one layer
#'   matches the class name passed to \code{match_type}. It is also possible to
#'   pass a numeric vector with multiple indexes through parameter \code{idx}.
#'   In both cases multiple layers will be operated upon, but their relative
#'   positions will remain unchanged.
#'
#'   If a numeric vector with multiple position indexes is supplied as argument
#'   for \code{position}, the topmost position will be used if all indexes are
#'   positive, while if all indexes are negative, the deepest one will be used.
#'   If mixed positive and negative values are passed, the behaviour is
#'   undefined. As indexing in R starts at 1, passing 0 or \code{bottom} as
#'   argument for \code{position} puts the moved or appended layer(s) behind
#'   all other layers.
#'
#' @note The functions described here are not expected to be useful in everyday
#'   plotting as one can more easily change the order in which layers are added
#'   to a ggplot. However, if one uses high level methods or functions that
#'   automatically produce a full plot using 'ggplot2' internally, one may need
#'   to add, move or delete layers so as to profit from such canned methods and
#'   retain enough flexibility.
#'
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(
#'   gp = factor(rep(letters[1:3], each = 10)),
#'   y = rnorm(30)
#' )
#' p <- ggplot(df, aes(gp, y)) +
#'      geom_point() +
#'      stat_summary(fun.data = "mean_se", colour = "red")
#' p
#' delete_layers(p, "GeomPoint")
#' delete_layers(p, "StatSummary")
#' move_layers(p, "GeomPoint", position = "top")
#' move_layers(p, "GeomPointrange", position = "bottom")
#' move_layers(p, "StatSummary", position = "bottom")
#' move_layers(p, "GeomPointrange", position = 1L)
#' append_layers(p, geom_line(colour = "orange"), position = "bottom")
#' append_layers(p, geom_line(colour = "orange"), position = 1L)
#' show_layers(p, "GeomPoint")
#' which_layers(p, "GeomPoint")
#'
#' @export
#'
delete_layers <- function(x, match_type = NULL, idx = NULL) {
  edit_layers(x = x,
              match_type = match_type,
              idx = idx,
              action = "delete")
}

#' @rdname delete_layers
#'
#' @param position character or interger, the position of the layer immediately
#'    above of which to move or append the moved or appended layers.
#'
#' @export
#'
move_layers <- function(x, match_type = NULL, position = "top", idx = NULL) {
  edit_layers(x = x,
              match_type = match_type,
              position = position,
              idx = idx,
              action = "move")
}

#' @rdname delete_layers
#'
#' @export
#'
show_layers <- function(x, match_type = NULL, idx = NULL) {
  which.idxs <-   edit_layers(x = x,
                              match_type = match_type,
                              idx = idx,
                              action = "which")
  x$layers[which.idxs]
}

#' @rdname delete_layers
#'
#' @export
#'
which_layers <- function(x, match_type = NULL, idx = NULL) {
  edit_layers(x = x,
              match_type = match_type,
              idx = idx,
              action = "which")
}

#' @rdname delete_layers
#'
#' @param object a ggplot layer
#'
#' @export
#'
append_layers <- function(x, object, position = "top") {
  stopifnot(is.ggplot(x))
  stopifnot(is(object, "Layer") ||
              is.list(object) && all(sapply(object, is, class2 = "Layer")) ||
              is.list(object) && length(object) == 0L)
  z <- x + object
  if (length(z$layers) > length(x$layers) && position != "top") {
    z <- edit_layers(x = z,
                     match_type = NULL,
                     position = position,
                     idx = (length(x$layers) + 1):length(z$layers),
                     action = "move")
  }
  z
}

#' Edit plot layer
#'
#' Delete or move one or more layers in a ggplot object.
#'
#' @param x an object of class \code{gg} to be operated upon.
#' @param match_type The name of the ggproto object class for the geom(s),
#'   position(s) or stat(s) matching that of the layers to be operated upon.
#' @param idx integer vector Index(es) into the list of layers, used to select
#'   the layers to be operated upon.
#' @param action character One of "delete", "move", or "which".
#'
#' @keywords internal
#'
edit_layers <- function(x, match_type = NULL, position = 0L, idx = NULL, action) {
  stopifnot(is.ggplot(x))
  stopifnot(xor(is.null(match_type), is.null(idx) ))
  if (is.numeric(idx)) {
    # Convert into logical vector---equivalent to the inverse of which()
    idx <- seq_len(length(x$layers)) %in% as.integer(idx)
  }
  if (is.null(idx)) {
    # Check what to search for
    known_fields <- c("geom", "stat", "position")
    matched_field <-
      known_fields[sapply(known_fields, grepl, x = tolower(match_type))]
    if (length(matched_field) == 0L) {
      stop("Argument '", match_type, "' not in supported fields: ", known_fields, ".")
    }
    # Find layers that match the requested type.
    idx <- sapply(x$layers,
                  function(y) {
                    class(y[[matched_field]])[1] == match_type
                  })
  }
  if (action == "delete") {
    if (sum(idx) > 0L) {
      # Delete the layers.
      x$layers[idx] <- NULL
    }
  } else if ((action == "move")) {
    if (sum(idx) > 0L) {
      # Move the layers.
      if (position == "top") {
        x$layers <- c(x$layers[!idx], x$layers[idx])
      } else if (position == "bottom" | position == 0L) {
        x$layers <- c(x$layers[idx], x$layers[!idx])
      } else if (is.integer(position)) {
        # We use the topmost layer given by position if positive
        # and the lowermost if negative, this should be useful with multiple
        # matching layers.
        position <- abs(max(position))
        x$layers <- append(x$layers[!idx], x$layers[idx],
                           ifelse(position > length(x$layers[!idx]),
                                  length(x$layers[!idx]),
                                  position))
      } else {
        stop("Position must be one of 'top' or 'bottom' or a positive integer.")
      }
    }
  } else if (action == "which") {
      x <- which(idx)
  }
  x
}
