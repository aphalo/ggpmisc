#' Layer manipulation
#'
#' Delete or move one or more layers in a ggplot object.
#'
#' @param x an object of class \code{gg} to be operated upon.
#' @param match_type The name of the ggproto object class for the geom(s),
#'   position(s) or stat(s) to be removed.
#' @param idx integer vector If not \code{NULL}, the default, it overrides
#'   matching by class and applies the operation to the layers selected by the
#'   indexing into the list of layers.
#'
#' @return An edited copy of x.
#'
#' @references
#' \url{https://stackoverflow.com/questions/13407236/remove-a-layer-from-a-ggplot2-chart}
#'
#' @note These functions must be used with care as they select all layers matching
#'   the provided geom, position or stat ggproto object class. Layers added with
#'   a stat do use a geom, and vice versa.
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
#' move_layers(p, "GeomPoint", "top")
#' move_layers(p, "GeomPointrange", "bottom")
#' move_layers(p, "StatSummary", "bottom")
#' move_layers(p, "GeomPointrange", 1L)
#' show_layers(p, "GeomPoint")
#'
#' @export
#'
delete_layers <- function(x, match_type, idx = NULL) {
  edit_layers(x = x,
              match_type = match_type,
              idx = idx,
              action = "delete")
}

#' @rdname delete_layers
#'
#' @export
#'
move_layers <- function(x, match_type, position = "top", idx = NULL) {
  edit_layers(x = x,
              match_type = match_type,
              idx = idx,
              position = position,
              action = "move")
}

#' @rdname delete_layers
#'
#' @return An integer vector with indexes to matched layers.
#'
#' @export
#'
show_layers <- function(x, match_type, idx = NULL) {
  edit_layers(x = x,
              match_type = match_type,
              idx = idx,
              action = "show")
}

#' @rdname delete_layers
#'
#' @param action character One of "delete", "move", or "show".
#'
#' @keywords internal
#'
edit_layers <- function(x, match_type, idx = NULL, position = 0L, action) {
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
  if (sum(idx) > 0L) {
    if (action == "delete") {
      # Delete the layers.
      x$layers[idx] <- NULL
    } else if ((action == "move")) {
      # Move the layers.
      if (position == "top" | position == 0L) {
        x$layers <- c(x$layers[!idx], x$layers[idx])
      } else if (position == "bottom") {
        x$layers <- c(x$layers[idx], x$layers[!idx])
      } else if (is.integer(position) && position > 0L) {
        x$layers <- append(x$layers[!idx], x$layers,
                           ifelse(position <= sum(idx),
                                  position,
                                  sum(idx)))
      } else {
        stop("Position must be one of 'top' or 'bottom' or a positive integer.")
      }
    } else if (action == "show") {
      if (is.logical(idx)) {
        x <- which(idx)
      } else {
        x <- idx
      }
    }
  }
  x
}
