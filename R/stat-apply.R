#' Apply a function to x or y values
#'
#' \code{stat_apply_group} and \code{stat_apply_panel} apply functions to data.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs
#'   to be set
#'    at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'    the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points
#'    on this layer
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This can
#'   include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical value indicating whether NA values should be
#'   stripped before the computation proceeds.
#' @param .fun.x,.fun.y function to be applied or the name of the function to be
#'   applied as a character string. One and only one of these parameters should
#'   be passed a non-null argument.
#' @param .fun.x.args,.fun.y.args additional arguments to be passed to
#'   the function as a named list.
#'
#' @details The function to be applied is expected to be vectorized and to
#'   return a vector of the same length. The vector mapped to the x or y
#'   aesthetic is passed as the first positional argument to the call. The
#'   function must accept as first argument a vector or list that matches the
#'   data.
#'
#' @note This stat is at early stages of development and its interface may
#'   change at any time, or it may be even removed from the package without
#'   previous notice.
#'
#' @section Computed variables:
#' One x or y of is replaced by the vector returned by the applied function.
#' \describe{
#'   \item{x}{x-value as returned by \code{.fun.x}}
#'   \item{y}{y-value as returned by \code{.fun.y}}
#' }
#'
#' @references
#'
#' Answers question "R ggplot on-the-fly calculation by grouping variable" at
#' \url{https://stackoverflow.com/questions/51412522}.
#'
#' @examples
#'
#' library(ggplot2)
#' set.seed(123456)
#' df <- data.frame(X = rep(1:20,2),
#'                  Y = runif(40),
#'                  category = rep(c("A","B"), each = 20))
#' ggplot(df, aes(x = X, y = Y, colour = category)) +
#'   stat_apply_group(.fun.y = cumsum)
#' ggplot(df, aes(x = X, y = Y, colour = category)) +
#'   geom_point() +
#'   stat_apply_group(.fun.y = runmed, .fun.y.args = list(k = 5))
#' ggplot(df, aes(x = X, y = Y, colour = category)) +
#'   stat_apply_panel(.fun.y = function(x) {(x - min(x)) / (max(x) - min(x))})
#'
#' @rdname stat_apply
#'
#' @export
#' @family summary stats
#'
stat_apply_group <- function(mapping = NULL, data = NULL, geom = "line",
                             .fun.x = NULL, .fun.x.args = list(),
                             .fun.y = NULL, .fun.y.args = list(),
                             position = "identity", na.rm = FALSE, show.legend = FALSE,
                             inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatApplyGroup, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(.fun.x = .fun.x,
                  .fun.x.args = .fun.x.args,
                  .fun.y = .fun.y,
                  .fun.y.args = .fun.y.args,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname stat_apply
#'
#' @export
#'
stat_apply_panel <- function(mapping = NULL, data = NULL, geom = "line",
                             .fun.x = NULL, .fun.x.args = list(),
                             .fun.y = NULL, .fun.y.args = list(),
                             position = "identity", na.rm = FALSE, show.legend = FALSE,
                             inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatApplyPanel, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(.fun.x = .fun.x,
                  .fun.x.args = .fun.x.args,
                  .fun.y = .fun.y,
                  .fun.y.args = .fun.y.args,
                  na.rm = na.rm,
                  ...)
  )
}

# Define here to avoid a note in check as the import from 'dplyr' is not seen
# when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
stat_apply_fun <- function(data,
                           scales,
                           .fun.x, .fun.x.args,
                           .fun.y, .fun.y.args) {
  force(data)
  stopifnot(xor(is.null(.fun.x), is.null(.fun.y)))
  if (!is.null(.fun.x)) {
    args <- c(unname(data["x"]), .fun.x.args)
    new.data <- tibble::tibble(x = do.call(.fun.x, args = args),
                               y = data[["y"]])
  } else if (!is.null(.fun.y)) {
    args <- c(unname(data["y"]), .fun.y.args)
    new.data <- tibble::tibble(x = data[["x"]],
                               y = do.call(.fun.y, args = args))
  }
  data %>%
    dplyr::mutate(x = new.data[["x"]], y = new.data[["y"]])
}

#' \code{Stat*} Objects
#'
#' All \code{stat_*} functions (like \code{stat_bin}) return a layer that
#' contains a \code{Stat*} object (like \code{StatBin}). The \code{Stat*}
#' object is responsible for rendering the data in the plot.
#'
#' Each of the \code{Stat*} objects is a \code{\link[ggplot2]{ggproto}} object, descended
#' from the top-level \code{Stat}, and each implements various methods and
#' fields. To create a new type of Stat object, you typically will want to
#' implement one or more of the following:
#'
#' @name Stats
#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @seealso \code{\link[ggplot2]{ggplot2-ggproto}}
#' @keywords internal
StatApplyGroup <-
  ggplot2::ggproto("StatApplyGroup", ggplot2::Stat,
                   compute_group = stat_apply_fun,
                   required_aes = c("x", "y")
  )

#' \code{Stat*} Objects
#'
#' All \code{stat_*} functions (like \code{stat_bin}) return a layer that
#' contains a \code{Stat*} object (like \code{StatBin}). The \code{Stat*}
#' object is responsible for rendering the data in the plot.
#'
#' Each of the \code{Stat*} objects is a \code{\link[ggplot2]{ggproto}} object, descended
#' from the top-level \code{Stat}, and each implements various methods and
#' fields. To create a new type of Stat object, you typically will want to
#' implement one or more of the following:
#'
#' @name Stats
#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @seealso \code{\link[ggplot2]{ggplot2-ggproto}}
#' @keywords internal
StatApplyPanel <-
  ggplot2::ggproto("StatApplyPanel", ggplot2::Stat,
                   compute_panel = stat_apply_fun,
                   required_aes = c("x", "y")
  )
