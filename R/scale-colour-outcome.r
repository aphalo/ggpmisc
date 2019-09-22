#' Colour and fill scales for ternary outcomes
#'
#' Manual scales for colour and fill aesthetics with defaults suitable for the
#' three way outcome from some statistical tests.
#'
#' @param ... other named arguments passed to \code{scale_manual}.
#' @param name The name of the scale, used for the axis-label.
#' @param ns.colour,down.colour,up.colour,de.colour The colour defintions to use for each
#'   of the three possible outcomes.
#' @param na.colour colour definition used for NA.
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful,
#'   for example, to apply colour settings to the colour and fill aesthetics at
#'   the same time, via aesthetics = c("colour", "fill").
#'
#' @details These scales only alter the \code{breaks}, \code{values}, and
#'   \code{na.value} default arguments of
#'   \code{scale_colour_manual()} and \code{scale_fill_manual()}. Please, see
#'   documentation for \code{\link[ggplot2]{scale_manual}} for details.
#'
#' @export
#'
#' @family scales for omics data
#'
#' @examples
#'
#' set.seed(12346)
#' outcome <- sample(c(-1, 0, +1), 50, replace = TRUE)
#' my.df <- data.frame(x = rnorm(50),
#'                     y = rnorm(50),
#'                     outcome2 = outcome2factor(outcome, n.levels = 2),
#'                     outcome3 = outcome2factor(outcome))
#'
#' ggplot(my.df, aes(x, y, colour = outcome3)) +
#'   geom_point() +
#'   scale_colour_outcome() +
#'   theme_bw()
#'
#' ggplot(my.df, aes(x, y, colour = outcome2)) +
#'   geom_point() +
#'   scale_colour_outcome() +
#'   theme_bw()
#'
#' ggplot(my.df, aes(x, y, fill = outcome3)) +
#'   geom_point(shape = 21) +
#'   scale_fill_outcome() +
#'   theme_bw()
#'
scale_colour_outcome <- function(...,
                                 name = "Outcome",
                                 ns.colour = "grey80",
                                 up.colour = "red",
                                 down.colour = "dodgerblue2",
                                 de.colour = "goldenrod",
                                 na.colour = "black",
                                 aesthetics = "colour") {
  ggplot2::scale_colour_manual(...,
                               name = name,
                               values = c("down" = down.colour,
                                          "uncertain" = ns.colour,
                                          "up" = up.colour,
                                          "de" = de.colour),
                               na.value = na.colour,
                               aesthetics = aesthetics)
}

#' @rdname scale_colour_outcome
#'
#' @export
#'
scale_fill_outcome <- function(...,
                               name = "Outcome",
                               ns.colour = "grey80",
                               up.colour = "red",
                               down.colour = "dodgerblue2",
                               de.colour = "goldenrod",
                               na.colour = "black",
                               aesthetics = "fill") {
  ggplot2::scale_fill_manual(...,
                             name = name,
                             values = c("down" = down.colour,
                                        "uncertain" = ns.colour,
                                        "up" = up.colour,
                                        "de" = de.colour),
                             na.value = na.colour,
                             aesthetics = aesthetics)
}

#' Convert numeric ternary outcomes into a factor
#'
#' @param x a numeric vector of -1, 0, and +1 values, indicating down
#'   regulation, uncertain response or upregulation.
#' @param n.levels numeric Number of levels to create.
#'
#' @details This function converts the numerically encoded values into a factor
#'   with the three levels \code{"down"}, \code{"uncertain"} and \code{"up"}, or
#'   into a factor with two levels \code{de} and \code{uncertain} as expected
#'   by scales \code{scale_colour_outcome()}, \code{scale_fill_outcome()} and
#'   \code{scale_shape_outcome()}.
#'
#' @note When \code{n.levels = 2} both -1 and +1 are merged to the same level
#'   with label \code{"de"}.
#'
#' @export
#'
#' @examples
#'
#' outcome2factor(c(-1, 1, 0, 1))
#'
#' @family scales for omics data
#'
outcome2factor <- function(x, n.levels = 3L) {
  stopifnot(all(unique(stats::na.omit(x)) %in% -1:1))
  if (n.levels == 3L) {
    fct.labels <- c( "up", "uncertain","down")
  } else if (n.levels == 2) {
    fct.labels <- c("de", "uncertain", "de")
  }
  factor(x, levels = c(+1, 0, -1), labels = fct.labels)
}


