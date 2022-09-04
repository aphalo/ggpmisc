#' Colour and fill scales for ternary outcomes
#'
#' Manual scales for colour and fill aesthetics with defaults suitable for the
#' three way outcome from some statistical tests.
#'
#' @param ... other named arguments passed to \code{scale_colour_manual}.
#' @param name The name of the scale, used for the axis-label.
#' @param ns.colour,down.colour,up.colour,de.colour The colour definitions to
#'   use for each of the three possible outcomes.
#' @param na.colour colour definition used for NA.
#' @param values a set of aesthetic values to map data values to. The values
#'   will be matched in order (usually alphabetical) with the limits of the
#'   scale, or with breaks if provided. If this is a named vector, then the
#'   values will be matched based on the names instead. Data values that don't
#'   match will be given na.value. In addition the special values
#'   \code{"outcome:updown"}, \code{"outcome:de"} and \code{"outcome:both"} set
#'   predefined values, with \code{"outcome:both"} as default.
#' @param drop logical Should unused factor levels be omitted from the scale?
#'   The default, TRUE, uses the levels that appear in the data; FALSE uses all
#'   the levels in the factor.
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful,
#'   for example, to apply colour settings to the colour and fill aesthetics at
#'   the same time, via aesthetics = c("colour", "fill").
#'
#' @details These scales only alter the \code{breaks}, \code{values}, and
#'   \code{na.value} default arguments of \code{scale_colour_manual()} and
#'   \code{scale_fill_manual()}. Please, see documentation for
#'   \code{\link[ggplot2]{scale_manual}} for details.
#'
#' @note In 'ggplot2' (3.3.4, 3.3.5, 3.3.6) \code{scale_colour_manual()} and
#'   \code{scale_fill_manual()} do not obey \code{drop}, most likely due to a
#'   bug as this worked in version 3.3.3 and earlier. This results in spureous
#'   levels in the plot legend when using versions 3.3.4, 3.3.5, 3.3.6 of
#'   'ggplot2'.
#'
#' @export
#'
#' @family Functions for quadrant and volcano plots
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
                                 values = "outcome:updown",
                                 drop = TRUE,
                                 aesthetics = "colour") {
  if (length(values) == 1L & grepl("outcome:", values)) {
    if (values == "outcome:updown") {
      values <- c("down" = down.colour,
                  "uncertain" = ns.colour,
                  "up" = up.colour)
    } else if (values == "outcome:de") {
      values <- c("uncertain" = ns.colour,
                  "de" = de.colour)
    } else if (values == "outcome:both") {
      values <- c("down" = down.colour,
                  "uncertain" = ns.colour,
                  "up" = up.colour,
                  "de" = de.colour)
    } else {
      stop("Invalid input: values = ", values)
    }
  }
  # else values can be a vector that is valid input for scale_colour_manual()
  ggplot2::scale_colour_manual(...,
                               name = name,
                               values = values,
                               na.value = na.colour,
                               drop = drop,
                               aesthetics = aesthetics)
}


#' @rdname scale_colour_outcome
#'
#' @export
#'
scale_color_outcome <- scale_colour_outcome

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
                               values = "outcome:both",
                               drop = TRUE,
                               aesthetics = "fill") {
  if (length(values) == 1L & grepl("outcome:", values)) {
    if (values == "outcome:updown") {
      values <- c("down" = down.colour,
                  "uncertain" = ns.colour,
                  "up" = up.colour)
    } else if (values == "outcome:de") {
      values <- c("uncertain" = ns.colour,
                  "de" = de.colour)
    } else if (values == "outcome:both") {
      values <- c("down" = down.colour,
                  "uncertain" = ns.colour,
                  "up" = up.colour,
                  "de" = de.colour)
    } else {
      stop("Invalid input: values = ", values)
    }
  }
  # else values can be a vector that is valid input for scale_colour_manual()
  ggplot2::scale_fill_manual(...,
                             name = name,
                             values = values,
                             na.value = na.colour,
                             drop = drop,
                             aesthetics = aesthetics)
}

