#' Shape scale for ternary outcomes
#'
#' Manual scales for colour and fill aesthetics with defaults suitable for the
#' three way outcome from some statistical tests.
#'
#' @param ... other named arguments passed to \code{scale_manual}.
#' @param ns.shape,down.shape,up.shape,de.shape The shapes to use for each
#'   of the three possible outcomes.
#' @param na.shape Shape used for NA.
#'
#' @details These scales only alter the \code{values}, and
#'   \code{na.value} default arguments of
#'   \code{scale_shape_manual()}. Please, see
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
#' ggplot(my.df, aes(x, y, shape = outcome3)) +
#'   geom_point() +
#'   scale_shape_outcome() +
#'   theme_bw()
#'
#' ggplot(my.df, aes(x, y, shape = outcome2)) +
#'   geom_point(size = 2) +
#'   scale_shape_outcome() +
#'   theme_bw()
#'
#' ggplot(my.df, aes(x, y, shape = outcome3, fill = outcome2)) +
#'   geom_point() +
#'   scale_shape_outcome(name = "direction") +
#'   scale_fill_outcome(name = "significance") +
#'   theme_bw()
#'
scale_shape_outcome <- function(...,
                                ns.shape = "circle filled",
                                up.shape = "triangle filled",
                                down.shape = "triangle down filled",
                                de.shape = "square filled",
                                na.shape = "cross") {
  ggplot2::scale_shape_manual(...,
                              values = c("down" = down.shape,
                                         "uncertain" = ns.shape,
                                         "up" = up.shape,
                                         "de" = de.shape),
                              na.value = na.shape)
}
