#' Predefined label mappings
#'
#' Mappings of model-fit-derived text or expressions to the label aesthetic.
#'
#' @rdname label.mappings
#'
#' @export
#'
#' @examples
#' # generate artificial data
#' set.seed(4321)
#' x <- 1:100
#' y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
#' my.data <- data.frame(x = x,
#'                       y = y * 1e-5,
#'                       group = c("A", "B"),
#'                       y2 = y * 1e-5 + c(2, 0))
#'
#' # give a name to a formula
#' formula <- y ~ poly(x, 3, raw = TRUE)
#'
#' # using predefined label mappings
#' ggplot(my.data, aes(x, y2, colour = group)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(rr.n_label, formula = formula)
#'
#' ggplot(my.data, aes(x, y2, colour = group)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(eq.adj.rr.n_label, formula = formula, size = 3)
#'
#' ggplot(my.data, aes(x, y2, colour = group)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(F.p_label, formula = formula)
#'
#' ggplot(my.data, aes(x, y)) +
#'   stat_quant_band(formula = formula) +
#'   stat_quant_eq(eq.n_label, formula = formula, size = 3) +
#'   geom_point()
#'
#' ggplot(my.data, aes(x, y)) +
#'   stat_ma_line() +
#'   stat_ma_eq(eq.n_label) +
#'   geom_point()
#'
label.sep <-
  if (getOption("OutDec") == ".") {
    "*\", \"*"
  } else {
    "*\"; \"*"
  }

#' @rdname label.mappings
#'
#' @export
#'
eq_label <- ggplot2::aes(label =
                           ggplot2::after_stat(eq.label))

#' @rdname label.mappings
#'
#' @export
#'
eq.n_label <- ggplot2::aes(label =
                               paste(ggplot2::after_stat(eq.label),
                                     ggplot2::after_stat(n.label),
                                     sep = label.sep))

#' @rdname label.mappings
#'
#' @export
#'
rr.n_label <- ggplot2::aes(label =
                             paste(ggplot2::after_stat(rr.label),
                                   ggplot2::after_stat(n.label),
                                   sep = label.sep))

#' @rdname label.mappings
#'
#' @export
#'
rr.p_label <- ggplot2::aes(label =
                             paste(ggplot2::after_stat(rr.label),
                                   ggplot2::after_stat(p.value.label),
                                   sep = label.sep))

#' @rdname label.mappings
#'
#' @export
#'
adj.rr.n_label <- ggplot2::aes(label =
                                 paste(ggplot2::after_stat(adj.rr.label),
                                     ggplot2::after_stat(n.label),
                                     sep = label.sep))

#' @rdname label.mappings
#'
#' @export
#'
adj.rr.p_label <- ggplot2::aes(label =
                                 paste(ggplot2::after_stat(adj.rr.label),
                                       ggplot2::after_stat(p.value.label),
                                       sep = label.sep))

#' @rdname label.mappings
#'
#' @export
#'
eq.rr.n_label <- ggplot2::aes(label =
                                paste(ggplot2::after_stat(eq.label),
                                    ggplot2::after_stat(rr.label),
                                    ggplot2::after_stat(n.label),
                                    sep = label.sep))

#' @rdname label.mappings
#'
#' @export
#'
eq.adj.rr.n_label <- ggplot2::aes(label =
                                    paste(ggplot2::after_stat(eq.label),
                                        ggplot2::after_stat(adj.rr.label),
                                        ggplot2::after_stat(n.label),
                                        sep = label.sep))

#' @rdname label.mappings
#'
#' @export
#'
eq.p.n_label <- ggplot2::aes(label =
                                    paste(ggplot2::after_stat(eq.label),
                                          ggplot2::after_stat(adj.rr.label),
                                          ggplot2::after_stat(n.label),
                                          sep = label.sep))

#' @rdname label.mappings
#'
#' @export
#'
F.p.n_label <- ggplot2::aes(label =
                               paste(ggplot2::after_stat(f.value.label),
                                     ggplot2::after_stat(p.value.label),
                                     ggplot2::after_stat(n.label),
                                     sep = label.sep))

#' @rdname label.mappings
#'
#' @export
#'
F.p_label <- ggplot2::aes(label =
                              paste(ggplot2::after_stat(f.value.label),
                                    ggplot2::after_stat(p.value.label),
                                    sep = label.sep))

