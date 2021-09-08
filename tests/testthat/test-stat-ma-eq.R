context("stat_ma_eq")

set.seed(4321)
# generate artificial data
my.data <- data.frame(x = (1:100) / 10 + rnorm(n = 100),
                      y = (1:100) / 5 + rnorm(n = 100))

if (isNamespaceLoaded(name = "package:ggpmisc")) detach(package:ggpmisc, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggpp")) detach(package:ggpp, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggplot2")) detach(package:ggplot2, unload = TRUE)

test_that("ma_eq_noload", {
  vdiffr::expect_doppelganger("stat_ma_eq_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_ma_eq(formula = y ~ x, parse = TRUE,
                                             mapping =
                                               ggplot2::aes(label = paste(ggplot2::after_stat(eq.label),
                                                                          ggplot2::after_stat(rr.label),
                                                                          ggplot2::after_stat(p.value.label),
                                                                          ggplot2::after_stat(n.label),
                                                                 sep = "~~")))
  )
})

library(ggpmisc)

test_that("ma_formulas", {
  vdiffr::expect_doppelganger("stat_ma_eq_formula_x",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_eq(formula = y ~ x, parse = TRUE,
                                           mapping =
                                             aes(label = paste(after_stat(eq.label),
                                                               after_stat(rr.label),
                                                               after_stat(p.value.label),
                                                               after_stat(n.label),
                                                               sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_ma_eq_formula_poly1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_eq(formula = y ~ poly(x, 1, raw = TRUE),
                                           parse = TRUE,
                                           mapping =
                                             aes(label = paste(after_stat(eq.label),
                                                               after_stat(rr.label),
                                                               after_stat(p.value.label),
                                                               after_stat(n.label),
                                                               sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_ma_eq_formula_y",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_eq(formula = x ~ y,
                                           parse = TRUE,
                                           mapping =
                                             aes(label = paste(after_stat(eq.label),
                                                               after_stat(rr.label),
                                                               after_stat(p.value.label),
                                                               after_stat(n.label),
                                                               sep = "~~")))
  )

})

test_that("ma_methods", {
    vdiffr::expect_doppelganger("stat_ma_eq_ma",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_ma_eq(formula = y ~ x,
                                             method = "MA",
                                             parse = TRUE,
                                               mapping =
                                                 aes(label = paste(after_stat(eq.label),
                                                                   after_stat(rr.label),
                                                                   after_stat(p.value.label),
                                                                   after_stat(n.label),
                                                                   sep = "~~")))
    )

  vdiffr::expect_doppelganger("stat_ma_eq_sma",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_eq(formula = y ~ x,
                                           method = "SMA",
                                           parse = TRUE,
                                           mapping =
                                             aes(label = paste(after_stat(eq.label),
                                                               after_stat(rr.label),
                                                               after_stat(p.value.label),
                                                               after_stat(n.label),
                                                               sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_ma_eq_ols",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_eq(formula = y ~ x,
                                           method = "OLS",
                                           parse = TRUE,
                                           mapping =
                                             aes(label = paste(after_stat(eq.label),
                                                               after_stat(rr.label),
                                                               after_stat(p.value.label),
                                                               after_stat(n.label),
                                                               sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_ma_eq_rma_int",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_eq(formula = y ~ x,
                                           method = "RMA",
                                           range.y = "interval",
                                           range.x = "interval",
                                           parse = TRUE,
                                           mapping =
                                             aes(label = paste(after_stat(eq.label),
                                                               after_stat(rr.label),
                                                               after_stat(p.value.label),
                                                               after_stat(n.label),
                                                               sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_ma_eq_rma_rel",
                              ggplot(my.data, aes(x + 5, y + 20)) +
                                geom_point() +
                                stat_ma_eq(formula = y ~ x,
                                           method = "RMA",
                                           range.y = "relative",
                                           range.x = "relative",
                                           parse = TRUE,
                                           mapping =
                                             aes(label = paste(after_stat(eq.label),
                                                               after_stat(rr.label),
                                                               after_stat(p.value.label),
                                                               after_stat(n.label),
                                                               sep = "~~")))
  )

})

test_that("rounding_signif", {
  vdiffr::expect_doppelganger("stat_ma_eq_formula_x_round",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_eq(formula = y ~ x,
                                           parse = TRUE,
                                           rr.digits = 3,
                                           p.digits = 4,
                                           coef.digits = 6,
                                           mapping =
                                             aes(label = paste(after_stat(eq.label),
                                                               after_stat(rr.label),
                                                               after_stat(p.value.label),
                                                               after_stat(n.label),
                                                               sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_ma_eq_formula_x_round_less",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_eq(formula = y ~ x,
                                           parse = TRUE,
                                           rr.digits = 2,
                                           p.digits = 2,
                                           coef.digits = 3,
                                           mapping =
                                             aes(label = paste(after_stat(eq.label),
                                                               after_stat(rr.label),
                                                               after_stat(p.value.label),
                                                               after_stat(n.label),
                                                               sep = "~~")))
  )

})
