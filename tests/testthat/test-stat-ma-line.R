context("stat_ma_line")

set.seed(4321)
# generate artificial data
my.data <- data.frame(x = (1:100) / 10 + rnorm(n = 100),
                      y = (1:100) / 5 + rnorm(n = 100))

if (isNamespaceLoaded(name = "package:ggpmisc")) detach(package:ggpmisc, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggpp")) detach(package:ggpp, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggplot2")) detach(package:ggplot2, unload = TRUE)

test_that("ma_line_noload", {
  vdiffr::expect_doppelganger("stat_ma_line_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_ma_line()
  )
})

library(ggpmisc)

test_that("ma_formulas", {
  vdiffr::expect_doppelganger("stat_ma_line_formula_1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line()
  )

  vdiffr::expect_doppelganger("stat_ma_line_formula_x",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(formula = y ~ x)
  )

  vdiffr::expect_doppelganger("stat_ma_line_formula_x_Iy",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(formula = I(y) ~ x)
  )

  # vdiffr::expect_doppelganger("stat_ma_line_formula_x_Ix",
  #                             ggplot(my.data, aes(x, y)) +
  #                               geom_point() +
  #                               stat_ma_line(formula = y ~ I(x))
  # )

  vdiffr::expect_doppelganger("stat_ma_line_formula_y",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(formula = x ~ y)
  )

  vdiffr::expect_doppelganger("stat_ma_line_formula_y_Ix",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(formula = I(x) ~ y)
  )

  # vdiffr::expect_doppelganger("stat_ma_line_formula_y_Iy",
  #                             ggplot(my.data, aes(x, y)) +
  #                               geom_point() +
  #                               stat_ma_line(formula = x ~ I(y))
  # )

  vdiffr::expect_doppelganger("stat_ma_line_formula_x_mf",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = y ~ x, fm.values = TRUE)
  )

  vdiffr::expect_doppelganger("stat_ma_line_formula_x_nose",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = y ~ x, se = FALSE)
  )

})

test_that("ma_methods", {
  vdiffr::expect_doppelganger("stat_ma_line_ma",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(method = "MA")
  )

  vdiffr::expect_doppelganger("stat_ma_line_sma",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(method = "SMA")
  )

  vdiffr::expect_doppelganger("stat_ma_line_ols",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(method = "OLS")
  )

  vdiffr::expect_doppelganger("stat_ma_line_rma_int",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(method = "RMA",
                                             range.y = "interval",
                                             range.x = "interval")
  )

  vdiffr::expect_doppelganger("stat_ma_line_rma_rel",
                              ggplot(my.data, aes(x + 5, y + 10)) +
                                geom_point() +
                                stat_ma_line(method = "RMA",
                                             range.y = "relative",
                                             range.x = "relative")
  )

  # vdiffr::expect_doppelganger("stat_ma_line_missing_fm",
  #                             ggplot(my.data, aes(x + 5, y + 10)) +
  #                               geom_point() +
  #                               stat_ma_line(method = function(...) {list()},
  #                                            range.y = "relative",
  #                                            range.x = "relative")
  # )
  #
  # vdiffr::expect_doppelganger("stat_ma_line_NULL_fm",
  #                             ggplot(my.data, aes(x + 5, y + 10)) +
  #                               geom_point() +
  #                               stat_ma_line(method = function(...) {NULL})
  # )
  #
  # vdiffr::expect_doppelganger("stat_ma_line_NA_fm",
  #                             ggplot(my.data, aes(x + 5, y + 10)) +
  #                               geom_point() +
  #                               stat_ma_line(method = function(...) {NA})
  # )

})

