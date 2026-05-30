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

  # Fails with an ERROR!
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

  # Fails with an ERROR!
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

test_that("ma_range", {
  vdiffr::expect_doppelganger("stat_ma_line_formula_x_fullrange_true",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(fullrange = TRUE) +
                                expand_limits(y = c(-10, 30),
                                              x = c(-3, 15))
  )

  vdiffr::expect_doppelganger("stat_ma_line_formula_x_fullrange_false",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(fullrange = FALSE) +
                                expand_limits(y = c(-10, 30),
                                              x = c(-3, 15))
  )

  vdiffr::expect_doppelganger("stat_ma_line_formula_y_fullrange_true",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(formula = y ~ x, fullrange = TRUE) +
                                expand_limits(y = c(-10, 30),
                                              x = c(-3, 15))
  )

  vdiffr::expect_doppelganger("stat_ma_line_formula_y_fullrange_false",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(formula = y ~ x, fullrange = FALSE) +
                                expand_limits(y = c(-10, 30),
                                              x = c(-3, 15))
  )

  vdiffr::expect_doppelganger("stat_ma_line_formula_x_limit_to_none",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(limit.to = "none") +
                                expand_limits(y = c(-10, 30),
                                              x = c(-3, 15))
  )

  vdiffr::expect_doppelganger("stat_ma_line_formula_y_limit_to_x",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(limit.to = "x") +
                                expand_limits(y = c(-10, 30),
                                              x = c(-3, 15))
  )

  vdiffr::expect_doppelganger("stat_ma_line_formula_y_limit_to_y",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(limit.to = "y") +
                                expand_limits(y = c(-10, 30),
                                              x = c(-3, 15))
  )

  vdiffr::expect_doppelganger("stat_ma_line_formula_y_limit_to_xy",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(limit.to = "xy") +
                                expand_limits(y = c(-10, 30),
                                              x = c(-3, 15))
  )

  vdiffr::expect_doppelganger("stat_ma_line_formula_y_limit_to_m2_13",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(limit.to = -2:13) +
                                expand_limits(y = c(-10, 30),
                                              x = c(-3, 15))
  )

  vdiffr::expect_doppelganger("stat_ma_line_formula_y_limit_to_13",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_ma_line(limit.to = 13,
                                               geom = "pointrange") +
                                expand_limits(y = c(-10, 30),
                                              x = c(-3, 15))
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

  vdiffr::expect_doppelganger("stat_ma_line_missing_fm",
                              ggplot(my.data, aes(x + 5, y + 10)) +
                                geom_point() +
                                stat_ma_line(method = function(...) {list()})
  )

  vdiffr::expect_doppelganger("stat_ma_line_NULL_fm",
                              ggplot(my.data, aes(x + 5, y + 10)) +
                                geom_point() +
                                stat_ma_line(method = function(...) {NULL})
  )

  vdiffr::expect_doppelganger("stat_ma_line_NA_fm",
                              ggplot(my.data, aes(x + 5, y + 10)) +
                                geom_point() +
                                stat_ma_line(method = function(...) {NA})
  )

})

