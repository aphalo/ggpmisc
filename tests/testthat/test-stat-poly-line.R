context("stat_poly_line")

library(tibble)

set.seed(4321)
# generate artificial data
x <- 1:100
y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
my.data <- data.frame(x,
                      y,
                      group = c("A", "B"),
                      y2 = y * c(0.5,2),
                      block = c("a", "a", "b", "b"),
                      wt = sqrt(x))
formula <- y ~ poly(x, 3, raw = TRUE)

if (isNamespaceLoaded(name = "package:ggpmisc")) detach(package:ggpmisc, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggpp")) detach(package:ggpp, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggplot2")) detach(package:ggplot2, unload = TRUE)

test_that("poly_line_noload", {
  vdiffr::expect_doppelganger("stat_poly_line_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_poly_line(formula = y ~ 1)
  )
})

library(ggpmisc)

test_that("poly_formulas", {
  vdiffr::expect_doppelganger("stat_poly_line_formula_1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = y ~ 1)
  )

  vdiffr::expect_doppelganger("stat_poly_line_formula_x",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = y ~ x)
  )

  vdiffr::expect_doppelganger("stat_poly_line_formula_x_Iy",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = I(y) ~ x)
  )

  vdiffr::expect_doppelganger("stat_poly_line_formula_x_Ix",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = y ~ I(x))
  )

  vdiffr::expect_doppelganger("stat_poly_line_formula_x_mf",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = y ~ x, fm.values = TRUE)
  )

  vdiffr::expect_doppelganger("stat_poly_line_formula_x_nose",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = y ~ x, se = FALSE)
  )

  vdiffr::expect_doppelganger("stat_poly_line_formula_xminus1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = y ~ x - 1)
  )

  vdiffr::expect_doppelganger("stat_poly_line_formula_xplus0",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = y ~ x + 0)
  )

  vdiffr::expect_doppelganger("stat_poly_line_formula_poly1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = y ~ poly(x, 1))
  )

  vdiffr::expect_doppelganger("stat_poly_line_formula_poly3",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = y ~ poly(x, 3))
  )

  vdiffr::expect_doppelganger("stat_poly_line_formula_x1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = x ~ 1)
  )

  vdiffr::expect_doppelganger("stat_poly_line_formula_y",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = x ~ y)
  )

  vdiffr::expect_doppelganger("stat_poly_line_formula_y_Ix",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = I(x) ~ y)
  )

  vdiffr::expect_doppelganger("stat_poly_line_formula_y_Iy",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = x ~ I(y))
  )

  vdiffr::expect_doppelganger("stat_poly_line_formula_yminus1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = x ~ y - 1)
  )

  vdiffr::expect_doppelganger("stat_poly_line_formula_yplus0",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = x ~ y + 0)
  )

  vdiffr::expect_doppelganger("stat_poly_line_formula_ypoly1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = x ~ poly(y, 1))
  )

})

test_that("poly_methods", {
  vdiffr::expect_doppelganger("stat_poly_line_lm_chr",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = y ~ x, method = "lm")
  )

  vdiffr::expect_doppelganger("stat_poly_line_lm_fun",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = y ~ x, method = stats::lm)
  )

  vdiffr::expect_doppelganger("stat_poly_line_rlm_chr",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = y ~ x, method = "rlm")
  )

  vdiffr::expect_doppelganger("stat_poly_line_rlm_fun",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(formula = y ~ x, method = MASS::rlm)
  )

})

