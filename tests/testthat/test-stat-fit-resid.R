context("residuals")

set.seed(4321)
# generate artificial data
my.data <- data.frame(x = (1:25) / 10 + rnorm(n = 25),
                      y = (1:25) / 5 + rnorm(n = 25))

if (isNamespaceLoaded(name = "package:ggpmisc")) detach(package:ggpmisc, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggpp")) detach(package:ggpp, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggplot2")) detach(package:ggplot2, unload = TRUE)

test_that("stat_fit_deviations works with imports not attached", {
  vdiffr::expect_doppelganger("fit_deviations_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                geom_point() +
                                ggpmisc::stat_fit_deviations()
  )
  # vdiffr::expect_doppelganger("fit_deviations_x_noload",
  #                             ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
  #                               geom_point() +
  #                               ggpmisc::stat_fit_deviations(orientation = "x")
  # )
  # vdiffr::expect_doppelganger("fit_deviations_y_noload",
  #                             ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
  #                               geom_point() +
  #                               ggpmisc::stat_fit_deviations(orientation = "y")
  # )
  vdiffr::expect_doppelganger("fit_deviations_x_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                geom_point() +
                                ggpmisc::stat_fit_deviations(formula = y ~ x)
  )
  vdiffr::expect_doppelganger("fit_deviations_y_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                geom_point() +
                                ggpmisc::stat_fit_deviations(formula = x ~ y)
  )
})

test_that("stat_fit_residuals works with imports not attached", {
  vdiffr::expect_doppelganger("fit_residuals_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                geom_point() +
                                ggpmisc::stat_fit_residuals()
  )
  # vdiffr::expect_doppelganger("fit_residuals_x_noload",
  #                             ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
  #                               geom_point() +
  #                               ggpmisc::stat_fit_residuals(orientation = "x")
  # )
  # vdiffr::expect_doppelganger("fit_residuals_y_noload",
  #                             ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
  #                               geom_point() +
  #                               ggpmisc::stat_fit_residuals(orientation = "y")
  # )
  vdiffr::expect_doppelganger("fit_residuals_x_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                geom_point() +
                                ggpmisc::stat_fit_residuals(formula = y ~ x)
  )
  vdiffr::expect_doppelganger("fit_residuals_y_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                geom_point() +
                                ggpmisc::stat_fit_residuals(formula = x ~ y)
  )
})

test_that("stat_fit_residuals works with imports not attached", {
  vdiffr::expect_doppelganger("fit_fitted_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                geom_point() +
                                ggpmisc::stat_fit_fitted(geom = "line")
  )
  # vdiffr::expect_doppelganger("fit_fitted_x_noload",
  #                             ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
  #                               geom_point() +
  #                               ggpmisc::stat_fit_fitted(orientation = "x")
  # )
  # vdiffr::expect_doppelganger("fit_fitted_y_noload",
  #                             ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
  #                               geom_point() +
  #                               ggpmisc::stat_fit_fitted(orientation = "y")
  # )
  vdiffr::expect_doppelganger("fit_fitted_x_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                geom_point() +
                                ggpmisc::stat_fit_fitted(formula = y ~ x,
                                                         geom = "line")
  )
  vdiffr::expect_doppelganger("fit_fitted_y_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                geom_point() +
                                ggpmisc::stat_fit_fitted(formula = x ~ y,
                                                         geom = "line")
  )
})

# after attaching packages

library(ggpmisc)

test_that("stat_fit_deviations works with imports attached", {
  vdiffr::expect_doppelganger("fit_deviations",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_deviations()
  )
  # vdiffr::expect_doppelganger("fit_deviations_x",
  #                             ggplot(my.data, aes(x, y)) +
  #                               geom_point() +
  #                               stat_fit_deviations(orientation = "x")
  # )
  # vdiffr::expect_doppelganger("fit_deviations_y",
  #                             ggplot(my.data, aes(x, y)) +
  #                               geom_point() +
  #                               stat_fit_deviations(orientation = "y")
  # )
  vdiffr::expect_doppelganger("fit_deviations_formula_x",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_deviations(formula = y ~ x)
  )
  vdiffr::expect_doppelganger("fit_deviations_formula_y",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_deviations(formula = x ~ y)
  )
})

test_that("stat_fit_residuals works with imports attached", {
  vdiffr::expect_doppelganger("fit_residuals",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_residuals()
  )
  # vdiffr::expect_doppelganger("fit_residuals_x",
  #                             ggplot(my.data, aes(x, y)) +
  #                               geom_point() +
  #                               stat_fit_residuals(orientation = "x")
  # )
  # vdiffr::expect_doppelganger("fit_residuals_y",
  #                             ggplot(my.data, aes(x, y)) +
  #                               geom_point() +
  #                               stat_fit_residuals(orientation = "y")
  # )
  vdiffr::expect_doppelganger("fit_residuals_formula_x",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_residuals(formula = y ~ x)
  )
  vdiffr::expect_doppelganger("fit_residuals_formula_y",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_residuals(formula = x ~ y)
  )
})

test_that("stat_fit_residuals works with imports attached", {
  vdiffr::expect_doppelganger("fit_fitted",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_fitted(geom = "line")
  )
  # vdiffr::expect_doppelganger("fit_fitted_x",
  #                             ggplot(my.data, aes(x, y)) +
  #                               geom_point() +
  #                               stat_fit_fitted(orientation = "x")
  # )
  # vdiffr::expect_doppelganger("fit_fitted_y",
  #                             ggplot(my.data, aes(x, y)) +
  #                               geom_point() +
  #                               stat_fit_fitted(orientation = "y")
  # )
  vdiffr::expect_doppelganger("fit_fitted_x",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_fitted(formula = y ~ x,
                                                geom = "line")
  )
  vdiffr::expect_doppelganger("fit_fitted_y",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_fitted(formula = x ~ y,
                                                geom = "line")
  )
})

