context("residuals")

set.seed(4321)
# generate artificial data
my.data <- data.frame(x = (1:25) / 10 + rnorm(n = 25),
                      y = (1:25) / 5 + rnorm(n = 25))
my.data$y.otlr <- my.data$y
outliers <- sample(1:25, 3)
my.data$y.otlr[outliers] <- outliers / 5 + rnorm(length(outliers), sd = 3)
my.data$wght <- 1
my.data$wght[outliers] <- 0

if (isNamespaceLoaded(name = "package:ggpmisc")) detach(package:ggpmisc, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggpp")) detach(package:ggpp, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggplot2")) detach(package:ggplot2, unload = TRUE)

test_that("stat_fit_deviations works with imports not attached", {
  vdiffr::expect_doppelganger("fit_deviations_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                geom_point() +
                                ggpmisc::stat_fit_deviations()
  )
  vdiffr::expect_doppelganger("fit_deviations_orx_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                geom_point() +
                                ggpmisc::stat_fit_deviations(orientation = "x")
  )
  vdiffr::expect_doppelganger("fit_deviations_ory_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                geom_point() +
                                ggpmisc::stat_fit_deviations(orientation = "y")
  )
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
  vdiffr::expect_doppelganger("fit_residuals_orx_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                geom_point() +
                                ggpmisc::stat_fit_residuals(orientation = "x")
  )
  vdiffr::expect_doppelganger("fit_residuals_ory_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                geom_point() +
                                ggpmisc::stat_fit_residuals(orientation = "y")
  )
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
  vdiffr::expect_doppelganger("fit_fitted_orx_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                geom_point() +
                                ggpmisc::stat_fit_fitted(orientation = "x",
                                                         geom = "line")
  )
  vdiffr::expect_doppelganger("fit_fitted_ory_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                geom_point() +
                                ggpmisc::stat_fit_fitted(orientation = "y",
                                                         geom = "line")
  )
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


# stat_fit_deviations -----------------------------------------------------

test_that("stat_fit_deviations works with imports attached", {
  vdiffr::expect_doppelganger("fit_deviations",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_deviations()
  )
  vdiffr::expect_doppelganger("fit_deviations_orx",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_deviations(orientation = "x")
  )
  vdiffr::expect_doppelganger("fit_deviations_ory",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_deviations(orientation = "y")
  )
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

test_that("stat_fit_deviations works with different methods", {
  vdiffr::expect_doppelganger("fit_deviations_lm",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(method = "lm") +
                                stat_fit_deviations(method = "lm")
  )
  vdiffr::expect_doppelganger("fit_deviations_lm_prior_weights",
                              ggplot(my.data, aes(x, y.otlr, weight = wght)) +
                                geom_point() +
                                stat_poly_line(method = "lm") +
                                stat_fit_deviations(aes(colour = after_stat(weights)),
                                                    method = "lm")
  )
  vdiffr::expect_doppelganger("fit_deviations_lm_weights",
                              ggplot(my.data, aes(x, y.otlr, weight = wght)) +
                                geom_point() +
                                stat_poly_line(method = "lm") +
                                stat_fit_deviations(aes(colour = after_stat(posterior.weights)),
                                                    method = "lm")
  )
  vdiffr::expect_doppelganger("fit_deviations_rlm",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(method = "rlm") +
                                stat_fit_deviations(method = "rlm")
  )
  vdiffr::expect_doppelganger("fit_deviations_rlm_weights",
                              ggplot(my.data, aes(x, y.otlr)) +
                                geom_point() +
                                stat_poly_line(method = "rlm") +
                                stat_fit_deviations(aes(colour = after_stat(posterior.weights)),
                                                        method = "rlm")
  )
  vdiffr::expect_doppelganger("fit_deviations_lts",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(method = "lts") +
                                stat_fit_deviations(method = "lts")
  )
  vdiffr::expect_doppelganger("fit_deviations_lts_weights",
                              ggplot(my.data, aes(x, y.otlr)) +
                                geom_point() +
                                stat_poly_line(method = "lts") +
                                stat_fit_deviations(aes(colour = after_stat(posterior.weights)),
                                                    method = "lts")
  )
  vdiffr::expect_doppelganger("fit_deviations_gls",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(method = "gls") +
                                stat_fit_deviations(method = "gls")
  )
  vdiffr::expect_doppelganger("fit_deviations_gls_weights",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(method = "gls") +
                                stat_fit_deviations(aes(colour = after_stat(posterior.weights)),
                                                    method = "gls",
                                                    show.legend = TRUE)
  )
  vdiffr::expect_doppelganger("fit_deviations_gls_weights_vPower",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(method = "gls",
                                               method.args = list(weights = nlme::varPower())) +
                                stat_fit_deviations(aes(colour = after_stat(posterior.weights)),
                                                    method = "gls",
                                                    method.args = list(weights = nlme::varPower()))
  )
  vdiffr::expect_doppelganger("fit_deviations_gls_prior_weights_vPower",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_line(method = "gls",
                                               method.args = list(weights = nlme::varPower())) +
                                stat_fit_deviations(aes(colour = after_stat(weights)),
                                                    method = "gls",
                                                    method.args = list(weights = nlme::varPower()))
  )
  vdiffr::expect_doppelganger("fit_deviations_rq",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_quant_line(method = "rq",
                                                quantiles = 0.5,
                                                se = FALSE) +
                                stat_fit_deviations(method = "rq")
  )
  vdiffr::expect_doppelganger("fit_deviations_rq_weights",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_quant_line(method = "rq",
                                                quantiles = 0.5,
                                                se = FALSE) +
                                stat_fit_deviations(aes(colour = after_stat(posterior.weights)),
                                                    method = "rq")
  )
  # vdiffr::expect_doppelganger("fit_deviations_sma",
  #                             ggplot(my.data, aes(x, y)) +
  #                               geom_point() +
  #                               stat_poly_line(method = "sma") +
  #                               stat_fit_deviations(method = "sma")
  # )
  # vdiffr::expect_doppelganger("fit_deviations_ma",
  #                             ggplot(my.data, aes(x, y)) +
  #                               geom_point() +
  #                               stat_poly_line(method = "ma") +
  #                               stat_fit_deviations(method = "ma")
  # )
})


# stat_fit_residuals ------------------------------------------------------

test_that("stat_fit_residuals works with imports attached", {
  vdiffr::expect_doppelganger("fit_residuals",
                              ggplot(my.data, aes(x, y)) +
                                stat_fit_residuals()
  )
  vdiffr::expect_doppelganger("fit_residuals_orx",
                              ggplot(my.data, aes(x, y)) +
                                stat_fit_residuals(orientation = "x")
  )
  vdiffr::expect_doppelganger("fit_residuals_ory",
                              ggplot(my.data, aes(x, y)) +
                                stat_fit_residuals(orientation = "y")
  )
  vdiffr::expect_doppelganger("fit_residuals_formula_x",
                              ggplot(my.data, aes(x, y)) +
                                stat_fit_residuals(formula = y ~ x)
  )
  vdiffr::expect_doppelganger("fit_residuals_formula_y",
                              ggplot(my.data, aes(x, y)) +
                                stat_fit_residuals(formula = x ~ y)
  )
})

test_that("stat_fit_residuals works with different methods", {
  vdiffr::expect_doppelganger("fit_residuals_lm",
                              ggplot(my.data, aes(x, y)) +
                                stat_fit_residuals(method = "lm")
  )
  vdiffr::expect_doppelganger("fit_residuals_lm_weights",
                              ggplot(my.data, aes(x, y.otlr)) +
                                stat_fit_residuals(aes(colour = after_stat(posterior.weights)),
                                                   method = "lm")
  )
  vdiffr::expect_doppelganger("fit_residuals_rlm",
                              ggplot(my.data, aes(x, y)) +
                                stat_fit_residuals(method = "rlm")
  )
  vdiffr::expect_doppelganger("fit_residuals_rlm_weights",
                              ggplot(my.data, aes(x, y.otlr)) +
                                 stat_fit_residuals(aes(colour = after_stat(posterior.weights)),
                                                   method = "rlm")
  )
  vdiffr::expect_doppelganger("fit_residuals_lts",
                              ggplot(my.data, aes(x, y)) +
                                stat_fit_residuals(method = "lts")
  )
  vdiffr::expect_doppelganger("fit_residuals_lts_weigths",
                              ggplot(my.data, aes(x, y.otlr)) +
                                stat_fit_residuals(aes(colour = after_stat(posterior.weights)),
                                                   method = "lts")
  )
  vdiffr::expect_doppelganger("fit_residuals_gls",
                              ggplot(my.data, aes(x, y)) +
                                stat_fit_residuals(method = "gls")
  )
  vdiffr::expect_doppelganger("fit_residuals_gls_weights",
                              ggplot(my.data, aes(x, y)) +
                                stat_fit_residuals(aes(colour = after_stat(posterior.weights)),
                                                   method = "gls",
                                                   show.legend = TRUE)
  )
  vdiffr::expect_doppelganger("fit_residuals_gls_weights_vPower",
                              ggplot(my.data, aes(x, y)) +
                                stat_fit_residuals(aes(colour = after_stat(posterior.weights)),
                                                   method = "gls",
                                                   method.args = list(weights = nlme::varPower()))
  )
  vdiffr::expect_doppelganger("fit_residuals_sma",
                              ggplot(my.data, aes(x, y)) +
                                stat_fit_residuals(method = "sma")
  )
  vdiffr::expect_doppelganger("fit_residuals_ma",
                              ggplot(my.data, aes(x, y)) +
                                stat_fit_residuals(method = "ma")
  )
})


# stat_fit_fitted ---------------------------------------------------------

test_that("stat_fit_fitted works with imports attached", {
  vdiffr::expect_doppelganger("fit_fitted",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_fitted(geom = "line")
  )
  vdiffr::expect_doppelganger("fit_fitted_orx",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_fitted(orientation = "x",
                                                geom = "line")
  )
  vdiffr::expect_doppelganger("fit_fitted_ory",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_fitted(orientation = "y",
                                                geom = "line")
  )
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

test_that("stat_fit_fitted works with different methods", {
  vdiffr::expect_doppelganger("fit_fitted_lm",
                              ggplot(my.data, aes(x, y)) +
                                stat_poly_line(method = "lm") +
                                stat_fit_fitted(method = "lm")
  )
  vdiffr::expect_doppelganger("fit_fitted_rlm",
                              ggplot(my.data, aes(x, y)) +
                                stat_poly_line(method = "rlm") +
                                stat_fit_fitted(method = "rlm")
  )
  vdiffr::expect_doppelganger("fit_fitted_lts",
                              ggplot(my.data, aes(x, y)) +
                                stat_poly_line(method = "lts") +
                                stat_fit_fitted(method = "lts")
  )
  vdiffr::expect_doppelganger("fit_fitted_gls",
                              ggplot(my.data, aes(x, y)) +
                                stat_poly_line(method = "gls") +
                                stat_fit_fitted(method = "gls")
  )
  vdiffr::expect_doppelganger("fit_fitted_rq",
                              ggplot(my.data, aes(x, y)) +
                                stat_quant_line(method = "rq",
                                                quantiles = 0.5,
                                                se = FALSE) +
                                stat_fit_fitted(method = "rq")
  )
  # expect_warning(
  #   ggplot(my.data, aes(x, y)) + stat_fit_fitted(method = "sma")
  # )
  # expect_warning(
  #   ggplot(my.data, aes(x, y)) + stat_fit_fitted(method = "ma")
  # )
})
