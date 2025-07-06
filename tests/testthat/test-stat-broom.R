context("stat_fit_glance")

skip_if_not_installed("broom", minimum_version = NULL)
skip_if_not_installed("broom.mixed", minimum_version = NULL)

library(tibble)
library(nlme)
library(quantreg)
library(broom)
library(broom.mixed)

# versioning of snaps
ggplot2_version <- packageVersion("ggplot2")
if (grepl("^3\\.5\\.2\\.9|^4\\.0\\.[0-1]", ggplot2_version)) {
  ggplot2_version <- "gg-4.0.x"
} else if (grepl("^3\\.5\\.[0-2]", ggplot2_version)) {
  ggplot2_version <- "gg-3.5.x"
} else {
  ggplot2_version <- paste("gg", ggplot2_version, sep = "-")
}
R_version <- paste("R",
                   substr(as.character(getRversion()), start = 1, stop = 3),
                   sep = "-")
snap_version <- paste(R_version, ggplot2_version, sep = "_")

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

test_that("broom_noload", {
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_glance(geom = "debug")
  #
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_glance(method = "rq", geom = "debug")
  #
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_glance(method = "cor.test", method.args = list(formula = ~ x + y), geom = "debug")
  #
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_glance(method = "cor.test", method.arg = list(x = "x", y = "y"), geom = "debug")

  vdiffr::expect_doppelganger("glance_method_default_noload",
                              ggplot2::ggplot(my.data, aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_fit_glance(mapping =
                                                  ggplot2::aes(label = sprintf("%.3g, %.3f, %.3f, %.3g, %.3g, %.3g",
                                                                               ggplot2::after_stat(p.value),
                                                                               ggplot2::after_stat(r.squared),
                                                                               ggplot2::after_stat(adj.r.squared),
                                                                               ggplot2::after_stat(AIC),
                                                                               ggplot2::after_stat(BIC),
                                                                               ggplot2::after_stat(df.residual))))
  )
  vdiffr::expect_doppelganger("tidy_method_default_noload",
                              ggplot2::ggplot(my.data, aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_fit_tidy(mapping =
                                                         ggplot2::aes(label = sprintf("%.3g, %.3g, %.3g, %.3g\n%.3g, %.3g, %.3g, %.3g",
                                                                                      ggplot2::after_stat(Intercept_estimate),
                                                                                      ggplot2::after_stat(Intercept_p.value),
                                                                                      ggplot2::after_stat(Intercept_stat),
                                                                                      ggplot2::after_stat(Intercept_se),
                                                                                      ggplot2::after_stat(x_estimate),
                                                                                      ggplot2::after_stat(x_p.value),
                                                                                      ggplot2::after_stat(x_stat),
                                                                                      ggplot2::after_stat(x_se))))
  )
  vdiffr::expect_doppelganger("augment_method_default_noload",
                              ggplot2::ggplot(my.data, aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_fit_augment(),
                              variant = snap_version
  )
})

library(ggpmisc)

test_that("glance_methods", {
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_glance(geom = "debug")
  #
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_glance(method = "rq", geom = "debug")
  #
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_glance(method = "cor.test", method.args = list(formula = ~ x + y), geom = "debug")
  #
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_glance(method = "cor.test", method.arg = list(x = "x", y = "y"), geom = "debug")

  vdiffr::expect_doppelganger("glance_method_default",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_glance(mapping =
                                  aes(label = sprintf("%.3g, %.3f, %.3f, %.3g, %.3g, %.3g",
                                                    after_stat(p.value),
                                                    after_stat(r.squared),
                                                    after_stat(adj.r.squared),
                                                    after_stat(AIC),
                                                    after_stat(BIC),
                                                    after_stat(df.residual))))
  )

  vdiffr::expect_doppelganger("glance_method_lm_fun",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_glance(method = "lm",
                                                mapping =
                                                  aes(label = sprintf("%.3g, %.3f, %.3f, %.3g, %.3g, %.3g",
                                                                      after_stat(p.value),
                                                                      after_stat(r.squared),
                                                                      after_stat(adj.r.squared),
                                                                      after_stat(AIC),
                                                                      after_stat(BIC),
                                                                      after_stat(df.residual))))
  )

  vdiffr::expect_doppelganger("glance_method_lm_char",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_glance(method = "lm",
                                                mapping =
                                                  aes(label = sprintf("%.3g, %.3f, %.3f, %.3g, %.3g, %.3g",
                                                                      after_stat(p.value),
                                                                      after_stat(r.squared),
                                                                      after_stat(adj.r.squared),
                                                                      after_stat(AIC),
                                                                      after_stat(BIC),
                                                                      after_stat(df.residual))))
  )

  vdiffr::expect_doppelganger("glance_method_args",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_glance(method = "lm",
                                                method.args = list(formula = y ~ x + I(x^2)),
                                                mapping =
                                                  aes(label = sprintf("%.3g, %.3f, %.3f, %.3g, %.3g, %.3g",
                                                                      after_stat(p.value),
                                                                      after_stat(r.squared),
                                                                      after_stat(adj.r.squared),
                                                                      after_stat(AIC),
                                                                      after_stat(BIC),
                                                                      after_stat(df.residual))))
  )


  # triggers an expected warning but supressWarnings
  # vdiffr::expect_doppelganger("glance_method_cortest_xy",
  #                               ggplot(my.data, aes(x, y)) +
  #                                 geom_point() +
  #                                 stat_fit_glance(method = "cor.test",
  #                                                 method.args = list(x = "x", y = "y"),
  #                                                 mapping =
  #                                                   aes(label = sprintf("%.3g, %.3g",
  #                                                                       after_stat(estimate),
  #                                                                       after_stat(p.value))))
  # )

  vdiffr::expect_doppelganger("glance_method_cortest_formula",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_glance(method = "cor.test",
                                                method.args = list(formula = ~ x + y),
                                                mapping =
                                                  aes(label = sprintf("%.3g, %.3g",
                                                                      after_stat(estimate),
                                                                      after_stat(p.value))))
  )

  old.options <- options(warn = -1)
  vdiffr::expect_doppelganger("glance_method_rq",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_glance(method = "rq",
                                                method.args = list(formula = y ~ x + I(x^2)),
                                                mapping =
                                                  aes(label = sprintf("%.3g, %.3g, %.3g, %.3g, %.3g",
                                                                      after_stat(tau),
                                                                      after_stat(logLik),
                                                                      after_stat(AIC),
                                                                      after_stat(BIC),
                                                                      after_stat(df.residual))))
  )
  options(old.options)

})

test_that("tidy_methods", {
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_tidy(geom = "debug")
  #
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_tidy(method = "rq", geom = "debug")
  #
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_tidy(method = "rq", tidy.args = list(se = "nid"), geom = "debug")

  vdiffr::expect_doppelganger("tidy_method_default",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_tidy(mapping =
                                                  aes(label = sprintf("%.3g, %.3g, %.3g, %.3g\n%.3g, %.3g, %.3g, %.3g",
                                                                      after_stat(Intercept_estimate),
                                                                      after_stat(Intercept_p.value),
                                                                      after_stat(Intercept_stat),
                                                                      after_stat(Intercept_se),
                                                                      after_stat(x_estimate),
                                                                      after_stat(x_p.value),
                                                                      after_stat(x_stat),
                                                                      after_stat(x_se))))
  )

  vdiffr::expect_doppelganger("tidy_method_lm_fun",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_tidy(method = lm,
                                              mapping =
                                                aes(label = sprintf("%.3g, %.3g, %.3g, %.3g\n%.3g, %.3g, %.3g, %.3g",
                                                                    after_stat(Intercept_estimate),
                                                                    after_stat(Intercept_p.value),
                                                                    after_stat(Intercept_stat),
                                                                    after_stat(Intercept_se),
                                                                    after_stat(x_estimate),
                                                                    after_stat(x_p.value),
                                                                    after_stat(x_stat),
                                                                    after_stat(x_se))))
  )

  vdiffr::expect_doppelganger("tidy_method_lm_char",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_tidy(method = "lm",
                                              mapping =
                                                aes(label = sprintf("%.3g, %.3g, %.3g, %.3g\n%.3g, %.3g, %.3g, %.3g",
                                                                    after_stat(Intercept_estimate),
                                                                    after_stat(Intercept_p.value),
                                                                    after_stat(Intercept_stat),
                                                                    after_stat(Intercept_se),
                                                                    after_stat(x_estimate),
                                                                    after_stat(x_p.value),
                                                                    after_stat(x_stat),
                                                                    after_stat(x_se))))
  )

  vdiffr::expect_doppelganger("tidy_method_args",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_tidy(method = "lm",
                                              method.args = list(formula = y ~ x + I(x^2)),
                                              mapping =
                                                aes(label = sprintf("%.3g, %.3g, %.3g, %.3g\n%.3g, %.3g, %.3g, %.3g",
                                                                    after_stat(Intercept_estimate),
                                                                    after_stat(Intercept_p.value),
                                                                    after_stat(Intercept_stat),
                                                                    after_stat(Intercept_se),
                                                                    after_stat(x_estimate),
                                                                    after_stat(x_p.value),
                                                                    after_stat(x_stat),
                                                                    after_stat(x_se))))
  )

  old.options <- options(warn = -1)
  vdiffr::expect_doppelganger("tidy_tidy_args",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_tidy(method = "rq",
                                              tidy.args = list(se.type = "nid"),
                                              mapping =
                                                aes(label = sprintf("%.3g, %.3g, %.3g, %.3g\n%.3g, %.3g, %.3g, %.3g",
                                                                    after_stat(Intercept_estimate),
                                                                    after_stat(Intercept_p.value),
                                                                    after_stat(Intercept_stat),
                                                                    after_stat(Intercept_se),
                                                                    after_stat(x_estimate),
                                                                    after_stat(x_p.value),
                                                                    after_stat(x_stat),
                                                                    after_stat(x_se))))
  )

  skip_on_os("mac")
  vdiffr::expect_doppelganger("tidy_method_rq",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_tidy(method = "rq",
                                              mapping =
                                                aes(label = sprintf("%.3g, %.3g, %.3g, %.3g\n%.3g, %.3g, %.3g, %.3g",
                                                                    after_stat(Intercept_estimate),
                                                                    after_stat(Intercept_conf.low),
                                                                    after_stat(Intercept_conf.high),
                                                                    after_stat(Intercept_tau),
                                                                    after_stat(x_estimate),
                                                                    after_stat(x_conf.low),
                                                                    after_stat(x_conf.high),
                                                                    after_stat(x_tau))))
  )
  options(old.options)

})

test_that("augment_methods", {
  # ggplot(my.data, aes(x, y)) +
  #   geom_point() +
  #   stat_fit_augment(geom = "debug")

  vdiffr::expect_doppelganger("augment_method_default",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_augment(),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("augment_method_lm_fun",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_augment(method = lm),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("augment_method_lm_char",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_augment(method = "lm"),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("augment_method_args",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_augment(method = "lm",
                                                 method.args = list(formula = y ~ x + I(x^2))),
                              variant = snap_version
  )

  old.options <- options(warn = -1)
  vdiffr::expect_doppelganger("augment_method_rq",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_augment(method = "rq"),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("augment_rqmethod__args",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_fit_augment(method = "rq",
                                                 method.args = list(formula = y ~ x + I(x^2))),
                              variant = snap_version
  )
  options(old.options)

})
