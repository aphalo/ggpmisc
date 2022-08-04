context("stat_correlation")

set.seed(4321)
# generate artificial data
my.data <- data.frame(x = (1:100) / 10 + rnorm(n = 100),
                      y = (1:100) / 5 + rnorm(n = 100))

if (isNamespaceLoaded(name = "package:ggpmisc")) detach(package:ggpmisc, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggpp")) detach(package:ggpp, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggplot2")) detach(package:ggplot2, unload = TRUE)

test_that("corr_noload", {
  set.seed(4321)
  vdiffr::expect_doppelganger("stat_correlation_pearson_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_correlation(method = "pearson",
                                                          mapping =
                                                            ggplot2::aes(label = paste(ggplot2::after_stat(r.label),
                                                                                       ggplot2::after_stat(r.confint.label),
                                                                                       ggplot2::after_stat(t.value.label),
                                                                                       ggplot2::after_stat(p.value.label),
                                                                                       ggplot2::after_stat(n.label),
                                                                                       sep = "~~")))
  )
  vdiffr::expect_doppelganger("stat_correlation_pearson_noload_use_label",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_correlation(method = "pearson",
                                                          mapping = ggpmisc::use_label(c("R", "R.CI", "t", "P", "n"),
                                                                                       sep = "~~"))
  )
  vdiffr::expect_doppelganger("stat_correlation_pearson_noload_use_label_lc",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_correlation(method = "pearson",
                                                          mapping = ggpmisc::use_label(c("cor", "r.ci", "t", "p", "n"),
                                                                                       sep = "~~"))
  )
  vdiffr::expect_doppelganger("stat_correlation_kendall_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_correlation(method = "kendall",
                                                          mapping =
                                                            ggplot2::aes(label = paste(ggplot2::after_stat(r.label),
                                                                                       ggplot2::after_stat(r.confint.label),
                                                                                       ggplot2::after_stat(z.value.label),
                                                                                       ggplot2::after_stat(p.value.label),
                                                                                       ggplot2::after_stat(n.label),
                                                                                       sep = "~~")))
  )
  vdiffr::expect_doppelganger("stat_correlation_kendall_noload_use_label",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_correlation(method = "kendall",
                                                          mapping = ggpmisc::use_label(c("R", "R.CI", "Z", "P", "n"),
                                                                                       sep = "~~"))

  )
  vdiffr::expect_doppelganger("stat_correlation_kendall_noload_use_label_lc",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_correlation(method = "kendall",
                                                          mapping = ggpmisc::use_label(c("tau", "tau.CI", "z", "p", "n"),
                                                                                       sep = "~~"))
  )
  vdiffr::expect_doppelganger("stat_correlation_spearman_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_correlation(method = "spearman",
                                                          mapping =
                                                            ggplot2::aes(label = paste(ggplot2::after_stat(r.label),
                                                                                       ggplot2::after_stat(r.confint.label),
                                                                                       ggplot2::after_stat(S.value.label),
                                                                                       ggplot2::after_stat(p.value.label),
                                                                                       ggplot2::after_stat(n.label),
                                                                                       sep = "~~")))
  )
  vdiffr::expect_doppelganger("stat_correlation_spearman_noload_use_label",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_correlation(method = "spearman",
                                                          mapping = ggpmisc::use_label(c("R", "R.CI", "S", "P", "N"),
                                                                                       sep = "~~"))
  )
  vdiffr::expect_doppelganger("stat_correlation_spearman_noload_use_label_lc",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_correlation(method = "spearman",
                                                          mapping = ggpmisc::use_label(c("rho", "rho.ci", "s", "p", "n"),
                                                                                       sep = "~~"))
  )
})

library(ggpmisc)

test_that("corr_load", {
  set.seed(4321)
  vdiffr::expect_doppelganger("stat_correlation_pearson_load",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_correlation(method = "pearson",
                                                 mapping =
                                                   aes(label = paste(after_stat(r.label),
                                                                     after_stat(r.confint.label),
                                                                     after_stat(t.value.label),
                                                                     after_stat(p.value.label),
                                                                     after_stat(n.label),
                                                                     sep = "~~")))
  )
  vdiffr::expect_doppelganger("stat_correlation_kendall_load",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_correlation(method = "kendall",
                                                 mapping =
                                                   aes(label = paste(after_stat(r.label),
                                                                     after_stat(r.confint.label),
                                                                     after_stat(z.value.label),
                                                                     after_stat(p.value.label),
                                                                     after_stat(n.label),
                                                                     sep = "~~")))
  )
  vdiffr::expect_doppelganger("stat_correlation_spearman_load",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_correlation(method = "spearman",
                                                 mapping =
                                                   aes(label = paste(after_stat(r.label),
                                                                     after_stat(r.confint.label),
                                                                     after_stat(S.value.label),
                                                                     after_stat(p.value.label),
                                                                     after_stat(n.label),
                                                                     sep = "~~")))
  )
  vdiffr::expect_doppelganger("stat_correlation_pearson_small",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_correlation(method = "pearson", small.r = TRUE, small.p = TRUE,
                                                 mapping =
                                                   aes(label = paste(after_stat(r.label),
                                                                     after_stat(r.confint.label),
                                                                     after_stat(t.value.label),
                                                                     after_stat(p.value.label),
                                                                     after_stat(n.label),
                                                                     sep = "~~")))
  )
})

test_that("corr_exact", {
  set.seed(4321)
  vdiffr::expect_doppelganger("stat_correlation_pearson_exact",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_correlation(method = "pearson", exact = TRUE,
                                                 mapping =
                                                   aes(label = paste(after_stat(r.label),
                                                                     after_stat(r.confint.label),
                                                                     after_stat(t.value.label),
                                                                     after_stat(p.value.label),
                                                                     after_stat(n.label),
                                                                     sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_correlation_pearson_exact_boot",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_correlation(method = "pearson", exact = TRUE,
                                                 boot.R = 999,
                                                 mapping =
                                                   aes(label = paste(after_stat(r.label),
                                                                     after_stat(r.confint.label),
                                                                     after_stat(t.value.label),
                                                                     after_stat(p.value.label),
                                                                     after_stat(n.label),
                                                                     sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_correlation_kendall_exact",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_correlation(method = "kendall", exact = TRUE,
                                                 mapping =
                                                   aes(label = paste(after_stat(r.label),
                                                                     after_stat(r.confint.label),
                                                                     after_stat(z.value.label),
                                                                     after_stat(p.value.label),
                                                                     after_stat(n.label),
                                                                     sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_correlation_spearman_exact",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_correlation(method = "spearman", exact = TRUE,
                                                 mapping =
                                                   aes(label = paste(after_stat(r.label),
                                                                     after_stat(r.confint.label),
                                                                     after_stat(S.value.label),
                                                                     after_stat(p.value.label),
                                                                     after_stat(n.label),
                                                                     sep = "~~")))
  )

})

test_that("corr_continuity", {
  set.seed(4321)
  vdiffr::expect_doppelganger("stat_correlation_pearson_cont",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_correlation(method = "pearson", continuity = TRUE,
                                                 mapping =
                                                   aes(label = paste(after_stat(r.label),
                                                                     after_stat(r.confint.label),
                                                                     after_stat(t.value.label),
                                                                     after_stat(p.value.label),
                                                                     after_stat(n.label),
                                                                     sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_correlation_kendall_cont",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_correlation(method = "kendall", continuity = TRUE,
                                                 mapping =
                                                   aes(label = paste(after_stat(r.label),
                                                                     after_stat(r.confint.label),
                                                                     after_stat(z.value.label),
                                                                     after_stat(p.value.label),
                                                                     after_stat(n.label),
                                                                     sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_correlation_spearman_cont",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_correlation(method = "spearman", continuity = TRUE,
                                                 mapping =
                                                   aes(label = paste(after_stat(r.label),
                                                                     after_stat(r.confint.label),
                                                                     after_stat(S.value.label),
                                                                     after_stat(p.value.label),
                                                                     after_stat(n.label),
                                                                     sep = "~~")))
  )

})

test_that("rounding_signif", {
  set.seed(4321)
  vdiffr::expect_doppelganger("stat_coor_pearson_round",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_correlation(r.digits = 3,
                                                 p.digits = 4,
                                                 t.digits = 4,
                                                 mapping =
                                                   aes(label = paste(after_stat(r.label),
                                                                     after_stat(r.confint.label),
                                                                     after_stat(t.value.label),
                                                                     after_stat(p.value.label),
                                                                     after_stat(n.label),
                                                                     sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_coor_pearson_round2",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_correlation(r.digits = 2,
                                                 p.digits = 2,
                                                 t.digits = 2,
                                                 mapping =
                                                   aes(label = paste(after_stat(r.label),
                                                                     after_stat(r.confint.label),
                                                                     after_stat(t.value.label),
                                                                     after_stat(p.value.label),
                                                                     after_stat(n.label),
                                                                     sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_coor_kendall_round",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_correlation(method = "kendall",
                                                 r.digits = 3,
                                                 p.digits = 4,
                                                 t.digits = 4,
                                                 mapping =
                                                   aes(label = paste(after_stat(r.label),
                                                                     after_stat(r.confint.label),
                                                                     after_stat(z.value.label),
                                                                     after_stat(p.value.label),
                                                                     after_stat(n.label),
                                                                     sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_coor_kendall_round2",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_correlation(method = "kendall",
                                                 r.digits = 2,
                                                 p.digits = 2,
                                                 t.digits = 2,
                                                 mapping =
                                                   aes(label = paste(after_stat(r.label),
                                                                     after_stat(r.confint.label),
                                                                     after_stat(z.value.label),
                                                                     after_stat(p.value.label),
                                                                     after_stat(n.label),
                                                                     sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_coor_spearman_round",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_correlation(method = "spearman",
                                                 r.digits = 3,
                                                 p.digits = 4,
                                                 t.digits = 4,
                                                 mapping =
                                                   aes(label = paste(after_stat(r.label),
                                                                     after_stat(r.confint.label),
                                                                     after_stat(S.value.label),
                                                                     after_stat(p.value.label),
                                                                     after_stat(n.label),
                                                                     sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_coor_spearman_round2",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_correlation(method = "spearman",
                                                 r.digits = 2,
                                                 p.digits = 2,
                                                 t.digits = 2,
                                                 mapping =
                                                   aes(label = paste(after_stat(r.label),
                                                                     after_stat(r.confint.label),
                                                                     after_stat(S.value.label),
                                                                     after_stat(p.value.label),
                                                                     after_stat(n.label),
                                                                     sep = "~~")))
  )


})

