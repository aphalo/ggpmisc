context("stat_corelation")

set.seed(4321)
# generate artificial data
my.data <- data.frame(x = (1:100) / 10 + rnorm(n = 100),
                      y = (1:100) / 5 + rnorm(n = 100))

if (isNamespaceLoaded(name = "package:ggpmisc")) detach(package:ggpmisc, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggpp")) detach(package:ggpp, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggplot2")) detach(package:ggplot2, unload = TRUE)

test_that("corr_noload", {
  vdiffr::expect_doppelganger("stat_corelation_pearson_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_corelation(method = "pearson",
                                             mapping =
                                               ggplot2::aes(label = paste(ggplot2::after_stat(r.label),
                                                                          ggplot2::after_stat(t.value.label),
                                                                          ggplot2::after_stat(p.value.label),
                                                                          ggplot2::after_stat(n.label),
                                                                 sep = "~~")))
  )
  vdiffr::expect_doppelganger("stat_corelation_kendall_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_corelation(method = "kendall",
                                                   mapping =
                                                     ggplot2::aes(label = paste(ggplot2::after_stat(r.label),
                                                                                ggplot2::after_stat(z.value.label),
                                                                                ggplot2::after_stat(p.value.label),
                                                                                ggplot2::after_stat(n.label),
                                                                                sep = "~~")))
  )
  vdiffr::expect_doppelganger("stat_corelation_spearman_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_corelation(method = "spearman",
                                                   mapping =
                                                     ggplot2::aes(label = paste(ggplot2::after_stat(r.label),
                                                                                ggplot2::after_stat(S.value.label),
                                                                                ggplot2::after_stat(p.value.label),
                                                                                ggplot2::after_stat(n.label),
                                                                                sep = "~~")))
  )
})

library(ggpmisc)

test_that("corr_load", {
  vdiffr::expect_doppelganger("stat_corelation_pearson_load",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_corelation(method = "pearson",
                                                   mapping =
                                                     ggplot2::aes(label = paste(ggplot2::after_stat(r.label),
                                                                                ggplot2::after_stat(t.value.label),
                                                                                ggplot2::after_stat(p.value.label),
                                                                                ggplot2::after_stat(n.label),
                                                                                sep = "~~")))
  )
  vdiffr::expect_doppelganger("stat_corelation_kendall_load",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_corelation(method = "kendall",
                                                   mapping =
                                                     ggplot2::aes(label = paste(ggplot2::after_stat(r.label),
                                                                                ggplot2::after_stat(z.value.label),
                                                                                ggplot2::after_stat(p.value.label),
                                                                                ggplot2::after_stat(n.label),
                                                                                sep = "~~")))
  )
  vdiffr::expect_doppelganger("stat_corelation_spearman_load",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_corelation(method = "spearman",
                                                   mapping =
                                                     ggplot2::aes(label = paste(ggplot2::after_stat(r.label),
                                                                                ggplot2::after_stat(S.value.label),
                                                                                ggplot2::after_stat(p.value.label),
                                                                                ggplot2::after_stat(n.label),
                                                                                sep = "~~")))
  )
})

test_that("corr_exact", {
    vdiffr::expect_doppelganger("stat_corelation_pearson_exact",
                                ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                  ggplot2::geom_point() +
                                  ggpmisc::stat_corelation(method = "pearson", exact = TRUE,
                                                     mapping =
                                                       ggplot2::aes(label = paste(ggplot2::after_stat(r.label),
                                                                                  ggplot2::after_stat(t.value.label),
                                                                                  ggplot2::after_stat(p.value.label),
                                                                                  ggplot2::after_stat(n.label),
                                                                                  sep = "~~")))
    )

  vdiffr::expect_doppelganger("stat_corelation_kendall_exact",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_corelation(method = "kendall", exact = TRUE,
                                                   mapping =
                                                     ggplot2::aes(label = paste(ggplot2::after_stat(r.label),
                                                                                ggplot2::after_stat(z.value.label),
                                                                                ggplot2::after_stat(p.value.label),
                                                                                ggplot2::after_stat(n.label),
                                                                                sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_corelation_spearman_exact",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_corelation(method = "spearman", exact = TRUE,
                                                   mapping =
                                                     ggplot2::aes(label = paste(ggplot2::after_stat(r.label),
                                                                                ggplot2::after_stat(S.value.label),
                                                                                ggplot2::after_stat(p.value.label),
                                                                                ggplot2::after_stat(n.label),
                                                                                sep = "~~")))
  )

})

test_that("corr_continuity", {
  vdiffr::expect_doppelganger("stat_corelation_pearson_cont",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_corelation(method = "pearson", continuity = TRUE,
                                                   mapping =
                                                     ggplot2::aes(label = paste(ggplot2::after_stat(r.label),
                                                                                ggplot2::after_stat(t.value.label),
                                                                                ggplot2::after_stat(p.value.label),
                                                                                ggplot2::after_stat(n.label),
                                                                                sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_corelation_kendall_cont",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_corelation(method = "kendall", continuity = TRUE,
                                                   mapping =
                                                     ggplot2::aes(label = paste(ggplot2::after_stat(r.label),
                                                                                ggplot2::after_stat(z.value.label),
                                                                                ggplot2::after_stat(p.value.label),
                                                                                ggplot2::after_stat(n.label),
                                                                                sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_corelation_spearman_cont",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_corelation(method = "spearman", continuity = TRUE,
                                                   mapping =
                                                     ggplot2::aes(label = paste(ggplot2::after_stat(r.label),
                                                                                ggplot2::after_stat(S.value.label),
                                                                                ggplot2::after_stat(p.value.label),
                                                                                ggplot2::after_stat(n.label),
                                                                                sep = "~~")))
  )

})

test_that("rounding_signif", {
  vdiffr::expect_doppelganger("stat_coor_pearson_round",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_corelation(r.digits = 3,
                                          p.digits = 4,
                                           t.digits = 4,
                                           mapping =
                                             aes(label = paste(after_stat(r.label),
                                                               after_stat(t.value.label),
                                                               after_stat(p.value.label),
                                                               after_stat(n.label),
                                                               sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_coor_pearson_round2",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_corelation(r.digits = 2,
                                          p.digits = 2,
                                          t.digits = 2,
                                          mapping =
                                            aes(label = paste(after_stat(r.label),
                                                              after_stat(t.value.label),
                                                              after_stat(p.value.label),
                                                              after_stat(n.label),
                                                              sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_coor_kendall_round",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_corelation(method = "kendall",
                                          r.digits = 3,
                                          p.digits = 4,
                                          t.digits = 4,
                                          mapping =
                                            aes(label = paste(after_stat(r.label),
                                                              after_stat(z.value.label),
                                                              after_stat(p.value.label),
                                                              after_stat(n.label),
                                                              sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_coor_kendall_round2",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_corelation(method = "kendall",
                                          r.digits = 2,
                                          p.digits = 2,
                                          t.digits = 2,
                                          mapping =
                                            aes(label = paste(after_stat(r.label),
                                                              after_stat(z.value.label),
                                                              after_stat(p.value.label),
                                                              after_stat(n.label),
                                                              sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_coor_spearman_round",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_corelation(method = "spearman",
                                          r.digits = 3,
                                          p.digits = 4,
                                          t.digits = 4,
                                          mapping =
                                            aes(label = paste(after_stat(r.label),
                                                              after_stat(S.value.label),
                                                              after_stat(p.value.label),
                                                              after_stat(n.label),
                                                              sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_coor_spearman_round2",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_corelation(method = "spearman",
                                          r.digits = 2,
                                          p.digits = 2,
                                          t.digits = 2,
                                          mapping =
                                            aes(label = paste(after_stat(r.label),
                                                              after_stat(S.value.label),
                                                              after_stat(p.value.label),
                                                              after_stat(n.label),
                                                              sep = "~~")))
  )


})

