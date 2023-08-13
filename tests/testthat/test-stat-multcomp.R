context("stat_multcomp")

library(tibble)

old.out.dec <- options(OutDec = ".")
on.exit(options(old.out.dec), add = TRUE, after = FALSE)

set.seed(4321)
# generate artificial data
x <- 1:100
y <- x + rnorm(length(x), mean = 0)

my.data <- data.frame(x,
                      y,
                      group = factor(c("A", "B", "C", "D")),
                      y2 = y + 0:3 * rnorm(length(x), mean = 30, sd = 2),
                      wt = sqrt(x))
formula <- y ~ x

if (isNamespaceLoaded(name = "package:ggpmisc")) detach(package:ggpmisc, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggpp")) detach(package:ggpp, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggplot2")) detach(package:ggplot2, unload = TRUE)

test_that("multcomp_noload", {
  vdiffr::expect_doppelganger("stat_multcomp_noload",
                              ggplot2::ggplot(my.data, ggplot2::aes(group, y2)) +
                                ggplot2::geom_boxplot() +
                                ggpmisc::stat_multcomp()
  )

  vdiffr::expect_doppelganger("stat_multcomp_noload_more",
                              ggplot2::ggplot(my.data, ggplot2::aes(group, y2)) +
                                ggplot2::stat_summary(fun.data = "mean_se") +
                                ggpmisc::stat_multcomp(mapping = ggpmisc::use_label(c("delta", "P")),
                                                       size = 2.5)
  )

})

library(ggpmisc)


test_that("stat_multcomp_contrast_type", {
  vdiffr::expect_doppelganger("stat_multcomp_bars_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Tukey")
  )

  vdiffr::expect_doppelganger("stat_multcomp_bars_dunnet",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Dunnet")
  )

  vdiffr::expect_doppelganger("stat_multcomp_letters_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Tukey",
                                              label.type = "letters")
  )

})

test_that("stat_multcomp_geoms", {
  vdiffr::expect_doppelganger("stat_multcomp_text_pairwise_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Tukey",
                                              geom = "text_pairwise",
                                              label.y = "bottom")
  )

  vdiffr::expect_doppelganger("stat_multcomp_text_pairwise_dunnet",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Dunnet",
                                              geom = "text_pairwise",
                                              label.y = "bottom")
  )

  vdiffr::expect_doppelganger("stat_multcomp_letters_label_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Tukey",
                                              label.type = "letters",
                                              geom = "label")
  )

})

test_that("stat_multcomp_label.y", {
  vdiffr::expect_doppelganger("stat_multcomp_y_top_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Tukey",
                                              label.y = "top")
  )

  vdiffr::expect_doppelganger("stat_multcomp_y_top_dunnet",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Dunnet",
                                              label.y = "top")
  )

  vdiffr::expect_doppelganger("stat_multcomp_y.bottom_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Tukey",
                                              label.type = "letters",
                                              label.y = "bottom")
  )

  vdiffr::expect_doppelganger("stat_multcomp_y_top_num_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Tukey",
                                              label.y = 150)
  )

  vdiffr::expect_doppelganger("stat_multcomp_y_top_num_dunnet",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Dunnet",
                                              label.y = 150)
  )

  vdiffr::expect_doppelganger("stat_multcomp_y.top_num_letters_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Tukey",
                                              label.type = "letters",
                                              label.y = 200)
  )

  vdiffr::expect_doppelganger("stat_multcomp_y_bottom_num_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Tukey",
                                              label.y = -25)
  )

  vdiffr::expect_doppelganger("stat_multcomp_y_bottom_num_dunnet",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Dunnet",
                                              label.y = -25)
  )

  vdiffr::expect_doppelganger("stat_multcomp_y.bottom_num_letters_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Tukey",
                                              label.type = "letters",
                                              label.y = -50)
  )

})

test_that("stat_multcomp_adjusted", {
  vdiffr::expect_doppelganger("stat_multcomp_bonferroni_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Tukey",
                                              adjusted.type = "bonferroni")
  )

  vdiffr::expect_doppelganger("stat_multcomp_bonferroni_dunnet",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Dunnet",
                                              adjusted.type = "bonferroni")
  )

})

test_that("stat_multcomp_digits", {
  vdiffr::expect_doppelganger("stat_multcomp_p.digits2",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Tukey",
                                              p.digits = 2)
  )

  vdiffr::expect_doppelganger("stat_multcomp_p.digits6",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrast.type = "Tukey",
                                              p.digits = 6)
  )

  # testthat::expect_warning(ggplot(my.data, aes(group, y2)) +
  #                            stat_boxplot() +
  #                            stat_multcomp(contrast.type = "Tukey",
  #                                          p.digits = 1))

  testthat::expect_error(ggplot(my.data, aes(group, y2)) +
                             stat_boxplot() +
                             stat_multcomp(contrast.type = "zzzz"))

  testthat::expect_error(ggplot(my.data, aes(group, y2)) +
                           stat_boxplot() +
                           stat_multcomp(label.type = "zzzz"))

})

test_that("stat_multcomp_methods", {
  vdiffr::expect_doppelganger("stat_multcomp_lm.char",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(method = "lm")
  )

  vdiffr::expect_doppelganger("stat_multcomp_lm.fun",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(method = stats::lm)
  )

  vdiffr::expect_doppelganger("stat_multcomp_lm.char",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(method = "aov")
  )

  vdiffr::expect_doppelganger("stat_multcomp_lm.fun",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(method = stats::aov)
  )

  vdiffr::expect_doppelganger("stat_multcomp_rlm.char",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(method = "rlm")
  )

  vdiffr::expect_doppelganger("stat_multcomp_rlm.fun",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(method = MASS::rlm)
  )
})

