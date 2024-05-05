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
                      group = factor(LETTERS[1:5]),
                      group.rev = factor(LETTERS[1:5], levels = LETTERS[5:1]),
                      group.mixed = factor(LETTERS[1:5], levels = LETTERS[c(3,5,4,1,2)]),
                      group10 = factor(LETTERS[1:10]),
                      y2 = y + 0:4 * rnorm(length(x), mean = 20, sd = 2),
                      y10 = y + 0:9 * rnorm(length(x), mean = 20, sd = 4),
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
  set.seed(4321)

  vdiffr::expect_doppelganger("stat_multcomp_bars_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey")
  )

  vdiffr::expect_doppelganger("stat_multcomp_bars_dunnet",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Dunnet")
  )

  vdiffr::expect_doppelganger("stat_multcomp_letters_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey",
                                              label.type = "letters")
  )

  vdiffr::expect_doppelganger("stat_multcomp_letters_tukey_rev",
                              ggplot(my.data, aes(group.rev, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey",
                                              label.type = "letters")
  )

  vdiffr::expect_doppelganger("stat_multcomp_letters_tukey_mixed",
                              ggplot(my.data, aes(group.mixed, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey",
                                              label.type = "letters")
  )

  testthat::expect_error(ggplot(my.data, aes(group.mixed, y2)) +
                              stat_boxplot() +
                              stat_multcomp(contrasts = "Dunnet",
                                            label.type = "letters")
                          )
})

test_that("stat_multcomp_many_levels", {
  set.seed(4321)

  vdiffr::expect_doppelganger("stat_multcomp_bars_tukey2",
                              ggplot(subset(my.data, group10 %in% LETTERS[1:6]), aes(group10, y10)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey",
                                              label.type = "letters") +
                                expand_limits(x = -1)
  )

  # warning is suppressed during test!!
  # expect_warning(ggplot(subset(my.data, group10 %in% LETTERS[1:6]), aes(group10, y10)) +
  #                                                stat_boxplot() +
  #                                                stat_multcomp(contrasts = "Tukey")
  # )

  vdiffr::expect_doppelganger("stat_multcomp_bars_dunnet_many_levels",
                              ggplot(my.data, aes(group10, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Dunnet", p.digits = 2)
  )

  vdiffr::expect_doppelganger("stat_multcomp_letters_tukey_many_levels",
                              ggplot(subset(my.data, group10 %in% LETTERS[1:6]), aes(group10, y10)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey",
                                              label.type = "letters",
                                              adj.method.tag = 0)
  )

})

test_that("stat_multcomp_geoms", {
  set.seed(4321)

  vdiffr::expect_doppelganger("stat_multcomp_text_pairwise_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey",
                                              geom = "text_pairwise",
                                              label.y = "bottom",
                                              p.digits = 2)
  )

  vdiffr::expect_doppelganger("stat_multcomp_text_pairwise_dunnet",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Dunnet",
                                              geom = "text_pairwise",
                                              label.y = "bottom")
  )

  vdiffr::expect_doppelganger("stat_multcomp_letters_label_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey",
                                              label.type = "letters",
                                              geom = "label")
  )

})

test_that("stat_multcomp_label.y", {
  set.seed(4321)

  vdiffr::expect_doppelganger("stat_multcomp_y_top_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey",
                                              label.y = "top")
  )

  vdiffr::expect_doppelganger("stat_multcomp_y_top_dunnet",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Dunnet",
                                              label.y = "top")
  )

  vdiffr::expect_doppelganger("stat_multcomp_y.bottom_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey",
                                              label.type = "letters",
                                              label.y = "bottom",
                                              p.digits = 2)
  )

  vdiffr::expect_doppelganger("stat_multcomp_y_top_num_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey",
                                              label.y = 150,
                                              p.digits = 2)
  )

  vdiffr::expect_doppelganger("stat_multcomp_y_top_num_dunnet",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Dunnet",
                                              label.y = 150)
  )

  vdiffr::expect_doppelganger("stat_multcomp_y.top_num_letters_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey",
                                              label.type = "letters",
                                              label.y = 200)
  )

  vdiffr::expect_doppelganger("stat_multcomp_y_bottom_num_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey",
                                              label.y = -25,
                                              p.digits = 2)
  )

  vdiffr::expect_doppelganger("stat_multcomp_y_bottom_num_dunnet",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Dunnet",
                                              label.y = -25)
  )

  vdiffr::expect_doppelganger("stat_multcomp_y.bottom_num_letters_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey",
                                              label.type = "letters",
                                              label.y = -50)
  )

})

test_that("stat_multcomp_adjusted", {
  set.seed(4321)

  vdiffr::expect_doppelganger("stat_multcomp_bonferroni_tukey",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey",
                                              p.adjust.method = "bonferroni")
  )

  vdiffr::expect_doppelganger("stat_multcomp_bonferroni_dunnet",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Dunnet",
                                              p.adjust.method = "bonferroni")
  )

})

test_that("stat_multcomp_digits", {
  set.seed(4321)

  vdiffr::expect_doppelganger("stat_multcomp_p.digits2",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey",
                                              p.digits = 2)
  )

  # p-value unstable in least significant digit
  vdiffr::expect_doppelganger("stat_multcomp_p.digits6",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey",
                                              p.digits = 6)
  )

  # p-value unstable in least significant digit
  vdiffr::expect_doppelganger("stat_multcomp_p.digits_Inf",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(contrasts = "Tukey",
                                              p.digits = Inf)
  )

  # warning is solected by testthat!!
  # testthat::expect_warning(ggplot(my.data, aes(group, y2)) +
  #                            stat_boxplot() +
  #                            stat_multcomp(contrasts = "Tukey",
  #                                          p.digits = 1))

  testthat::expect_error(ggplot(my.data, aes(group, y2)) +
                             stat_boxplot() +
                             stat_multcomp(contrasts = "zzzz"))

  testthat::expect_error(ggplot(my.data, aes(group, y2)) +
                           stat_boxplot() +
                           stat_multcomp(label.type = "zzzz"))

})

test_that("stat_multcomp_methods", {
  set.seed(4321)

  vdiffr::expect_doppelganger("stat_multcomp_lm.char2",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(method = "lm")
  )

  vdiffr::expect_doppelganger("stat_multcomp_lm.fun",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(method = stats::lm,
                                              p.digits = 2)
  )

  # inconsistent across OSs
  vdiffr::expect_doppelganger("stat_multcomp_lm.char",
                              ggplot(my.data, aes(group, y2)) +
                                stat_boxplot() +
                                stat_multcomp(method = "aov")
  )

  vdiffr::expect_doppelganger("stat_multcomp_lm.fun2",
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

