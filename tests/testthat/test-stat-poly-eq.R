context("stat_poly_eq")

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

test_that("poly_eq_noload", {
  vdiffr::expect_doppelganger("stat_poly_eq_noload",
                              ggplot2::ggplot(my.data, aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_poly_eq(formula = y ~ 1, parse = TRUE,
                                             mapping =
                                               ggplot2::aes(label = paste(ggplot2::after_stat(eq.label),
                                                                          ggplot2::after_stat(adj.rr.label),
                                                                          ggplot2::after_stat(AIC.label),
                                                                          ggplot2::after_stat(BIC.label),
                                                                 sep = "~~")))
  )
})

library(ggpmisc)

test_that("poly_formulas", {
  vdiffr::expect_doppelganger("stat_poly_eq_formula_1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ 1, parse = TRUE,
                                             mapping =
                                  aes(label = paste(after_stat(eq.label),
                                                    after_stat(adj.rr.label),
                                                    after_stat(AIC.label),
                                                    after_stat(BIC.label),
                                                    sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_1a",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ 1, parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_x",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_xminus1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x - 1, parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_xminus1a",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x -1, parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_xminus1b",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x -  1, parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_xplus0",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x + 0, parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_xplus0a",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x +0, parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_xplus0b",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x +  0, parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_poly1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ poly(x, 1), parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_poly3",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ poly(x, 3), parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_x1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = x ~ 1, parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(AIC.label),
                                                                 after_stat(BIC.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_y",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = x ~ y, parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_yminus1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = x ~ y - 1, parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_yplus0",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = x ~ y + 0, parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_ypoly1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = x ~ poly(y, 1), parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~")))
  )

})

test_that("poly_methods", {
  vdiffr::expect_doppelganger("stat_poly_eq_lm_chr",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             method = "lm",
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(rr.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 after_stat(n.label),
                                                                 after_stat(AIC.label),
                                                                 after_stat(BIC.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_lm_fun",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             method = lm,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(rr.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 after_stat(n.label),
                                                                 after_stat(AIC.label),
                                                                 after_stat(BIC.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_rlm_chr",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             method = "rlm",
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(rr.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 after_stat(n.label),
                                                                 after_stat(AIC.label),
                                                                 after_stat(BIC.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_rlm_fun",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             method = MASS::rlm,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(rr.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 after_stat(n.label),
                                                                 after_stat(AIC.label),
                                                                 after_stat(BIC.label),
                                                                 sep = "~~")))
  )

})

test_that("textual_positions", {
  vdiffr::expect_doppelganger("stat_poly_eq_0",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE)
  )
  vdiffr::expect_doppelganger("stat_poly_eq_1",
                               ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             geom = "text_npc")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_2",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             geom = "label_npc")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_3",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             geom = "text")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_4",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             geom = "label")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_5",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = "right", label.y = "bottom",
                                             geom = "text_npc")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_6",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = "right", label.y = "bottom",
                                             geom = "label_npc")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_7",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = "right", label.y = "bottom",
                                             geom = "text")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_8",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = "right", label.y = "bottom",
                                             geom = "label")
  )
})

test_that("numeric_positions", {
  vdiffr::expect_doppelganger("stat_poly_eq_n1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 0, label.y = 0,
                                             geom = "text_npc")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n2",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 0, label.y = 0,
                                             geom = "label_npc")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n3",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 0, label.y = -1e5,
                                             geom = "text")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n4",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 0, label.y = -1e5,
                                             geom = "label")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n5",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 1, label.y = 0.5,
                                             geom = "text_npc")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n6",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 1, label.y = 0.5,
                                             geom = "label_npc")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n7",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 100, label.y = 5e5,
                                             geom = "text")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n8",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 100, label.y = 5e5,
                                             geom = "label")
  )
})

test_that("rounding_signif", {
  vdiffr::expect_doppelganger("stat_poly_eq_formula_x_round",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x,
                                             parse = TRUE,
                                             rr.digits = 3,
                                             p.digits = 2,
                                             f.digits = 2,
                                             coef.digits = 6,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_1_round",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ 1,
                                             parse = TRUE,
                                             rr.digits = 3,
                                             p.digits = 2,
                                             f.digits = 2,
                                             coef.digits = 4,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~")))
  )

})

