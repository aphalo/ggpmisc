context("stat_quant_eq")

library(tibble)

set.seed(4321)
# generate artificial data
x <- 1:100
y <- x + rnorm(length(x), mean = 0, sd = 10)
my.data <- data.frame(x,
                      y,
                      group = c("A", "B"),
                      y2 = y * c(0.5,2),
                      block = c("a", "a", "b", "b"),
                      wt = sqrt(x))
formula <- y ~ x

test_that("quant_formulas", {
  withCallingHandlers({
    vdiffr::expect_doppelganger("stat_quant_eq_formula_1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ 1, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(stat(eq.label),
                                                                    stat(rho.label),
                                                                    stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_1a",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ 1, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(stat(eq.label),
                                                                    stat(rho.label),
                                                                    stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_x",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ x, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(stat(eq.label),
                                                                    stat(rho.label),
                                                                    stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_xminus1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ x - 1, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(stat(eq.label),
                                                                    stat(rho.label),
                                                                    stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_xminus1a",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ x -1, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(stat(eq.label),
                                                                    stat(rho.label),
                                                                    stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_xminus1b",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ x -  1, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(stat(eq.label),
                                                                    stat(rho.label),
                                                                    stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_xplus0",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ x + 0, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(stat(eq.label),
                                                                    stat(rho.label),
                                                                    stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_xplus0a",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ x +0, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(stat(eq.label),
                                                                    stat(rho.label),
                                                                    stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_xplus0b",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ x +  0, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(stat(eq.label),
                                                                    stat(rho.label),
                                                                    stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_poly1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ poly(x, 1, raw = TRUE), parse = TRUE,
                                                mapping =
                                                  aes(label = paste(stat(eq.label),
                                                                    stat(rho.label),
                                                                    stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_poly3",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ poly(x, 3, raw = TRUE), parse = TRUE,
                                                mapping =
                                                  aes(label = paste(stat(eq.label),
                                                                    stat(rho.label),
                                                                    stat(AIC.label),
                                                                    sep = "~~")))
    )
  }, warning=function(w) {
    if (startsWith(conditionMessage(w), "Solution may be nonunique"))
      invokeRestart("muffleWarning")
  })

})

test_that("textual_positions", {
  withCallingHandlers({
    vdiffr::expect_doppelganger("stat_quant_eq_0",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quantile(formula = formula) +
                                  stat_quant_eq(formula = formula, parse = TRUE)
    )
    vdiffr::expect_doppelganger("stat_quant_eq_1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quantile(formula = formula) +
                                  stat_quant_eq(formula = formula, parse = TRUE,
                                                geom = "text_npc")
    )
    vdiffr::expect_doppelganger("stat_quant_eq_2",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quantile(formula = formula) +
                                  stat_quant_eq(formula = formula, parse = TRUE,
                                                geom = "label_npc")
    )
    vdiffr::expect_doppelganger("stat_quant_eq_3",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_smooth(method = "lm", formula = formula) +
                                  stat_quant_eq(formula = formula, parse = TRUE,
                                                geom = "text")
    )
    vdiffr::expect_doppelganger("stat_quant_eq_4",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quantile(formula = formula) +
                                  stat_quant_eq(formula = formula, parse = TRUE,
                                                geom = "label")
    )
    vdiffr::expect_doppelganger("stat_quant_eq_5",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_smooth(method = "lm", formula = formula) +
                                  stat_quant_eq(formula = formula, parse = TRUE,
                                                label.x = "right", label.y = "bottom",
                                                geom = "text_npc")
    )
    vdiffr::expect_doppelganger("stat_quant_eq_6",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quantile(formula = formula) +
                                  stat_quant_eq(formula = formula, parse = TRUE,
                                                label.x = "right", label.y = "bottom",
                                                geom = "label_npc")
    )
    vdiffr::expect_doppelganger("stat_quant_eq_7",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quantile(formula = formula) +
                                  stat_quant_eq(formula = formula, parse = TRUE,
                                                label.x = "right", label.y = "bottom",
                                                geom = "text")
    )
    vdiffr::expect_doppelganger("stat_quant_eq_8",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quantile(formula = formula) +
                                  stat_quant_eq(formula = formula, parse = TRUE,
                                                label.x = "right", label.y = "bottom",
                                                geom = "label")
    )
  }, warning=function(w) {
    if (startsWith(conditionMessage(w), "Solution may be nonunique"))
      invokeRestart("muffleWarning")
  })

})

test_that("numeric_positions", {
  withCallingHandlers({
    vdiffr::expect_doppelganger("stat_quant_eq_n1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quantile(formula = formula) +
                                  stat_quant_eq(formula = formula, parse = TRUE,
                                                label.x = 0.05, label.y = 0.05,
                                                geom = "text_npc")
    )
    vdiffr::expect_doppelganger("stat_quant_eq_n2",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quantile(formula = formula) +
                                  stat_quant_eq(formula = formula, parse = TRUE,
                                                label.x = 0.05, label.y = 0.05,
                                                geom = "label_npc")
    )
    vdiffr::expect_doppelganger("stat_quant_eq_n3",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quantile(formula = formula) +
                                  stat_quant_eq(formula = formula, parse = TRUE,
                                                label.x = 0, label.y = -1e5,
                                                geom = "text")
    )
    vdiffr::expect_doppelganger("stat_quant_eq_n4",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quantile(formula = formula) +
                                  stat_quant_eq(formula = formula, parse = TRUE,
                                                label.x = 0, label.y = -1e5,
                                                geom = "label")
    )
    vdiffr::expect_doppelganger("stat_quant_eq_n5",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quantile(formula = formula) +
                                  stat_quant_eq(formula = formula, parse = TRUE,
                                                label.x = 0.95, label.y = 0.5,
                                                geom = "text_npc")
    )
    vdiffr::expect_doppelganger("stat_quant_eq_n6",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quantile(formula = formula) +
                                  stat_quant_eq(formula = formula, parse = TRUE,
                                                label.x = 1, label.y = 0.5,
                                                geom = "label_npc")
    )
    vdiffr::expect_doppelganger("stat_quant_eq_n7",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quantile(formula = formula) +
                                  stat_quant_eq(formula = formula, parse = TRUE,
                                                label.x = 95, label.y = 5e5,
                                                geom = "text")
    )
    vdiffr::expect_doppelganger("stat_quant_eq_n8",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quantile(formula = formula) +
                                  stat_quant_eq(formula = formula, parse = TRUE,
                                                label.x = 95, label.y = 5e5,
                                                geom = "label")
    )
  }, warning=function(w) {
    if (startsWith(conditionMessage(w), "Solution may be nonunique"))
      invokeRestart("muffleWarning")
  })

})

test_that("rounding_signif", {
  withCallingHandlers({

    vdiffr::expect_doppelganger("stat_quant_eq_formula_x_round",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ x,
                                                parse = TRUE,
                                                rho.digits = 3,
                                                coef.digits = 6,
                                                mapping =
                                                  aes(label = paste(stat(eq.label),
                                                                    stat(rho.label),
                                                                    stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_1_round",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ 1,
                                                parse = TRUE,
                                                rho.digits = 2,
                                                coef.digits = 4,
                                                mapping =
                                                  aes(label = paste(stat(eq.label),
                                                                    stat(rho.label),
                                                                    stat(AIC.label),
                                                                    sep = "~~")))
    )
  }, warning=function(w) {
    if (startsWith(conditionMessage(w), "Solution may be nonunique"))
      invokeRestart("muffleWarning")
  })

})

