context("stat_quant_eq")

library(tibble)

old.out.dec <- options(OutDec = ".")
on.exit(options(old.out.dec), add = TRUE, after = FALSE)

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

if (isNamespaceLoaded(name = "package:ggpmisc")) detach(package:ggpmisc, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggpp")) detach(package:ggpp, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggplot2")) detach(package:ggplot2, unload = TRUE)

test_that("quant_eq_noload", {
  withCallingHandlers({
    vdiffr::expect_doppelganger("stat_quant_eq_noload",
                                ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                  ggplot2::geom_point() +
                                  ggpmisc::stat_quant_eq(formula = y ~ x, parse = TRUE,
                                                mapping =
                                                  ggplot2::aes(label = paste(ggplot2::after_stat(eq.label),
                                                                    ggplot2::after_stat(rho.label),
                                                                    ggplot2::after_stat(AIC.label),
                                                                    ggplot2::after_stat(method.label),
                                                                    sep = "~~")))
    )
    vdiffr::expect_doppelganger("stat_quant_eq_noload_use_label",
                                ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                  ggplot2::geom_point() +
                                  ggpmisc::stat_quant_eq(formula = y ~ x, parse = TRUE,
                                                         mapping = ggpmisc::use_label(c("qtl", "eq", "rho", "AIC", "method"),
                                                                                      ggplot2::aes(colour = ggplot2::after_stat(qtl.label)),
                                                                                      sep = "~~"))
    )
  }, warning=function(w) {
    if (grepl("Solution may be nonunique|2 non-positive fis", conditionMessage(w)))
      invokeRestart("muffleWarning")
  })  })

library(ggpmisc)

# testthat does not "see" the messages and warnings, so these tests play no role

# test_that("number_of_rows_quantreg", {
#   # message works but is not seen by the test
#   expect_message(
#     ggplot(my.data[1, ], aes(x, y)) +
#       geom_point() +
#       stat_quant_eq(formula = y ~ x, parse = TRUE,
#                     mapping =
#                       aes(label = paste(after_stat(eq.label),
#                                         after_stat(rho.label),
#                                         after_stat(AIC.label),
#                                         sep = "~~")))
#   )
#
#   expect_silent(
#     ggplot(my.data[1:2, ], aes(x, y)) +
#       geom_point() +
#       stat_quant_eq(formula = y ~ x, parse = TRUE,
#                     mapping =
#                       aes(label = paste(after_stat(eq.label),
#                                         after_stat(rho.label),
#                                         after_stat(AIC.label),
#                                         sep = "~~")))
#   )
#
#   expect_silent(
#     ggplot(my.data[1:3, ], aes(x, y)) +
#       geom_point() +
#       stat_quant_eq(formula = y ~ x, parse = TRUE,
#                     mapping =
#                       aes(label = paste(after_stat(eq.label),
#                                         after_stat(rho.label),
#                                         after_stat(AIC.label),
#                                         sep = "~~")))
#   )
#
#   expect_message(
#     ggplot(my.data[1:2, ], aes(x, y)) +
#       geom_point() +
#       stat_quant_eq(formula = y ~ x + I(x^2), parse = TRUE,
#                     mapping =
#                       aes(label = paste(after_stat(eq.label),
#                                         after_stat(rho.label),
#                                         after_stat(AIC.label),
#                                         sep = "~~")))
#   )
#
#   expect_silent(
#     ggplot(my.data[1:3, ], aes(x, y)) +
#       geom_point() +
#       stat_quant_eq(formula = y ~ x, parse = TRUE,
#                    mapping =
#                      aes(label = paste(after_stat(eq.label),
#                                        after_stat(rho.label),
#                                        after_stat(AIC.label),
#                                        sep = "~~")))
#   )
#
#   expect_silent(
#     ggplot(my.data[1:3, ], aes(x, y)) +
#       geom_point() +
#       stat_quant_eq(formula = y ~ x + I(x^2), parse = TRUE,
#                    mapping =
#                      aes(label = paste(after_stat(eq.label),
#                                        aes(label = paste(after_stat(eq.label),
#                                                          after_stat(rho.label),
#                                                          after_stat(AIC.label),
#                                                          sep = "~~")))))
#   )
#
#   expect_message(
#     ggplot(my.data[1:3, ], aes(x, y)) +
#       geom_point() +
#       stat_quant_eq(formula = y ~ x + I(x^2) + I(x^3), parse = TRUE,
#                    mapping =
#                      aes(label = paste(after_stat(eq.label),
#                                        after_stat(rho.label),
#                                        after_stat(AIC.label),
#                                        sep = "~~")))
#   )
#
#   expect_message(
#     ggplot(my.data[1:3, ], aes(x, y)) +
#       geom_point() +
#       stat_quant_eq(formula = x ~ y + I(y^2) + I(y^3), parse = TRUE,
#                    mapping =
#                      aes(label = paste(after_stat(eq.label),
#                                        after_stat(rho.label),
#                                        after_stat(AIC.label),
#                                        sep = "~~")))
#   )
#
# })
#

test_that("quant_formulas", {
  withCallingHandlers({
    vdiffr::expect_doppelganger("stat_quant_eq_formula_1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ 1, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_1a",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ 1, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_x",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ x, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_x_Iy",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = I(y) ~ x, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_x_Ix",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ I(x), parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_xminus1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ x - 1, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_xminus1a",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ x -1, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_xminus1b",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ x -  1, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_xplus0",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ x + 0, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_xplus0a",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ x +0, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_xplus0b",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ x +  0, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_poly1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ poly(x, 1, raw = TRUE), parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_poly3",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = y ~ poly(x, 3, raw = TRUE), parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    ###

    vdiffr::expect_doppelganger("stat_quant_eq_formula_x1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = x ~ 1, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_y",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = x ~ y, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_y_Ix",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = I(x) ~ y, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_y_Iy",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = x ~ I(y), parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_yminus1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = x ~ y - 1, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_yplus0",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = x ~ y + 0, parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

    vdiffr::expect_doppelganger("stat_quant_eq_formula_ypoly1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = x ~ poly(y, 1, raw = TRUE), parse = TRUE,
                                                mapping =
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )

  }, warning=function(w) {
    if (startsWith(conditionMessage(w), "Solution may be nonunique"))
      invokeRestart("muffleWarning")
  })

})

# library(ggtext)
# test_that("markdown_values", {
#   withCallingHandlers({
#     vdiffr::expect_doppelganger("stat_quant_eq_numeric",
#                                 ggplot(my.data, aes(x, y)) +
#                                   geom_point() +
#                                   stat_quant_eq(formula = y ~ poly(x, 3, raw = TRUE),
#                                                 hjust = 0, vstep = 0.1,
#                                                 coef.keep.zeros = TRUE,
#                                                 geom = "richtext",
#                                                 output.type = "markdown",
#                                                 mapping =
#                                                   aes(label = after_stat(AIC.label)))
#     )
#   }, warning=function(w) {
#     if (startsWith(conditionMessage(w), "Solution may be nonunique"))
#       invokeRestart("muffleWarning")
#   })
#
# })

test_that("quant_formulas", {
  vdiffr::expect_doppelganger("stat_quant_eq_fm_NA",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_quant_eq(formula = y ~ 1, parse = TRUE,
                                              method = function(...) {NA},
                                              mapping =
                                                aes(label = paste(after_stat(eq.label),
                                                                  after_stat(rho.label),
                                                                  after_stat(AIC.label),
                                                                  sep = "~~")))
  )
  vdiffr::expect_doppelganger("stat_quant_eq_fm_NULL",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_quant_eq(formula = y ~ 1, parse = TRUE,
                                              method = function(...) {NULL},
                                              mapping =
                                                aes(label = paste(after_stat(eq.label),
                                                                  after_stat(rho.label),
                                                                  after_stat(AIC.label),
                                                                  sep = "~~")))
  )
  vdiffr::expect_doppelganger("stat_quant_eq_fm_missing",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_quant_eq(formula = y ~ 1, parse = TRUE,
                                              method = function(...) {list()},
                                              mapping =
                                                aes(label = paste(after_stat(eq.label),
                                                                  after_stat(rho.label),
                                                                  after_stat(AIC.label),
                                                                  sep = "~~")))
  )

})


formula_n <- y ~ x + I(x^2) + I(x^3)
my.format <-
  "b[0]~`=`~%.3g*\", \"*b[1]~`=`~%.3g*\", \"*b[2]~`=`~%.3g*\", \"*b[3]~`=`~%.3g"

test_that("numeric_values", {
  withCallingHandlers({
    vdiffr::expect_doppelganger("stat_quant_eq_numeric",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_eq(formula = formula_n,
                                                quantiles = 0.5,
                                                output.type = "numeric",
                                                parse = TRUE,
                                                mapping =
                                                  aes(label = sprintf(my.format,
                                                                      after_stat(b_0), after_stat(b_1),
                                                                      after_stat(b_2), after_stat(b_3)))) +
                                  facet_wrap(~group, ncol = 1)
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
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
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
                                                  aes(label = paste(after_stat(eq.label),
                                                                    after_stat(rho.label),
                                                                    after_stat(AIC.label),
                                                                    sep = "~~")))
    )
  }, warning=function(w) {
    if (startsWith(conditionMessage(w), "Solution may be nonunique"))
      invokeRestart("muffleWarning")
  })

})

