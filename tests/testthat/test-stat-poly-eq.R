context("stat_poly_eq")

library(tibble)

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

old.out.dec <- options(OutDec = ".")
on.exit(options(old.out.dec), add = TRUE, after = FALSE)

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
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_poly_eq(formula = y ~ x, parse = TRUE,
                                                      mapping =
                                                        ggplot2::aes(label = paste(ggplot2::after_stat(eq.label),
                                                                                   ggplot2::after_stat(AIC.label),
                                                                                   ggplot2::after_stat(BIC.label),
                                                                                   sep = "~~"))),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("stat_poly_eq_noload_more",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_poly_eq(formula = y ~ x, parse = TRUE,
                                                      mapping =
                                                        ggplot2::aes(label = paste(ggplot2::after_stat(rr.label),
                                                                                   ggplot2::after_stat(rr.confint.label),
                                                                                   ggplot2::after_stat(adj.rr.label),
                                                                                   ggplot2::after_stat(f.value.label),
                                                                                   ggplot2::after_stat(p.value.label),
                                                                                   ggplot2::after_stat(n.label),
                                                                                   ggplot2::after_stat(method.label),
                                                                                   sep = "~~"))),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_noload_use_label",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_poly_eq(formula = y ~ x, parse = TRUE,
                                                      mapping = ggpmisc::use_label(c("eq", "AIC", "BIC"),
                                                                                   sep = "~~")),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_noload_use_label_more",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_poly_eq(formula = y ~ x, parse = TRUE,
                                                      mapping = ggpmisc::use_label(c("R2", "R2.CI", "adj.R2", "F", "P"),
                                                                                   sep = "~~")),
                              variant = snap_version
  )
})

library(ggpmisc)

# testthat does not "see" the messages and warnings, so these tests play no role

# test_that("number_of_rows_lm", {
#   # message works but is not seen by the test
#   expect_message(
#     ggplot(my.data[1, ], aes(x, y)) +
#     geom_point() +
#     stat_poly_eq(formula = y ~ x, parse = TRUE,
#                  mapping =
#                    aes(label = paste(after_stat(eq.label),
#                                      after_stat(adj.rr.label),
#                                      after_stat(AIC.label),
#                                      after_stat(BIC.label),
#                                      sep = "~~")))
#     )
#
#     expect_silent(
#       ggplot(my.data[1:2, ], aes(x, y)) +
#         geom_point() +
#         stat_poly_eq(formula = y ~ x, parse = TRUE,
#                      mapping =
#                        aes(label = paste(after_stat(eq.label),
#                                          after_stat(adj.rr.label),
#                                          after_stat(AIC.label),
#                                          after_stat(BIC.label),
#                                          sep = "~~")))
#       )
#
#     expect_message(
#       ggplot(my.data[1:2, ], aes(x, y)) +
#         geom_point() +
#         stat_poly_eq(formula = y ~ x + I(x^2), parse = TRUE,
#                      mapping =
#                        aes(label = paste(after_stat(eq.label),
#                                          after_stat(adj.rr.label),
#                                          after_stat(AIC.label),
#                                          after_stat(BIC.label),
#                                          sep = "~~")))
#     )
#
#     expect_silent(
#         ggplot(my.data[1:3, ], aes(x, y)) +
#           geom_point() +
#           stat_poly_eq(formula = y ~ x, parse = TRUE,
#                        mapping =
#                          aes(label = paste(after_stat(eq.label),
#                                            after_stat(adj.rr.label),
#                                            after_stat(AIC.label),
#                                            after_stat(BIC.label),
#                                            sep = "~~")))
#     )
#
#     expect_silent(
#       ggplot(my.data[1:3, ], aes(x, y)) +
#         geom_point() +
#         stat_poly_eq(formula = y ~ x + I(x^2), parse = TRUE,
#                      mapping =
#                        aes(label = paste(after_stat(eq.label),
#                                          after_stat(adj.rr.label),
#                                          after_stat(AIC.label),
#                                          after_stat(BIC.label),
#                                          sep = "~~")))
#     )
#
#     expect_message(
#       ggplot(my.data[1:3, ], aes(x, y)) +
#         geom_point() +
#         stat_poly_eq(formula = y ~ x + I(x^2) + I(x^3), parse = TRUE,
#                      mapping =
#                        aes(label = paste(after_stat(eq.label),
#                                          after_stat(adj.rr.label),
#                                          after_stat(AIC.label),
#                                          after_stat(BIC.label),
#                                          sep = "~~")))
#     )
#
#     expect_message(
#       ggplot(my.data[1:3, ], aes(x, y)) +
#         geom_point() +
#         stat_poly_eq(formula = x ~ y + I(y^2) + I(y^3), parse = TRUE,
#                      mapping =
#                        aes(label = paste(after_stat(eq.label),
#                                          after_stat(adj.rr.label),
#                                          after_stat(AIC.label),
#                                          after_stat(BIC.label),
#                                          sep = "~~")))
#     )
#
#
# })
#
# test_that("number_of_rows_rlm", {
#   # message works but is not seen by the test
#   expect_message(
#     ggplot(my.data[1, ], aes(x, y)) +
#     geom_point() +
#     stat_poly_eq(formula = y ~ x, parse = TRUE,
#                  method = "rlm",
#                  mapping =
#                    aes(label = paste(after_stat(eq.label),
#                                      after_stat(adj.rr.label),
#                                      after_stat(AIC.label),
#                                      after_stat(BIC.label),
#                                      sep = "~~")))
#     )
#
#   expect_silent(
#     ggplot(my.data[1:2, ], aes(x, y)) +
#       geom_point() +
#       stat_poly_eq(formula = y ~ x, parse = TRUE,
#                    method = "rlm",
#                    mapping =
#                      aes(label = paste(after_stat(eq.label),
#                                        after_stat(adj.rr.label),
#                                        after_stat(AIC.label),
#                                        after_stat(BIC.label),
#                                        sep = "~~")))
#   )
#
#   expect_warning(
#     ggplot(my.data[1:2, ], aes(x, y)) +
#       geom_point() +
#       stat_poly_eq(formula = y ~ x + I(x^2), parse = TRUE,
#                    method = "rlm",
#                    mapping =
#                      aes(label = paste(after_stat(eq.label),
#                                        after_stat(adj.rr.label),
#                                        after_stat(AIC.label),
#                                        after_stat(BIC.label),
#                                        sep = "~~")))
#   )
#   expect_message(
#     ggplot(my.data[1:2, ], aes(x, y)) +
#       geom_point() +
#       stat_poly_eq(formula = y ~ x + I(x^2), parse = TRUE,
#                    n.min = 3,
#                    method = "rlm",
#                    mapping =
#                      aes(label = paste(after_stat(eq.label),
#                                        after_stat(adj.rr.label),
#                                        after_stat(AIC.label),
#                                        after_stat(BIC.label),
#                                        sep = "~~")))
#   )
#
#   expect_warning(
#     ggplot(my.data[3:5, ], aes(x, y)) +
#       geom_point() +
#       stat_poly_eq(formula = y ~ x, parse = TRUE,
#                    method = "rlm",
#                    mapping =
#                      aes(label = paste(after_stat(eq.label),
#                                        after_stat(adj.rr.label),
#                                        after_stat(AIC.label),
#                                        after_stat(BIC.label),
#                                        sep = "~~")))
#   )
#
#   expect_silent(
#     ggplot(my.data[1:3, ], aes(x, y)) +
#       geom_point() +
#       stat_poly_eq(formula = y ~ x + I(x^2), parse = TRUE,
#                    method = "rlm",
#                    mapping =
#                      aes(label = paste(after_stat(eq.label),
#                                        after_stat(adj.rr.label),
#                                        after_stat(AIC.label),
#                                        after_stat(BIC.label),
#                                        sep = "~~")))
#   )
#
#   expect_warning(
#     ggplot(my.data[1:3, ], aes(x, y)) +
#       geom_point() +
#       stat_poly_eq(formula = y ~ x + I(x^2) + I(x^3), parse = TRUE,
#                    method = "rlm",
#                    mapping =
#                      aes(label = paste(after_stat(eq.label),
#                                        after_stat(adj.rr.label),
#                                        after_stat(AIC.label),
#                                        after_stat(BIC.label),
#                                        sep = "~~")))
#   )
#
#   expect_message(
#     ggplot(my.data[1:3, ], aes(x, y)) +
#       geom_point() +
#       stat_poly_eq(formula = y ~ x + I(x^2) + I(x^3), parse = TRUE,
#                    n.min = 4,
#                    method = "rlm",
#                    mapping =
#                      aes(label = paste(after_stat(eq.label),
#                                        after_stat(adj.rr.label),
#                                        after_stat(AIC.label),
#                                        after_stat(BIC.label),
#                                        sep = "~~")))
#   )
#
#   expect_message(
#     ggplot(my.data[1:3, ], aes(x, y)) +
#       geom_point() +
#       stat_poly_eq(formula = x ~ y + I(y^2) + I(y^3), parse = TRUE,
#                    n.min = 4,
#                    method = "rlm",
#                    mapping =
#                      aes(label = paste(after_stat(eq.label),
#                                        after_stat(adj.rr.label),
#                                        after_stat(AIC.label),
#                                        after_stat(BIC.label),
#                                        sep = "~~")))
#   )
#
# })

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
                                                    sep = "~~"))),
                              variant = snap_version
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
                                                                 after_stat(n.label),
                                                                 after_stat(method.label),
                                                                 sep = "~~"))),
                              variant = snap_version
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
                                                                 sep = "~~"))),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_x_Iy",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = I(y) ~ x, parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~"))),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_x_Ix",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ I(x), parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~"))),
                              variant = snap_version
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
                                                                 sep = "~~"))),
                              variant = snap_version
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
                                                                 sep = "~~"))),
                              variant = snap_version
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
                                                                 sep = "~~"))),
                              variant = snap_version
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
                                                                 sep = "~~"))),
                              variant = snap_version
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
                                                                 sep = "~~"))),
                              variant = snap_version
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
                                                                 sep = "~~"))),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_poly1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ poly(x, 1, raw = TRUE), parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~"))),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_poly3",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ poly(x, 3, raw = TRUE), parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~"))),
                              variant = snap_version
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
                                                                 sep = "~~"))),
                              variant = snap_version
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
                                                                 sep = "~~"))),
                              variant = snap_version
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
                                                                 sep = "~~"))),
                              variant = snap_version
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
                                                                 sep = "~~"))),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_ypoly1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = x ~ poly(y, 1, raw = TRUE), parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~"))),
                              variant = snap_version
  )

  old.out.dec <- options(OutDec = ",")
  vdiffr::expect_doppelganger("stat_poly_eq_formula_poly3_comma",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ poly(x, 3, raw = TRUE), parse = TRUE,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(rr.confint.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~"))),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_more_comma",
                              ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                ggplot2::geom_point() +
                                ggpmisc::stat_poly_eq(formula = y ~ x, parse = TRUE,
                                                      mapping =
                                                        ggplot2::aes(label = paste(after_stat(rr.label),
                                                                                   after_stat(f.value.label),
                                                                                   after_stat(p.value.label),
                                                                                   after_stat(n.label),
                                                                                   after_stat(method.label),
                                                                                   sep = "~~"))),
                              variant = snap_version
  )
  options(old.out.dec)
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
                                                                 sep = "~~"))),
                              variant = snap_version
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
                                                                 sep = "~~"))),
                              variant = snap_version
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
                                                                 sep = "~~"))),
                              variant = snap_version
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
                                                                 sep = "~~"))),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("stat_poly_eq_lqs_chr",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             method = "lqs",
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(rr.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 after_stat(n.label),
                                                                 after_stat(AIC.label),
                                                                 after_stat(BIC.label),
                                                                 sep = "~~"))),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("stat_poly_eq_lqs_fun",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             method = MASS::lqs,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(rr.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 after_stat(n.label),
                                                                 after_stat(AIC.label),
                                                                 after_stat(BIC.label),
                                                                 sep = "~~"))),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("stat_poly_eq_gls_chr",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             method = "gls",
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(rr.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 after_stat(n.label),
                                                                 after_stat(AIC.label),
                                                                 after_stat(BIC.label),
                                                                 sep = "~~"))),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("stat_poly_eq_gls_fun",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             method = nlme::gls,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(rr.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 after_stat(n.label),
                                                                 after_stat(AIC.label),
                                                                 after_stat(BIC.label),
                                                                 sep = "~~"))),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("stat_poly_eq_missing_fm",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             method = function(...) {list()})
  )

  vdiffr::expect_doppelganger("stat_poly_eq_NULL_fm",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             method = function(...) {NULL})
  )

  vdiffr::expect_doppelganger("stat_poly_eq_NA_fm",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             method = function(...) {NA})
  )

})

test_that("textual_positions", {
  vdiffr::expect_doppelganger("stat_poly_eq_0",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_1",
                               ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             geom = "text_npc"),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_2",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             geom = "label_npc"),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_3",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             geom = "text"),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_4",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             geom = "label"),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_5",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = "right", label.y = "bottom",
                                             geom = "text_npc"),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_6",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = "right", label.y = "bottom",
                                             geom = "label_npc"),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_7",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = "right", label.y = "bottom",
                                             geom = "text"),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_8",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = "right", label.y = "bottom",
                                             geom = "label"),
                              variant = snap_version
  )
})

test_that("numeric_positions", {
  vdiffr::expect_doppelganger("stat_poly_eq_n1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 0, label.y = 0,
                                             geom = "text_npc"),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n2",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 0, label.y = 0,
                                             geom = "label_npc"),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n3",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 0, label.y = -1e5,
                                             geom = "text"),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n4",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 0, label.y = -1e5,
                                             geom = "label"),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n5",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 1, label.y = 0.5,
                                             geom = "text_npc"),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n6",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 1, label.y = 0.5,
                                             geom = "label_npc"),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n7",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 100, label.y = 5e5,
                                             geom = "text"),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n8",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 100, label.y = 5e5,
                                             geom = "label"),
                              variant = snap_version
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
                                             f.digits = 3,
                                             coef.digits = 6,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~"))),
                              variant = snap_version
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
                                                                 sep = "~~"))),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_x_round_Inf",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x,
                                             parse = TRUE,
                                             rr.digits = 3,
                                             p.digits = Inf,
                                             f.digits = 3,
                                             coef.digits = 4,
                                             mapping =
                                               aes(label = paste(after_stat(eq.label),
                                                                 after_stat(adj.rr.label),
                                                                 after_stat(f.value.label),
                                                                 after_stat(p.value.label),
                                                                 sep = "~~"))),
                              variant = snap_version
  )

})

# Markdown ----------------------------------------------------------------

if (requireNamespace("ggtext", quietly = TRUE)) {
  library(ggtext)

  test_that("markdown_richtext", {
    vdiffr::expect_doppelganger("stat_poly_eq_n1_markdown",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_smooth(method = "lm", formula = formula) +
                                  stat_poly_eq(geom = "richtext",
                                               formula = formula,
                                               hjust = 0, vjust = 1,
                                               label.x = 0, label.y = 10e5),
                                variant = snap_version
    )
    vdiffr::expect_doppelganger("stat_poly_eq_n2_markdown",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_smooth(method = "lm", formula = formula) +
                                  stat_poly_eq(use_label("eq", "n", "P", "R2", sep = ", "),
                                               geom = "richtext",
                                               formula = formula,
                                               hjust = 0, vjust = 1,
                                               label.x = 0, label.y = 10e5),
                                variant = snap_version
    )
    vdiffr::expect_doppelganger("stat_poly_eq_n3_markdown",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_smooth(method = "lm", formula = formula) +
                                  stat_poly_eq(use_label("eq", "n", "P", "R2", sep = ", "),
                                               colour = "red",
                                               geom = "richtext",
                                               formula = formula,
                                               hjust = 0, vjust = 1,
                                               label.x = 0, label.y = 10e5),
                                variant = snap_version
    )
    vdiffr::expect_doppelganger("stat_poly_eq_n4_markdown",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_smooth(method = "lm", formula = formula) +
                                  stat_poly_eq(use_label("AIC", "BIC", "R2.CI", sep = ", "),
                                               geom = "richtext",
                                               formula = formula,
                                               hjust = 0, vjust = 1,
                                               label.x = 0, label.y = 10e5),
                                variant = snap_version
    )
  })
}

