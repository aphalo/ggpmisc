context("stat_quant_band")

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

if (isNamespaceLoaded(name = "package:ggpmisc")) detach(package:ggpmisc, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggpp")) detach(package:ggpp, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggplot2")) detach(package:ggplot2, unload = TRUE)

test_that("quant_band_noload", {
  withCallingHandlers({
    vdiffr::expect_doppelganger("stat_quant_band_noload",
                                ggplot2::ggplot(my.data, ggplot2::aes(x, y)) +
                                  ggplot2::geom_point() +
                                  ggpmisc::stat_quant_band(formula = y ~ 1)
    )
  }, warning=function(w) {
    if (grepl("Solution may be nonunique|2 non-positive fis", conditionMessage(w)))
      invokeRestart("muffleWarning")
  })  })

library(ggpmisc)

test_that("quant_formulas", {
  withCallingHandlers({
    vdiffr::expect_doppelganger("stat_quant_band_formula_1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_band(formula = y ~ 1)
    )

    vdiffr::expect_doppelganger("stat_quant_band_formula_x",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_band(formula = y ~ x)
    )

    vdiffr::expect_doppelganger("stat_quant_band_formula_x_Iy",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_band(formula = I(y) ~ x)
    )

    vdiffr::expect_doppelganger("stat_quant_band_formula_x_Ix",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_band(formula = y ~ I(x))
    )

    vdiffr::expect_doppelganger("stat_quant_band_formula_xminus1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_band(formula = y ~ x - 1)
    )

    vdiffr::expect_doppelganger("stat_quant_band_formula_xplus0",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_band(formula = y ~ x + 0)
    )

    vdiffr::expect_doppelganger("stat_quant_band_formula_poly1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_band(formula = y ~ poly(x, 1, raw = TRUE))
    )

    vdiffr::expect_doppelganger("stat_quant_band_formula_poly3",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_band(formula = y ~ poly(x, 3, raw = TRUE))
    )

    ###

    vdiffr::expect_doppelganger("stat_quant_band_formula_x1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_band(formula = x ~ 1)
    )

    vdiffr::expect_doppelganger("stat_quant_band_formula_y",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_band(formula = x ~ y)
    )

    vdiffr::expect_doppelganger("stat_quant_band_formula_y_Ix",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_band(formula = I(x) ~ y)
    )

    vdiffr::expect_doppelganger("stat_quant_band_formula_y_Iy",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_band(formula = x ~ I(y))
    )

    vdiffr::expect_doppelganger("stat_quant_band_formula_yminus1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_band(formula = x ~ y - 1)
    )

    vdiffr::expect_doppelganger("stat_quant_band_formula_yplus0",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_band(formula = x ~ y + 0)
    )

    vdiffr::expect_doppelganger("stat_quant_band_formula_ypoly1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_band(formula = x ~ poly(y, 1, raw = TRUE))
    )

  }, warning=function(w) {
    if (grepl("Solution may be nonunique|2 non-positive fis", conditionMessage(w)))
      invokeRestart("muffleWarning")
  })

})
