context("stat_quant_line")

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

test_that("quant_formulas", {
  withCallingHandlers({
    vdiffr::expect_doppelganger("stat_quant_line_formula_1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_line(formula = y ~ 1)
    )

    vdiffr::expect_doppelganger("stat_quant_line_formula_x",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_line(formula = y ~ x)
    )

    vdiffr::expect_doppelganger("stat_quant_line_formula_xminus1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_line(formula = y ~ x - 1)
    )

    vdiffr::expect_doppelganger("stat_quant_line_formula_xplus0",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_line(formula = y ~ x + 0)
    )

    vdiffr::expect_doppelganger("stat_quant_line_formula_poly1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_line(formula = y ~ poly(x, 1, raw = TRUE))
    )

    vdiffr::expect_doppelganger("stat_quant_line_formula_poly3",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_line(formula = y ~ poly(x, 3, raw = TRUE))
    )

    ###

    vdiffr::expect_doppelganger("stat_quant_line_formula_x1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_line(formula = x ~ 1)
    )

    vdiffr::expect_doppelganger("stat_quant_line_formula_y",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_line(formula = x ~ y)
    )

    vdiffr::expect_doppelganger("stat_quant_line_formula_yminus1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_line(formula = x ~ y - 1)
    )

    vdiffr::expect_doppelganger("stat_quant_line_formula_yplus0",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_line(formula = x ~ y + 0)
    )

    vdiffr::expect_doppelganger("stat_quant_line_formula_ypoly1",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_quant_line(formula = x ~ poly(y, 1, raw = TRUE))
    )

  }, warning=function(w) {
    if (grepl("Solution may be nonunique|2 non-positive fis", conditionMessage(w)))
      invokeRestart("muffleWarning")
  })

})
