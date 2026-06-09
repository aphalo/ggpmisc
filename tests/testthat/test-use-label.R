context("use_label")

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

library(ggpmisc)

test_that("use_label finds labels", {
  vdiffr::expect_doppelganger("use_label_R",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             mapping = use_label("R2", "adj.R2", "F", "P", "n")),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("use_label_eq",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             mapping = use_label("eq", "AIC", "BIC", "method")),
                              variant = snap_version
  )
})

test_that("use_label pastes labels", {
    vdiffr::expect_doppelganger("use_label_plotmath",
                                ggplot(my.data, aes(x, y)) +
                                  geom_point() +
                                  stat_poly_eq(formula = y ~ x, parse = TRUE,
                                               mapping = use_label("AIC", "BIC",
                                                                   sep = "~lambda~")),
                                variant = snap_version
    )

  vdiffr::expect_doppelganger("use_label_string",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             mapping = use_label("AIC", "BIC",
                                                                 sep = "*\" xyz \"*")),
                              variant = snap_version
  )
})

test_that("f_use_label builds labels", {
  vdiffr::expect_doppelganger("f_use_label_plotmath",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             mapping = f_use_label("AIC", "BIC",
                                                                   format = "%s~lambda~%s")),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("f_use_label_string",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             mapping = f_use_label("AIC", "BIC",
                                                                 format = "%s*\" xyz \"*%s")),
                              variant = snap_version
  )

  vdiffr::expect_doppelganger("f_use_label_single",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             mapping = f_use_label("eq", "method",
                                                                   format = "\"Model equation \"*%s*\" using method \"*%s")),
                              variant = snap_version
  )
})
