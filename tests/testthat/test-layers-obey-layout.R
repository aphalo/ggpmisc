context("ggplot2 'layout'")

# these tests are possibly redundant and test against 'ggplot2'
# snaps are likely to depend of OS but not worthwhile to version the snaps
skip_on_os(c("mac", "linux", "solaris"))
# the layout parameter was introduced in 'ggplot2' 4.0.0 which is not required
skip_if_not_installed(pkg = "ggplot2", minimum_version = "4.0.0")

old.out.dec <- options(OutDec = ".")
on.exit(options(old.out.dec), add = TRUE, after = FALSE)

set.seed(4321)
# generate artificial data
my.data <- data.frame(x = (1:100) / 10,
                      y = (1:100) / 5 + rnorm(n = 100) + c(0, 1),
                      group = rep(c("A", "B"), 50))

p0 <- ggplot(data = my.data,
             mapping = aes(x, y)) +
  geom_point() +
  facet_grid(rows = vars(group))

test_that("stat_correlation obeys layout", {
  vdiffr::expect_doppelganger(
    "stat_correlation_layout",
     p0 + stat_correlation(layout = 1)
  )
})

test_that("stat_poly_eq obeys layout", {
  vdiffr::expect_doppelganger(
    "stat_poly_eq_layout",
    p0 + stat_poly_eq(layout = 1)
  )
})

test_that("stat_poly_line obeys layout", {
  vdiffr::expect_doppelganger(
    "stat_poly_line_layout",
    p0 + stat_poly_line(layout = 1)
  )
})

test_that("stat_ma_eq obeys layout", {
  vdiffr::expect_doppelganger(
    "stat_ma_eq_layout",
    p0 + stat_ma_eq(layout = 1)
  )
})

test_that("stat_ma_line obeys layout", {
  vdiffr::expect_doppelganger(
    "stat_ma_line_layout",
    p0 + stat_ma_line(layout = 1)
  )
})

test_that("stat_fit_tb obeys layout", {
  vdiffr::expect_doppelganger(
    "stat_fit_tb_layout",
    p0 + stat_fit_tb(layout = 1,
                     tb.type = "fit.summary",
                     label.x = "left")
  )
})

test_that("stat_fit_tidy obeys layout", {
  vdiffr::expect_doppelganger(
    "stat_fit_tidy_layout",
    p0 + stat_fit_tidy(aes(label = after_stat(fm.method)),
                       layout = 1)
  )
})

test_that("stat_fit_augment obeys layout", {
  vdiffr::expect_doppelganger(
    "stat_fit_augment_layout",
    p0 + stat_fit_augment(layout = 1)
  )
})

test_that("stat_peaks obeys layout", {
  vdiffr::expect_doppelganger(
    "stat_peaks_layout",
    p0 + stat_peaks(span = 5, colour = "red", layout = 1)
  )
})

test_that("stat_valleys obeys layout", {
  vdiffr::expect_doppelganger(
    "stat_valleys_layout",
    p0 + stat_valleys(span = 5, colour = "blue", layout = 1)
  )
})

# quantreg tends to warn about non-unique results
test_that("stat_quant_eq obeys layout", {
  withCallingHandlers({
    vdiffr::expect_doppelganger(
      "stat_quant_eq_layout",
      suppressWarnings(p0 + stat_quant_eq(layout = 1, quantiles = 0.5))
    )
  }, warning=function(w) {
    if (grepl("Solution may be nonunique|2 non-positive fis", conditionMessage(w)))
      invokeRestart("muffleWarning")
  }) })

test_that("stat_quant_line obeys layout", {
  withCallingHandlers({
    vdiffr::expect_doppelganger(
      "stat_quant_line_layout",
      suppressWarnings(p0 + stat_quant_line(layout = 1))
    )
  }, warning=function(w) {
    if (grepl("Solution may be nonunique|2 non-positive fis", conditionMessage(w)))
      invokeRestart("muffleWarning")
  })})

test_that("stat_quant_band obeys layout", {
  withCallingHandlers({
    vdiffr::expect_doppelganger(
      "stat_quant_band_layout",
      suppressWarnings(p0 + stat_quant_band(layout = 1))
    )
  }, warning=function(w) {
    if (grepl("Solution may be nonunique|2 non-positive fis", conditionMessage(w)))
      invokeRestart("muffleWarning")
  })})

# with x mapped to factor -------------------------------------------------

set.seed(4321)
# generate artificial data
my1.data <- data.frame(x = rep(c("A", "B"), 50),
                      y = 5 + rnorm(n = 100) + c(0, 1),
                      group = rep(c("a", "b"), each = 50))

p1 <- ggplot(data = my1.data,
             mapping = aes(x, y)) +
  geom_point() +
  facet_grid(rows = vars(group))

test_that("stat_multcomp obeys layout", {
  vdiffr::expect_doppelganger(
    "stat_multcomp_layout",
    p1 + stat_multcomp(layout = 1)
  )
})

# mixture of normals ------------------------------------------------------

set.seed(1234)
# generate artificial data
my2.data <- data.frame(x = c(rnorm(100), rnorm(100, mean = 6, sd = 1.5)),
                       group = rep(c("a", "b"), 100))

p2 <- ggplot(data = my2.data,
             mapping = aes(x)) +
  stat_bin(aes(y = after_stat(density))) +
  facet_grid(rows = vars(group))

test_that("stat_distrmix_line and stat_distrmix_eq obey layout", {
  vdiffr::expect_doppelganger(
    "stat_distrmix_line.eq_layout",
    p2 + stat_distrmix_line(layout = 1) +
      stat_distrmix_eq(mapping = use_label("eq"), layout = 2)
  )
})
