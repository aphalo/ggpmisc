context("stat_dens2d_filter")

library(ggplot2)
library(tibble)

make_data_tbl <- function(nrow = 100, rfun = rnorm, ...) {
  if (nrow %% 2) {
    nrow <- nrow + 1
  }

  set.seed(1001)

  tibble::tibble(
    x = rfun(nrow, ...),
    y = rfun(nrow, ...),
    group = rep(c("A", "B"), c(nrow / 2, nrow / 2))
  )
}

test_that("numbers_tb", {
  vdiffr::expect_doppelganger("stat_d2d_flt_01",
                              ggplot(data = make_data_tbl(6), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(color = "red")
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_02",
                              ggplot(data = make_data_tbl(6), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(color = "red", keep.fraction = 1/2)
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_03",
                              ggplot(data = make_data_tbl(20), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(color = "red")
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_04",
                              ggplot(data = make_data_tbl(100), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(color = "red")
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_05",
                              ggplot(data = make_data_tbl(500), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(color = "red")
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_06",
                              ggplot(data = make_data_tbl(2000), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(color = "red")
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_07",
                              ggplot(data = make_data_tbl(2000), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(color = "red", keep.fraction = 0.01)
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_08",
                              ggplot(data = make_data_tbl(2000), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(color = "red",
                                                   keep.sparse = FALSE)
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_09",
                              ggplot(data = make_data_tbl(2000), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(color = "red",
                                                   keep.sparse = FALSE)+
                                stat_dens2d_filter(color = "blue")
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_10",
                              ggplot(data = make_data_tbl(2000), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(color = "red",
                                                   keep.fraction = 0.01,
                                                   keep.sparse = FALSE)
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_11",
                              ggplot(data = make_data_tbl(10000), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(color = "red")
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_12",
                              ggplot(data = make_data_tbl(10000), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(color = "red", keep.fraction = 0.01)
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_13",
                              ggplot(data = make_data_tbl(1000, rfun = runif), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(color = "red", keep.fraction = 0.1)
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_14",
                              ggplot(data = make_data_tbl(1000, rfun = rgamma, shape = 2), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(color = "red", keep.fraction = 0.1)
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_15",
                              ggplot(data = make_data_tbl(1000, rfun = rgamma, shape = 6), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(color = "red", keep.fraction = 0.1)
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_16",
                              ggplot(data = make_data_tbl(1000, rfun = rbeta, shape1 = 3, shape2 = 12), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(color = "red", keep.fraction = 0.1)
  )
  vdiffr::expect_doppelganger("stat_d2d_flt_17",
                              ggplot(data = make_data_tbl(1000, rfun = rbeta, shape1 = 3, shape2 = 12), aes(x, y)) +
                                geom_point() +
                                stat_dens2d_filter(color = "red", keep.fraction = 0.1) +
                                scale_y_log10()
  )
})
