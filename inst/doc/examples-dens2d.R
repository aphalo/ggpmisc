## ---- include=FALSE, echo=FALSE------------------------------------------
library(knitr)
opts_chunk$set(fig.path = 'figure/dens2d-pos-', fig.align = 'center', 
               fig.show = 'hold', fig.width = 7, fig.height = 4)
options(warnPartialMatchArgs = FALSE)

## ------------------------------------------------------------------------
library(ggpmisc)
library(ggplot2)
library(tibble)

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(6), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red")

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(6), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red", keep.fraction = 1/2)

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(20), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red")

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(100), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red")

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(500), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red")

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(2000), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red")

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(2000), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red", keep.fraction = 0.01)

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(2000), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red", 
                     keep.sparse = FALSE)

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(2000), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red", 
                     keep.sparse = FALSE)+
  stat_dens2d_filter(color = "blue")

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(2000), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red", 
                     keep.fraction = 0.01,
                     keep.sparse = FALSE)

## ---- eval=FALSE---------------------------------------------------------
#  ggplot(data = make_data_tbl(10000), aes(x, y)) +
#    geom_point() +
#    stat_dens2d_filter(color = "red")

## ---- eval=FALSE---------------------------------------------------------
#  ggplot(data = make_data_tbl(10000), aes(x, y)) +
#    geom_point() +
#    stat_dens2d_filter(color = "red", keep.fraction = 0.01)

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(1000, rfun = runif), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red", keep.fraction = 0.1)

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(1000, rfun = rgamma, shape = 2), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red", keep.fraction = 0.1)

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(1000, rfun = rgamma, shape = 6), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red", keep.fraction = 0.1)

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(1000, rfun = rbeta, shape1 = 3, shape2 = 12), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red", keep.fraction = 0.1)

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(1000, rfun = rbeta, shape1 = 3, shape2 = 12), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red", keep.fraction = 0.1) +
  scale_y_log10()

