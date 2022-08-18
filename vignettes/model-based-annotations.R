## ---- include=FALSE, echo=FALSE-----------------------------------------------
library(knitr)
opts_chunk$set(fig.align = 'center', 
               fig.show = 'hold', fig.width = 7, fig.height = 4)
options(warnPartialMatchArgs = FALSE,
        tibble.print.max = 4,
        tibble.print.min = 4,
        dplyr.summarise.inform = FALSE)
eval_flag <- TRUE # evaluate all code chunks

## ---- message=FALSE-----------------------------------------------------------
library(ggpmisc)
library(tibble)
library(dplyr)
library(quantreg)

eval_nlme <-  requireNamespace("nlme", quietly = TRUE)
if (eval_nlme) library(nlme)
eval_broom <-  requireNamespace("broom", quietly = TRUE)
if (eval_broom) library(broom)
eval_broom_mixed <-  requireNamespace("broom.mixed", quietly = TRUE)
if (eval_broom_mixed) library(broom.mixed)
eval_gginnards <-  requireNamespace("gginnards", quietly = TRUE)
if (eval_gginnards) library(gginnards)

## -----------------------------------------------------------------------------
old_theme <- theme_set(theme_bw())

## -----------------------------------------------------------------------------
set.seed(4321)
x <- (1:100) / 10
y <- x + rnorm(length(x))
my.data <- data.frame(x = x,
                      y = y,
                      y.desc = - y,
                      group = c("A", "B"))

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_correlation()

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y, color = group)) +
  geom_point() +
  stat_correlation()

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y, color = group)) +
  geom_point() +
  stat_correlation(method = "spearman")

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y, color = group)) +
  geom_point() +
  stat_correlation(mapping = use_label(c("R", "t", "P", "n")))

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_correlation() +
  facet_wrap(~group)

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_correlation(mapping = aes(color = ifelse(after_stat(cor) > 0.955,
                                                "red", "black"))) +
  scale_color_identity() +
  facet_wrap(~group)

## -----------------------------------------------------------------------------
set.seed(4321)
# generate artificial data
x <- 1:100
y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
y <- y / max(y)
my.data <- data.frame(x, 
                      y, 
                      group = c("A", "B"), 
                      y2 = y * c(1, 2) + c(0, 0.2),
                      block = c("a", "a", "b", "b"),
                      wt = sqrt(x))

