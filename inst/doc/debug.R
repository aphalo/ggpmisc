## ---- include=FALSE, echo=FALSE------------------------------------------
library(knitr)
opts_chunk$set(fig.path = 'figure/debug-pos-', fig.align = 'center', 
               fig.show = 'hold', fig.width = 7, fig.height = 4)
options(warnPartialMatchArgs = FALSE)

## ------------------------------------------------------------------------
library(ggpmisc)
# library(ggplot2)
library(tibble)

## ------------------------------------------------------------------------
set.seed(4321)
# generate artificial data
x <- 1:100
y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
my.data <- data.frame(x, 
                      y, 
                      group = c("A", "B"), 
                      y2 = y * c(0.5,2),
                      block = c("a", "a", "b", "b"))

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) + stat_debug_group()

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) + geom_point() + stat_debug_group()

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) + geom_point() + stat_debug_panel()

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + geom_point() + 
  stat_debug_group()

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + geom_point() + 
  stat_debug_panel()

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y, shape = group)) + geom_point() + 
  stat_debug_group()

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y, shape = group)) + geom_point() + 
  stat_debug_group(geom = "label", vjust = c(-0.5,1.5))

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) + geom_point() + 
  stat_debug_group(summary.fun = summary)

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) + geom_point() + 
  stat_debug_group(summary.fun = head)

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) + geom_point() + 
  stat_debug_group(summary.fun = nrow)

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) + geom_point() + 
  stat_debug_group(summary.fun = as_data_frame)

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) + geom_point() + 
  stat_debug_group(summary.fun = head, summary.fun.args = list(n = 3))

## ---- eval=FALSE---------------------------------------------------------
#  ggplot(my.data, aes(x, y)) + geom_point() +
#    stat_debug_group(summary.fun = function(x) {x})

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + geom_point() + 
  stat_debug_group(summary.fun = head, summary.fun.args = list(n = 3))

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + geom_point() + 
  stat_debug_group(summary.fun = nrow) +
  facet_wrap(~block)

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + geom_point() + 
  stat_debug_panel(summary.fun = nrow) +
  facet_wrap(~block)

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + geom_point() + 
  geom_debug(summary.fun = head)

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + geom_point() + 
  stat_smooth(method = "lm",
             geom = "debug", 
             summary.fun = as_data_frame, 
             summary.fun.args = list())

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + geom_point() + 
  stat_peaks(span = NULL,
             geom = "debug", 
             summary.fun = as_data_frame, 
             summary.fun.args = list())

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  stat_fit_residuals(formula = formula, 
                     geom = "debug",
                     summary.fun = as_data_frame, 
                     summary.fun.args = list())

## ------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_fit_augment(method = "lm", 
                   method.args = list(formula = formula),
                   geom = "debug",
                   summary.fun = tibble::as_data_frame, 
                   summary.fun.args = list()) +
  stat_fit_augment(method = "lm", 
                   method.args = list(formula = formula),
                   geom = "smooth")

## ------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y2, colour = group)) +
  geom_point() +
  stat_fit_augment(method = "lm", 
                   method.args = list(formula = formula),
                   geom = "debug",
                   summary.fun = tibble::as_data_frame, 
                   summary.fun.args = list()) +
  stat_fit_augment(method = "lm", 
                   method.args = list(formula = formula),
                   geom = "smooth")

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + geom_null()

