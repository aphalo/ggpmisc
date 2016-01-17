## ---- include=FALSE, echo=FALSE------------------------------------------
library(knitr)
opts_chunk$set(fig.path = 'figure/pos-', fig.align = 'center', fig.show = 'hold',
               fig.width = 7, fig.height = 4)
options(warnPartialMatchArgs = FALSE)

## ------------------------------------------------------------------------
library(ggplot2)
library(ggpmisc)

## ------------------------------------------------------------------------
set.seed(4321)
# generate artificial data
x <- 1:100
y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
my.data <- data.frame(x, y, group = c("A", "B"), y2 = y * c(0.5,2))


## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..adj.rr.label..), formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")), 
               formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula, parse = TRUE) +
  facet_wrap(~group)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula, parse = TRUE) +
  facet_wrap(~group, scales = "free_y")

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 1, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 2, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 5, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) + geom_point() + stat_debug()

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + geom_point() + 
  stat_debug(vjust = c(-0.5,1.5))

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y, shape = group)) + geom_point() + 
  stat_debug(vjust = c(-0.5,1.5))

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) + geom_point() + 
  facet_wrap(~group) + stat_debug()

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) + geom_point() + 
  facet_wrap(~group) + stat_debug()

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group, shape = group)) + geom_point() + 
  facet_wrap(~group) + stat_debug()

## ------------------------------------------------------------------------
lynx.df <- data.frame(year = as.numeric(time(lynx)), lynx = as.matrix(lynx))


## ------------------------------------------------------------------------
ggplot(lynx.df, aes(year, lynx)) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", vjust = -0.5, x.label.fmt = "%4.0f") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", vjust = 1.5, x.label.fmt = "%4.0f") +
  ylim(-100, 7300)

## ------------------------------------------------------------------------
ggplot(lynx.df, aes(year, lynx)) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "rug", colour = "red") +
  stat_peaks(geom = "text", colour = "red", vjust = -0.5, x.label.fmt = "%4.0f") +
  ylim(NA, 7300)

## ------------------------------------------------------------------------
ggplot(lynx.df, aes(year, lynx)) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "rug", colour = "red") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "rug", colour = "blue")

