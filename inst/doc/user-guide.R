## ---- include=FALSE, echo=FALSE------------------------------------------
library(knitr)
opts_chunk$set(fig.path = 'figure/guide-pos-', fig.align = 'center', 
               fig.show = 'hold', fig.width = 7, fig.height = 4)
options(warnPartialMatchArgs = FALSE)

## ------------------------------------------------------------------------
library(ggpmisc)
library(ggplot2)
library(ggrepel)
library(xts)
library(lubridate)
library(tibble)
library(nlme)

## ------------------------------------------------------------------------
class(austres)
austres.df <- try_tibble(austres)
class(austres.df)
lapply(austres.df, "class")
head(austres.df, 4)

## ------------------------------------------------------------------------
austres.df <- try_tibble(austres, as.numeric = TRUE)
lapply(austres.df, "class")
head(austres.df, 4)

## ------------------------------------------------------------------------
class(lynx)
lynx.df <- try_tibble(lynx)
class(lynx.df)
lapply(lynx.df, "class")
head(lynx.df, 3)

## ------------------------------------------------------------------------
lynx.df <- try_tibble(lynx, "year")
head(lynx.df, 3)

## ------------------------------------------------------------------------
lynx_n.df <- try_tibble(lynx, "year", as.numeric = TRUE)
lapply(lynx_n.df, "class")
head(lynx_n.df, 3)

## ------------------------------------------------------------------------
try_tibble(1:5)

## ------------------------------------------------------------------------
try_tibble(letters[1:5])

## ------------------------------------------------------------------------
try_tibble(factor(letters[1:5]))

## ------------------------------------------------------------------------
try_tibble(list(x = rep(1,5), y = 1:5))

## ------------------------------------------------------------------------
try_tibble(data.frame(x = rep(1,5), y = 1:5))

## ------------------------------------------------------------------------
try_tibble(matrix(1:10, ncol = 2))

## ------------------------------------------------------------------------
ggplot(lynx) + geom_line()

## ------------------------------------------------------------------------
ggplot(lynx, as.numeric = FALSE) + geom_line()

## ------------------------------------------------------------------------
ggplot(AirPassengers) + geom_line()

## ------------------------------------------------------------------------
ggplot(AirPassengers, as.numeric = FALSE) + geom_line()

## ------------------------------------------------------------------------
ggplot(lynx, as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", vjust = -0.5) +
  ylim(-100, 7300)

## ------------------------------------------------------------------------
ggplot(lynx) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", vjust = -0.5) +
  ylim(-100, 7300)

## ------------------------------------------------------------------------
ggplot(lynx, as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", vjust = -0.5, x.label.fmt = "%Y") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", vjust = 1.5, x.label.fmt = "%Y") +
  ylim(-100, 7300)

## ------------------------------------------------------------------------
ggplot(lynx) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", vjust = -0.5, x.label.fmt = "%4.0f") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", vjust = 1.5, x.label.fmt = "%4.0f") +
  ylim(-100, 7300)

## ------------------------------------------------------------------------
ggplot(lynx, as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", angle = 66,
             hjust = -0.1, x.label.fmt = "%Y") +
  ylim(NA, 7800)

## ------------------------------------------------------------------------
ggplot(lynx, as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "rug", colour = "red") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "rug", colour = "blue")

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
  stat_poly_eq(aes(label = ..adj.rr.label..), formula = formula, 
               parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..AIC.label..), 
               formula = formula, 
               parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula, 
               parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
               formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = paste("atop(", ..AIC.label.., ",", ..BIC.label.., ")", sep = "")), 
               formula = formula, 
               parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..),
               eq.with.lhs = FALSE,
               formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..),
               eq.with.lhs = "italic(hat(y))~`=`~",
               formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  labs(x = expression(italic(z)), y = expression(italic(h)) ) + 
  stat_poly_eq(aes(label = ..eq.label..),
               eq.with.lhs = "italic(h)~`=`~",
               eq.x.rhs = "~italic(z)",
               formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 2, raw = TRUE)
ggplot(my.data, aes(x, log10(y + 1e6))) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..),
               eq.with.lhs = "plain(log)[10](italic(y)+10^6)~`=`~",
               formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 5, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3) - 1
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula, 
               parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..), size = 3,
               formula = formula, parse = TRUE) +
  facet_wrap(~group)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..), size = 3,
               formula = formula, parse = TRUE) +
  facet_wrap(~group, scales = "free_y")

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..),
               formula = formula, parse = TRUE) +
  theme_bw()

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..),
               formula = formula, parse = TRUE, label.y.npc = "center") +
  theme_bw()

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..),
               formula = formula, parse = TRUE, label.y.npc = 0.75) +
  theme_bw()

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, fill = block)) +
  geom_point(shape = 21, size = 3) +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.33,
               formula = formula, parse = TRUE) +
  facet_wrap(~group, scales = "free_y") +
  theme_bw()

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group, fill = block)) +
  geom_point(shape = 21, size = 3) +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula, parse = TRUE,
               label.y.npc = 0.66) +
  facet_wrap(~group, scales = "free_y") +
  theme_bw()

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_fit_residuals(formula = formula)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_fit_residuals(formula = formula, resid.type = "working")

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y, color = group)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_fit_residuals(formula = formula)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_deviations(formula = formula, color = "red") +
  geom_point()

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y, color = group)) +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_deviations(formula = formula) +
  geom_point()

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_deviations(formula = formula, color = "red",
                      arrow = arrow(length = unit(0.015, "npc"), 
                                   ends = "both")) +
  geom_point()

## ------------------------------------------------------------------------
# formula <- y ~ poly(x, 3, raw = TRUE)
# broom::augment does not handle poly correctly!
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_glance(method = "lm", 
                  method.args = list(formula = formula),
                  geom = "text",
                  aes(label = signif(..p.value.., digits = 4)))

## ------------------------------------------------------------------------
# formula <- y ~ poly(x, 3, raw = TRUE)
# broom::augment does not handle poly() correctly!
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y, color = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_glance(method = "lm", 
                  method.args = list(formula = formula),
                  geom = "text", 
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")))

## ------------------------------------------------------------------------
# formula <- y ~ poly(x, 3, raw = TRUE)
# broom::augment does not handle poly correctly!
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y, color = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_glance(method = "lm", 
                  method.args = list(formula = formula),
                  label.x.npc = "right",
                  label.y.npc = "bottom",
                  geom = "text", 
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")))

## ------------------------------------------------------------------------
# formula <- y ~ poly(x, 3, raw = TRUE)
# broom::augment does not handle poly correctly!
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_fit_augment(method = "lm",
                   method.args = list(formula = formula))

## ------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y, color = group)) +
  geom_point() +
  stat_fit_augment(method = "lm", 
                   method.args = list(formula = formula))

## ------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  stat_fit_augment(method = "lm",
                   method.args = list(formula = formula),
                   geom = "point",
                   y.out = ".resid")

## ------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y, color = group)) +
  stat_fit_augment(method = "lm",
                   method.args = list(formula = formula),
                   geom = "point",
                   y.out = ".std.resid")

## ------------------------------------------------------------------------
args <- list(formula = y ~ k * e ^ x,
             start = list(k = 1, e = 2))
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  stat_fit_augment(method = "nls",
                   method.args = args)

## ------------------------------------------------------------------------
args <- list(formula = y ~ k * e ^ x,
             start = list(k = 1, e = 2))
ggplot(mtcars, aes(wt, mpg)) +
  stat_fit_augment(method = "nls",
                   method.args = args,
                   geom = "point",
                   y.out = ".resid")

## ------------------------------------------------------------------------
args <- list(model = y ~ SSlogis(x, Asym, xmid, scal),
             fixed = Asym + xmid + scal ~1,
             random = Asym ~1 | group,
             start = c(Asym = 200, xmid = 725, scal = 350))
ggplot(Orange, aes(age, circumference, color = Tree)) +
  geom_point() +
  stat_fit_augment(method = "nlme",
                   method.args = args,
                   augment.args = list(data = quote(data)))

## ------------------------------------------------------------------------
random_string <- function(len = 6) {
paste(sample(letters, len, replace = TRUE), collapse = "")
}

# Make random data.
set.seed(1001)
d <- tibble::tibble(
  x = rnorm(100),
  y = rnorm(100),
  group = rep(c("A", "B"), c(50, 50)),
  lab = replicate(100, { random_string() })
)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red")

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, color = group)) +
   stat_dens2d_filter(keep.fraction = 0.25,
                      size = 3,
                      color = "black") +
   geom_point()

## ------------------------------------------------------------------------
ggplot(data = d, aes(x + rep(c(-2,2), rep(50,2)), 
                     y, color = group)) +
   geom_point() +
   stat_dens2d_filter(shape = 1, size = 3,
                      keep.fraction = 0.25)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x + rep(c(-2,2), rep(50,2)), 
                     y, color = group)) +
   geom_point() +
   stat_dens2d_filter_g(shape = 1, size = 3,
                      keep.fraction = 0.25)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
  geom_point() + 
  stat_dens2d_filter(geom = "text_repel")

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
  geom_point() +
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.5)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
  geom_point() +
  stat_dens2d_labels()

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
  geom_point() +
  stat_dens2d_labels(keep.fraction = 0.45)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
  geom_point() +
  stat_dens2d_labels(keep.fraction = 0.25,
                     vjust = -0.3)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
  geom_point() +
  stat_dens2d_labels(geom = "text_repel", 
                     keep.fraction = 0.45)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
  geom_point() +
  stat_dens2d_labels(geom = "label_repel", 
                     keep.fraction = 0.25)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
geom_point() +
stat_dens2d_labels(geom = "text_repel",
keep.fraction = 0.25, angle = 90)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
  geom_point() +
  stat_dens2d_labels(geom = "label_repel", 
                     keep.fraction = 0.35, 
                     alpha = 0.5,
                     label.fill = NA)

