## ---- include=FALSE, echo=FALSE-----------------------------------------------
library(knitr)
opts_chunk$set(fig.align = 'center', 
               fig.show = 'hold', fig.width = 7, fig.height = 4)
options(warnPartialMatchArgs = FALSE,
        tibble.print.max = 4,
        tibble.print.min = 4,
        dplyr.summarise.inform = FALSE)

## ---- message=FALSE-----------------------------------------------------------
library(ggpmisc)
library(ggrepel)
library(xts)
library(lubridate)
library(tibble)
library(dplyr)
library(nlme)

## -----------------------------------------------------------------------------
old_theme <- theme_set(theme_bw())

## -----------------------------------------------------------------------------
class(lynx)
ggplot(lynx) + geom_line()

## -----------------------------------------------------------------------------
ggplot(lynx, as.numeric = FALSE) + geom_line()

## -----------------------------------------------------------------------------
tb <- mpg %>%
  group_by(cyl) %>%
  summarise(hwy = median(hwy), cty = median(cty))

data.tb <- tibble(x = 7, y = 44, tb = list(tb))

ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_table(data = data.tb, aes(x, y, label = tb)) +
  geom_point() 

## ---- eval=FALSE--------------------------------------------------------------
#  tb <- mpg %>%
#    group_by(cyl) %>%
#    summarise(hwy = median(hwy), cty = median(cty))
#  
#  ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
#    annotate("table", x = 7, y = 44, label = tb) +
#    geom_point()

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_table(data = data.tb, aes(x, y, label = tb),
             table.theme = ttheme_gtsimple,
             table.hjust = 0, colour = "darkred", fill = "#FFFFBB") +
  geom_point() 

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_table(data = data.tb, aes(x, y, label = tb),
             table.theme = ttheme_gtlight,
             size = 3, colour = "darkblue",
             stat = "fmt_tb", 
             tb.vars = c(Cylinders = "cyl", MPG = "hwy"), # rename
             tb.rows = 4:1) + # change order
  labs(x = "Engine displacement (l)", y = "Fuel use efficiency (MPG)",
       colour = "Engine cylinders\n(number)") +
  geom_point() +
  theme_bw()


## -----------------------------------------------------------------------------
tb.pm <- tibble(Parameter = c("frac(beta[1], a^2)", "frac(beta[2], a^3)"),
                Value = c("10^2.4", "10^3.532"))
data.tb <- tibble(x = 7, y = 44, tb = list(tb.pm))
ggplot(mpg, aes(displ, cty)) +
  geom_point() +
  geom_table(data = data.tb, aes(x, y, label = tb), parse = TRUE) +
  theme_bw()

## -----------------------------------------------------------------------------
p <- ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_point() 

data.tb <- 
  tibble(x = 7, y = 44, 
         plot = list(p + 
                       coord_cartesian(xlim = c(4.9, 6.2), 
                                       ylim = c(13, 21)) +
                       labs(x = NULL, y = NULL) +
                       theme_bw(8) +
                       scale_colour_discrete(guide = FALSE)))

ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_plot(data = data.tb, aes(x, y, label = plot)) +
  annotate(geom = "rect", 
           xmin = 4.9, xmax = 6.2, ymin = 13, ymax = 21,
           linetype = "dotted", fill = NA, colour = "black") +
  geom_point() 

## -----------------------------------------------------------------------------
p <- ggplot(mpg, aes(factor(cyl), hwy, fill = factor(cyl))) +
  stat_summary(geom = "col", fun = mean, width = 2/3) +
  labs(x = "Number of cylinders", y = NULL, title = "Means") +
  scale_fill_discrete(guide = FALSE)

data.tb <- tibble(x = 7, y = 44, 
                  plot = list(p +
                                theme_bw(8)))

ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_plot(data = data.tb, aes(x, y, label = plot)) +
  geom_point() +
  labs(x = "Engine displacement (l)", y = "Fuel use efficiency (MPG)",
       colour = "Engine cylinders\n(number)") +
  theme_bw()

## ---- eval=FALSE--------------------------------------------------------------
#  p <- ggplot(mpg, aes(factor(cyl), hwy, fill = factor(cyl))) +
#    stat_summary(geom = "col", fun = mean, width = 2/3) +
#    labs(x = "Number of cylinders", y = NULL, title = "Means") +
#    scale_fill_discrete(guide = FALSE)
#  
#  ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
#    annotate("plot", x = 7, y = 44, label = p + theme_bw(8)) +
#    geom_point() +
#    labs(x = "Engine displacement (l)", y = "Fuel use efficiency (MPG)",
#         colour = "Engine cylinders\n(number)") +
#    theme_bw()

## -----------------------------------------------------------------------------
file.name <- 
  system.file("extdata", "Isoquercitin.png", 
              package = "ggpmisc", mustWork = TRUE)
Isoquercitin <- magick::image_read(file.name)
grobs.tb <- tibble(x = c(0, 10, 20, 40), y = c(4, 5, 6, 9),
                   width = c(0.05, 0.05, 0.01, 1),
                   height =  c(0.05, 0.05, 0.01, 0.3),
                   grob = list(grid::circleGrob(), 
                               grid::rectGrob(), 
                               grid::textGrob("I am a Grob"),
                               grid::rasterGrob(image = Isoquercitin)))

ggplot() +
  geom_grob(data = grobs.tb, 
            aes(x, y, label = grob, vp.width = width, vp.height = height),
            hjust = 0.7, vjust = 0.55) +
  scale_y_continuous(expand = expansion(mult = 0.3, add = 0)) +
  scale_x_continuous(expand = expansion(mult = 0.2, add = 0)) +
 theme_bw(12)

## -----------------------------------------------------------------------------
ggplot() +
  annotate("grob", x = 1, y = 3, vp.width = 0.5,
           label = grid::rasterGrob(image = Isoquercitin, width = 1)) +
 theme_bw(12)

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_vhlines(xintercept = c(2.75, 4), yintercept = 27, linetype = "dashed") +
  geom_point() +
  labs(x = "Engine displacement (l)", y = "Fuel use efficiency (MPG)",
       colour = "Engine cylinders\n(number)") +
  theme_bw()


## -----------------------------------------------------------------------------
file.name <- 
  system.file("extdata", "Robinin.png", 
              package = "ggpmisc", mustWork = TRUE)
Robinin <- magick::image_read(file.name)

set.seed(123456)
data.tb <- tibble(x = 1:20, y = (1:20) + rnorm(20, 0, 10))

flavo.tb <- tibble(x = 0.02,
                   y = 0.95,
                   width = 1/2,
                   height = 1/4,
                   grob = list(grid::rasterGrob(image = Robinin)))

ggplot(data.tb, aes(x, y)) +
  geom_grob_npc(data = flavo.tb, 
                aes(label = grob, npcx = x, npcy = y, 
                    vp.width = width, vp.height = height)) +
  geom_point() +
  expand_limits(y = 55, x = 0)

## -----------------------------------------------------------------------------
ggplot(data.tb, aes(x, y)) +
  geom_grob_npc(label = list(grid::rasterGrob(image = Robinin, width = 1)), 
                npcx = 0.02, npcy = 0.95,
                vp.width = 1/2, vp.height = 1/4) +
  geom_point() +
  expand_limits(y = 55, x = 0)

## -----------------------------------------------------------------------------
ggplot(data.tb, aes(x, y)) +
  annotate("grob_npc", label = grid::rasterGrob(image = Robinin, width = 1), 
                npcx = 0.02, npcy = 0.95, vp.width = 1/2, vp.height = 1/4) +
  geom_point() +
  expand_limits(y = 55, x = 0)


## -----------------------------------------------------------------------------
corner_letters.tb <- tibble(label = LETTERS[1:4],
                            x = "left", 
                            y = "top",
                            cyl = c(4,5,6,8))
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~cyl, scales = "free") +
  geom_text_npc(data = corner_letters.tb,
                aes(npcx = x, npcy = y, label = label)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

## -----------------------------------------------------------------------------
data.tb <- mpg %>%
  group_by(cyl) %>%
  summarise(hwy = median(hwy), displ = median(displ))
ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_x_margin_point(data = data.tb,
                      aes(xintercept = displ, fill = factor(cyl))) +
  expand_limits(y = 10) +
  geom_point() 

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_point(alpha = 0.33) +
  stat_centroid(shape = "cross", size = 4)

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_point(alpha = 0.33) +
  stat_centroid(shape = "cross", size = 4, .fun = median)

## -----------------------------------------------------------------------------
ggplot(lynx, as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", vjust = -0.5, 
             check_overlap = TRUE) +
  ylim(-100, 7300)

## -----------------------------------------------------------------------------
ggplot(lynx) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", hjust = -0.2, vjust = 0.5, 
             angle = 90, check_overlap = TRUE) +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", hjust = 1.2, vjust = 0.5, 
             angle = 90, check_overlap = TRUE) +
  expand_limits(y = c(-900, 8000))

## -----------------------------------------------------------------------------
ggplot(lynx, as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", hjust = -0.2, vjust = 0.5, 
             angle = 90, check_overlap = TRUE, x.label.fmt = "%Y") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", hjust = 1.2, vjust = 0.5, 
             angle = 90, check_overlap = TRUE, x.label.fmt = "%Y") +
  expand_limits(y = c(-900, 8000))

## -----------------------------------------------------------------------------
ggplot(lynx, as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "rug", colour = "red") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "rug", colour = "blue")

## -----------------------------------------------------------------------------
set.seed(4321)
# generate artificial data
x <- -99:100
y <- x + rnorm(length(x), mean = 0, sd = abs(x))
my.data <- data.frame(x, 
                      y, 
                      group = c("A", "B"))

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_quadrant_lines(colour = "red") +
  stat_quadrant_counts(colour = "red") +
  geom_point() +
  expand_limits(y = c(-250, 250))

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_quadrant_lines(colour = "red", pool.along = "x") +
  stat_quadrant_counts(colour = "red", pool.along = "x") +
  geom_point()

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_quadrant_counts(quadrants = 0L, label.x = "left", 
                       aes(label = sprintf("%i observations", stat(count))))

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_quadrant_lines(colour = "red") +
  stat_quadrant_counts(colour = "red", quadrants = c(2, 4)) +
  geom_point()

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y, colour = group)) +
  geom_quadrant_lines() +
  stat_quadrant_counts(geom = "label_npc") +
  geom_point() +
  expand_limits(y = c(-260, 260)) +
  facet_wrap(~group)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_smooth(method = "lm", formula = formula) +
  stat_poly_eq(formula = formula, parse = TRUE)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = stat(eq.label)), formula = formula, 
               parse = TRUE)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = stat(adj.rr.label)), formula = formula, 
               parse = TRUE) +
  stat_poly_eq(aes(label = stat(AIC.label)),
               label.x = "right", label.y = "bottom", size = 3,     
               formula = formula, 
               parse = TRUE)


## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), 
                                  sep = "*\", \"*")),
               formula = formula, parse = TRUE) +
  labs(x = expression(italic(x)), y = expression(italic(y)))

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label =  paste(stat(eq.label), "*\" with \"*", 
                                  stat(rr.label), "*\", \"*", 
                                  stat(f.value.label), "*\", and \"*",
                                  stat(p.value.label), "*\".\"",
                                  sep = "")),
               formula = formula, parse = TRUE, size = 3)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), 
                                  sep = "~~italic(\"with\")~~")),
               formula = formula, parse = TRUE)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = paste("atop(", stat(AIC.label), ",", stat(BIC.label), ")", sep = "")), 
               formula = formula, 
               parse = TRUE)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = stat(eq.label)),
               eq.with.lhs = FALSE,
               formula = formula, parse = TRUE)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = stat(eq.label)),
               eq.with.lhs = "italic(hat(y))~`=`~",
               formula = formula, parse = TRUE)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  labs(x = expression(italic(z)), y = expression(italic(h)) ) + 
  stat_poly_eq(aes(label = stat(eq.label)),
               eq.with.lhs = "italic(h)~`=`~",
               eq.x.rhs = "~italic(z)",
               formula = formula, parse = TRUE)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 2, raw = TRUE)
ggplot(my.data, aes(x, log10(y + 1e6))) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = stat(eq.label)),
               eq.with.lhs = "plain(log)[10](italic(delta)+10^6)~`=`~",
               eq.x.rhs = "~Omega",
               formula = formula, parse = TRUE) +
  labs(y = expression(plain(log)[10](italic(delta)+10^6)), x = expression(Omega))

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 5, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = stat(eq.label)), formula = formula, parse = TRUE)

## -----------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3) - 1
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = stat(eq.label)), formula = formula, 
               parse = TRUE)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = stat(eq.label)), size = 3,
               formula = formula, parse = TRUE) +
  facet_wrap(~group)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = stat(eq.label)), size = 3,
               formula = formula, parse = TRUE) +
  facet_wrap(~group, scales = "free_y")

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = stat(eq.label)),
               formula = formula, parse = TRUE)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group, grp.label = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = stat(paste("bold(", grp.label, "*\":\")~~", 
                                      eq.label, sep = ""))),
               formula = formula, parse = TRUE)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, linetype = group, grp.label = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula, color = "black") +
  stat_poly_eq(aes(label = stat(paste("bold(", grp.label, "*':')~~~", 
                                      eq.label, sep = ""))),
               formula = formula, parse = TRUE)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = stat(eq.label)),
               formula = formula, parse = TRUE,
               label.x = "centre")

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, fill = block)) +
  geom_point(shape = 21, size = 3) +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = stat(rr.label)), size = 3,
               geom = "label_npc", alpha = 0.33,
               formula = formula, parse = TRUE) +
  facet_wrap(~group, scales = "free_y")

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group, fill = block)) +
  geom_point(shape = 21, size = 3) +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = stat(rr.label)), size = 3, alpha = 0.2,
               geom = "label_npc", label.y = c(0.95, 0.85, 0.95, 0.85),
               formula = formula, parse = TRUE) +
  facet_wrap(~group, scales = "free_y")

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(geom = "text", aes(label = stat(eq.label)),
               label.x = c(100, 90), label.y = c(-1e4, 2.1e6), hjust = "inward",
               formula = formula, parse = TRUE)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y, colour = group)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_fit_residuals(formula = formula)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_deviations(formula = formula, colour = "red") +
  geom_point()

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_smooth(method = "lm", formula = formula) +
  geom_point() +
  stat_fit_deviations(formula = formula, colour = "red",
                      arrow = arrow(length = unit(0.015, "npc"), 
                                   ends = "both"))

## -----------------------------------------------------------------------------
# formula <- y ~ poly(x, 3, raw = TRUE)
# broom::augment does not handle poly() correctly!
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y, colour = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_glance(method = "lm", 
                  method.args = list(formula = formula),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(stat(p.value), digits = 4),
                                    sep = "")),
                  parse = TRUE)

## -----------------------------------------------------------------------------
micmen.formula <- y ~ SSmicmen(x, Vm, K) 
ggplot(Puromycin, aes(conc, rate, colour = state)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = micmen.formula,
              se = FALSE) +
  stat_fit_glance(method = "nls", 
                  method.args = list(formula = micmen.formula),
                  aes(label = paste("AIC = ", signif(stat(AIC), digits = 3), 
                                    ", BIC = ", signif(stat(BIC), digits = 3),
                                    sep = "")),
                  label.x = "centre", label.y = "bottom")

## -----------------------------------------------------------------------------
micmen.formula <- y ~ SSmicmen(x, Vm, K) 
ggplot(Puromycin, aes(conc, rate, colour = state)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = micmen.formula,
              se = FALSE) +
  stat_fit_tidy(method = "nls", 
                method.args = list(formula = micmen.formula),
                label.x = "right",
                label.y = "bottom",
                aes(label = paste("V[m]~`=`~", signif(stat(Vm_estimate), digits = 3),
                                  "%+-%", signif(stat(Vm_se), digits = 2),
                                  "~~~~K~`=`~", signif(stat(K_estimate), digits = 3),
                                  "%+-%", signif(stat(K_se), digits = 2),
                                  sep = "")),
                parse = TRUE)

## -----------------------------------------------------------------------------
micmen.formula <- y ~ SSmicmen(x, Vm, K) 
ggplot(Puromycin, aes(conc, rate, colour = state)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = micmen.formula,
              se = FALSE) +
  stat_fit_tidy(method = "nls", 
                method.args = list(formula = micmen.formula),
                size = 3,
                label.x = "center",
                label.y = "bottom",
                vstep = 0.18,
                aes(label = paste("V~`=`~frac(", signif(stat(Vm_estimate), digits = 2), "~C,",
                                  signif(stat(K_estimate), digits = 2), "+C)",
                                  sep = "")),
                parse = TRUE) +
  labs(x = "C", y = "V")

## -----------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_tb(method = "lm",
              method.args = list(formula = formula),
              tb.vars = c(Parameter = "term", 
                          Estimate = "estimate", 
                          "s.e." = "std.error", 
                          "italic(t)" = "statistic", 
                          "italic(P)" = "p.value"),
              label.y = "top", label.x = "left",
              parse = TRUE)

## -----------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_tb(method = "lm",
              method.args = list(formula = formula),
              tb.type = "fit.anova",
              tb.vars = c(Effect = "term", 
                          df = "df",
                          "italic(F)" = "statistic", 
                          "italic(P)" = "p.value"),
              tb.params = c(x = 1, "x^2" = 2, "x^3" = 3, Resid = 4),
              label.y = "top", label.x = "left",
              parse = TRUE)

## -----------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_tb(method = "lm",
              method.args = list(formula = formula),
              tb.type = "fit.coefs",
              label.y = "center", label.x = "left")

## -----------------------------------------------------------------------------
micmen.formula <- y ~ SSmicmen(x, Vm, K)
ggplot(Puromycin, aes(conc, rate, colour = state)) +
  facet_wrap(~state) +
  geom_point() +
  geom_smooth(method = "nls",
              formula = micmen.formula,
              se = FALSE) +
  stat_fit_tb(method = "nls",
              method.args = list(formula = micmen.formula),
              tb.type = "fit.coefs",
              label.x = 0.9,
              label.y = c(0.75, 0.2)) +
  theme(legend.position = "none") +
  labs(x = "C", y = "V")

## -----------------------------------------------------------------------------
ggplot(chickwts, aes(factor(feed), weight)) +
  stat_summary(fun.data = "mean_se") +
  stat_fit_tb(tb.type = "fit.anova",
              label.x = "center",
              label.y = "bottom") +
  expand_limits(y = 0)

## -----------------------------------------------------------------------------
ggplot(chickwts, aes(factor(feed), weight)) +
  stat_summary(fun.data = "mean_se") +
  stat_fit_tb(tb.type = "fit.anova", label.x = "left", size = 3) +
  scale_x_discrete(expand = expansion(mult = c(0.2, 0.5))) +
  coord_flip()

## -----------------------------------------------------------------------------
ggplot(chickwts, aes(factor(feed), weight)) +
  stat_summary(fun.data = "mean_se") +
  stat_fit_tb(tb.type = "fit.anova",
              angle = 90, size = 3,
              label.x = "right", label.y = "center",
              hjust = 0.5, vjust = 0,
              tb.vars = c(Effect = "term", 
                          "df",
                          "M.S." = "meansq", 
                          "italic(F)" = "statistic", 
                          "italic(P)" = "p.value"),
              parse = TRUE) +
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.35))) +
  expand_limits(y = 0)

## -----------------------------------------------------------------------------
# formula <- y ~ poly(x, 3, raw = TRUE)
# broom::augment does not handle poly correctly!
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_fit_augment(method = "lm",
                   method.args = list(formula = formula))

## -----------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y, colour = group)) +
  geom_point() +
  stat_fit_augment(method = "lm", 
                   method.args = list(formula = formula))

## -----------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  stat_fit_augment(method = "lm",
                   method.args = list(formula = formula),
                   geom = "point",
                   y.out = ".resid")

## -----------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y, colour = group)) +
  stat_fit_augment(method = "lm",
                   method.args = list(formula = formula),
                   geom = "point",
                   y.out = ".std.resid")

## -----------------------------------------------------------------------------
args <- list(formula = y ~ k * e ^ x,
             start = list(k = 1, e = 2))
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  stat_fit_augment(method = "nls",
                   method.args = args)

## -----------------------------------------------------------------------------
args <- list(formula = y ~ k * e ^ x,
             start = list(k = 1, e = 2))
ggplot(mtcars, aes(wt, mpg)) +
  stat_fit_augment(method = "nls",
                   method.args = args,
                   geom = "point",
                   y.out = ".resid")

## ---- echo=FALSE, eval=FALSE--------------------------------------------------
#  # augment no longer available for "nlme"
#  args <- list(model = y ~ SSlogis(x, Asym, xmid, scal),
#               fixed = Asym + xmid + scal ~1,
#               random = Asym ~1 | group,
#               start = c(Asym = 200, xmid = 725, scal = 350))
#  ggplot(Orange, aes(age, circumference, colour = Tree)) +
#    geom_point() +
#    stat_fit_augment(method = "nlme",
#                     method.args = args,
#                     augment.args = list(data = quote(data)))

## -----------------------------------------------------------------------------
ggplot(Orange, aes(age, circumference, colour = Tree)) +
  stat_apply_group(.fun.y = function(x) {c(NA, diff(x))}, na.rm = TRUE)

## -----------------------------------------------------------------------------
random_string <- function(len = 3) {
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

## -----------------------------------------------------------------------------
ggplot(data = d, aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(keep.fraction = 1/4, colour = "red")

## -----------------------------------------------------------------------------
ggplot(data = d, aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(keep.fraction = 1/4, keep.number = 50, colour = "red")

## -----------------------------------------------------------------------------
ggplot(data = d, aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(keep.fraction = 1, keep.number = 50, colour = "red")

## -----------------------------------------------------------------------------
ggplot(data = d, aes(x, y, colour = group)) +
   stat_dens2d_filter(keep.fraction = 0.25,
                      size = 3,
                      colour = "black") +
   geom_point()

## -----------------------------------------------------------------------------
ggplot(data = d, aes(x + rep(c(-2,2), rep(50,2)), 
                     y, colour = group)) +
   geom_point() +
   stat_dens2d_filter(shape = 1, size = 3,
                      keep.fraction = 0.25)

## -----------------------------------------------------------------------------
ggplot(data = d, aes(x + rep(c(-2,2), rep(50,2)), 
                     y, colour = group)) +
   geom_point() +
   stat_dens2d_filter_g(shape = 1, size = 3,
                      keep.fraction = 0.25)

## -----------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, colour = group)) +
  stat_dens2d_labels(keep.fraction = 1/10, 
                     hjust = "outward", vjust = "outward") +
  geom_point()

## -----------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, colour = group)) +
  geom_point() +
  stat_dens2d_labels(geom = "text_repel", 
                     keep.fraction = 0.45)

## -----------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, colour = group)) +
  stat_dens2d_labels(geom = "label_repel", 
                     keep.fraction = 0.35, 
                     label.fill = NA) +
    geom_point()

## ---- eval = FALSE------------------------------------------------------------
#  random_string <- function(len = 6) {
#  paste(sample(letters, len, replace = TRUE), collapse = "")
#  }
#  
#  # Make random data.
#  set.seed(1001)
#  d <- tibble::tibble(
#    x = rnorm(100),
#    y = rnorm(100),
#    group = rep(c("A", "B"), c(50, 50)),
#    lab = replicate(100, { random_string() })
#  )

## -----------------------------------------------------------------------------
ggplot(data = d, aes(x, y)) +
  geom_point() +
  stat_dens1d_filter(keep.fraction = 0.25,
                     colour = "red")

## -----------------------------------------------------------------------------
ggplot(data = d, aes(x, y)) +
  geom_point() +
  stat_dens1d_filter(keep.fraction = 0.25,
                     colour = "red",
                     orientation = "y")

## -----------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, colour = group)) +
  geom_point() +
  stat_dens1d_labels(geom = "text_repel")

## -----------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, colour = group)) +
  geom_point() +
  stat_dens1d_labels(geom = "text_repel", orientation = "y")

## -----------------------------------------------------------------------------
head(volcano_example.df) 

## -----------------------------------------------------------------------------
volcano_example.df %>%
  mutate(., outcome.fct = outcome2factor(outcome)) %>%
  ggplot(., aes(logFC, PValue, colour = outcome.fct)) +
  geom_point() +
  scale_x_logFC(name = "Transcript abundance%unit") +
  scale_y_Pvalue() +
  scale_colour_outcome() +
  stat_quadrant_counts(data = . %>% filter(outcome != 0))

## -----------------------------------------------------------------------------
volcano_example.df %>%
  mutate(., outcome.fct = outcome2factor(outcome, n.levels = 2)) %>%
  ggplot(., aes(logFC, PValue, colour = outcome.fct)) +
  geom_point() +
  scale_x_logFC(name = "Transcript abundance%unit") +
  scale_y_Pvalue() +
  scale_colour_outcome() +
  stat_quadrant_counts(data = . %>% filter(outcome != 0))

## -----------------------------------------------------------------------------
head(quadrant_example.df)

## -----------------------------------------------------------------------------
quadrant_example.df %>%
  mutate(.,
         outcome.x.fct = outcome2factor(outcome.x),
         outcome.y.fct = outcome2factor(outcome.y),
         outcome.xy.fct = xy_outcomes2factor(outcome.x, outcome.y)) %>%
         filter(outcome.xy.fct != "none") %>%
  ggplot(., aes(logFC.x, logFC.y, colour = outcome.x.fct, fill = outcome.y.fct)) +
  geom_quadrant_lines(linetype = "dotted") +
  stat_quadrant_counts(size = 3, colour = "white") +
  geom_point(shape = "circle filled") +
  scale_x_logFC(name = "Transcript abundance for x%unit") +
  scale_y_logFC(name = "Transcript abundance for y%unit") +
  scale_colour_outcome() +
  scale_fill_outcome() +
  theme_dark()

## -----------------------------------------------------------------------------
all_quadrant_counts <- function(...) {
  list(  
    stat_quadrant_counts(data = . %>% filter(outcome.xy.fct == "xy"), ...),
    stat_quadrant_counts(data = . %>% filter(outcome.xy.fct == "x"), pool.along = "y", ...),
    stat_quadrant_counts(data = . %>% filter(outcome.xy.fct == "y"), pool.along = "x", ...),
    stat_quadrant_counts(data = . %>% filter(outcome.xy.fct == "none"), quadrants = 0L, ...)
  )
}

## -----------------------------------------------------------------------------
all_quadrant_lines <- function(...) { 
  list(
    geom_hline(data =  data.frame(outcome.xy.fct = factor(c("xy", "x", "y", "none"),
                                                          levels = c("xy", "x", "y", "none")),
                                  yintercept = c(0, NA, 0, NA)),
               aes_(yintercept = ~yintercept),
               na.rm = TRUE,
               ...),
    geom_vline(data =  data.frame(outcome.xy.fct = factor(c("xy", "x", "y", "none"),
                                                          levels = c("xy", "x", "y", "none")),
                                  xintercept = c(0, 0, NA, NA)),
               aes_(xintercept = ~xintercept),
               na.rm = TRUE,
               ...)
  )
}

## -----------------------------------------------------------------------------
quadrant_example.df %>%
  mutate(.,
         outcome.x.fct = outcome2factor(outcome.x),
         outcome.y.fct = outcome2factor(outcome.y),
         outcome.xy.fct = xy_outcomes2factor(outcome.x, outcome.y)) %>%
  ggplot(., aes(logFC.x, logFC.y, colour = outcome.x.fct, fill = outcome.y.fct)) +
  geom_point(shape = 21) +
  all_quadrant_lines(linetype = "dotted") +
  all_quadrant_counts(size = 3, colour = "white") +
  scale_x_logFC(name = "Transcript abundance for x%unit") +
  scale_y_logFC(name = "Transcript abundance for y%unit") +
  scale_colour_outcome() +
  scale_fill_outcome() +
  facet_wrap(~outcome.xy.fct) +
  theme_dark()

## -----------------------------------------------------------------------------
class(austres)
austres.df <- try_tibble(austres)
class(austres.df)
lapply(austres.df, "class")
head(austres.df, 4)

## -----------------------------------------------------------------------------
austres.df <- try_tibble(austres, as.numeric = TRUE)
lapply(austres.df, "class")
head(austres.df, 4)

## -----------------------------------------------------------------------------
class(lynx)
lynx.df <- try_tibble(lynx)
class(lynx.df)
lapply(lynx.df, "class")
head(lynx.df, 3)

## -----------------------------------------------------------------------------
lynx.df <- try_tibble(lynx, "year")
head(lynx.df, 3)

## -----------------------------------------------------------------------------
lynx_n.df <- try_tibble(lynx, "year", as.numeric = TRUE)
lapply(lynx_n.df, "class")
head(lynx_n.df, 3)

## -----------------------------------------------------------------------------
try_tibble(1:5)

## -----------------------------------------------------------------------------
try_tibble(letters[1:5])

## -----------------------------------------------------------------------------
try_tibble(factor(letters[1:5]))

## -----------------------------------------------------------------------------
try_tibble(list(x = rep(1,5), y = 1:5))

## -----------------------------------------------------------------------------
try_tibble(data.frame(x = rep(1,5), y = 1:5))

## -----------------------------------------------------------------------------
try_tibble(matrix(1:10, ncol = 2))

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
old_theme <- theme_set(theme_bw())

## -----------------------------------------------------------------------------
ggplot(data = make_data_tbl(300), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(colour = "red", 
                     keep.sparse = FALSE, 
                     keep.fraction = 1/3)

## -----------------------------------------------------------------------------
ggplot(data = make_data_tbl(300), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(colour = "red", 
                     keep.sparse = FALSE, 
                     keep.fraction = 1/3)+
  stat_dens2d_filter(colour = "blue", 
                     keep.fraction = 1/3)

## -----------------------------------------------------------------------------
ggplot(data = make_data_tbl(300, rfun = runif), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(colour = "red", keep.fraction = 1/2)

## -----------------------------------------------------------------------------
ggplot(data = make_data_tbl(300, rfun = rgamma, shape = 2), 
       aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(colour = "red", keep.fraction = 1/3)

