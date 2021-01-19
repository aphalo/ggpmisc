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

