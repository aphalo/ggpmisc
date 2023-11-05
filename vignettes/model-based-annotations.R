## ----include=FALSE, echo=FALSE------------------------------------------------
library(knitr)
opts_chunk$set(fig.align = 'center', 
               fig.show = 'hold', fig.width = 7, fig.height = 4)
options(warnPartialMatchArgs = FALSE,
        tibble.print.max = 4,
        tibble.print.min = 4,
        dplyr.summarise.inform = FALSE)
eval_flag <- TRUE # evaluate all code chunks

## ----message=FALSE------------------------------------------------------------
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

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(formula = formula)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("eq"), formula = formula)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = aes(label = after_stat(eq.label)), 
               formula = formula)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("adj.R2"), formula = formula) +
  stat_poly_eq(mapping = use_label("AIC"), label.x = "right", label.y = "bottom", size = 3,
               formula = formula)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label(c("eq", "adj.R2")),
               formula = formula) +
  labs(x = expression(italic(x)), y = expression(italic(y)))

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label =  paste(after_stat(eq.label), "*\" with \"*", 
                                  after_stat(rr.label), "*\", \"*", 
                                  after_stat(f.value.label), "*\", and \"*",
                                  after_stat(p.value.label), "*\".\"",
                                  sep = "")),
               formula = formula, size = 3)

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label(c("eq", "adj.R2"), sep = "~~italic(\"with\")~~"),
               formula = formula)

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label = paste("atop(", after_stat(AIC.label), ",", after_stat(BIC.label), ")", sep = "")), 
               formula = formula)

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("eq"),
               eq.with.lhs = FALSE,
               formula = formula)

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("eq"),
               eq.with.lhs = "italic(hat(y))~`=`~",
               formula = formula)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label(c("eq", "R2")),
               eq.with.lhs = "italic(h)~`=`~",
               eq.x.rhs = "~italic(z)",
               formula = formula) +
  labs(x = expression(italic(z)), y = expression(italic(h)))

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 2, raw = TRUE)
ggplot(my.data, aes(x, log10(y + 1e6))) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("eq"),
               eq.with.lhs = "plain(log)[10](italic(delta)+10^6)~`=`~",
               eq.x.rhs = "~Omega",
               formula = formula) +
  labs(y = expression(plain(log)[10](italic(delta)+10^6)), x = expression(Omega))

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 5, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("eq"), formula = formula)

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3) - 1
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label = after_stat(eq.label)), formula = formula)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label =  ifelse(after_stat(adj.r.squared > 0.3),
                                   paste(after_stat(eq.label), after_stat(adj.rr.label), 
                                         sep = "*\", \"*"),
                                   after_stat(adj.rr.label))),
               formula = formula) +
  labs(x = expression(italic(x)), y = expression(italic(y)))

## -----------------------------------------------------------------------------
poly_or_mean <- function(formula, data, ...) {
   fm <- lm(formula = formula, data = data, ...)
   if (anova(fm)[["Pr(>F)"]][1] > 0.1) {
      lm(formula = y ~ 1, data = data, ...)
   } else {
      fm
   }
}

## -----------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy)) +
   geom_point() +
   stat_poly_line(method = "poly_or_mean") +
   stat_poly_eq(method = poly_or_mean,
   aes(label = after_stat(eq.label)),
   label.x = "right") +
   theme(legend.position = "bottom") +
   facet_wrap(~class, ncol = 2)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label = after_stat(eq.label)), size = 3,
               formula = formula) +
  facet_wrap(~group)

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label = after_stat(eq.label)), size = 3,
               formula = formula) +
  facet_wrap(~group, scales = "free_y")

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label = after_stat(eq.label)), formula = formula, vstep = 0.06)

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group, grp.label = group)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label = after_stat(paste("bold(", grp.label, "*\":\")~~", 
                                      eq.label, sep = ""))),
               formula = formula)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, linetype = group, grp.label = group)) +
  geom_point() +
  stat_poly_line(formula = formula, color = "black") +
  stat_poly_eq(aes(label = after_stat(paste("bold(", grp.label, "*':')~~~", 
                                      eq.label, sep = ""))),
               formula = formula)

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label = after_stat(eq.label)),
               formula = formula,
               label.x = "centre")

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, fill = block)) +
  geom_point(shape = 21, size = 3) +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label = after_stat(rr.label)), size = 3,
               geom = "label_npc", alpha = 0.33,
               formula = formula) +
  facet_wrap(~group, scales = "free_y")

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group, fill = block)) +
  geom_point(shape = 21, size = 3) +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label = after_stat(rr.label)), size = 3, alpha = 0.2,
               geom = "label_npc", label.y = c(0.95, 0.85, 0.95, 0.85),
               formula = formula) +
  facet_wrap(~group, scales = "free_y")

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(geom = "text", aes(label = after_stat(eq.label)),
               label.x = c(100, 20), label.y = c(-0.1, 2.1), hjust = "inward",
               formula = formula)

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = y ~ x, color = "blue") +
  stat_poly_eq(mapping = use_label(c("R2", "eq")), color = "blue") +
  stat_poly_line(formula = y ~ x, color = "red", orientation = "y") +
  stat_poly_eq(mapping = use_label(c("R2", "eq")), color = "red", orientation = "y",
               label.y = 0.9)

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_ma_line() +
  stat_ma_eq(mapping = use_label("eq"))

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_ma_line(color = "blue") +
  stat_ma_eq(mapping = use_label(c("R2", "eq")), color = "blue") +
  stat_ma_line(color = "red", orientation = "y") +
  stat_ma_eq(mapping = use_label(c("R2", "eq")), color = "red", orientation = "y",
             label.y = 0.9)

## ----warning=FALSE------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_quant_band(formula = y ~ poly(x, 2))

## ----warning=FALSE------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_quant_line(formula = y ~ poly(x, 2), quantiles = c(0.05, 0.95))

## ----warning=FALSE------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_quant_line(formula = y ~ poly(x, 2), quantiles = 0.5)

## ----warning=FALSE------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_quant_band(formula = formula, color = "black", fill = "grey60") +
  stat_quant_eq(aes(label = paste(after_stat(grp.label), "*\": \"*",
                                  after_stat(eq.label), sep = "")),
                formula = formula) +
  theme_classic()

## ----warning=FALSE------------------------------------------------------------
ggplot(my.data, aes(x, y, color = group)) +
  geom_point() +
  stat_quant_line(formula = formula) +
  stat_quant_eq(aes(label = paste(after_stat(grp.label), "*\": \"*",
                                  after_stat(eq.label), sep = "")),
               formula = formula)

## ----warning=FALSE------------------------------------------------------------
ggplot(my.data, aes(x, y, group = group, linetype = group, 
                    shape = group, grp.label = group)) +
  geom_point() +
  stat_quant_line(formula = formula, quantiles = c(0.05, 0.95), color = "black") +
  stat_quant_eq(aes(label = paste(after_stat(grp.label), "*\": \"*",
                                  after_stat(eq.label), sep = "")),
                formula = formula, quantiles = c(0.05, 0.95)) +
  theme_classic()

## ----warning=FALSE------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_quant_line(formula = y ~ x, color = "blue", quantiles = 0.05) +
  stat_quant_eq(mapping = use_label("eq"), formula = y ~ x, color = "blue",
                quantiles = 0.05) +
  stat_quant_line(formula = x ~ y, color = "red", quantiles = 0.95) +
  stat_quant_eq(mapping = use_label("eq"), formula = x ~ y, color = "red", 
                quantiles = 0.95, label.y = 0.9)

## -----------------------------------------------------------------------------
# position of contrasts' bars (manual)
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33)  +
  stat_multcomp(label.y = c(7, 4, 1),
                contrast.type = "Dunnet",
                size = 2.75) +
  expand_limits(y = 0)

## -----------------------------------------------------------------------------
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33) +
   stat_multcomp(label.y = 
                   seq(from = 15, 
                       by = -3, 
                       length.out = 6),
                 size = 2.5) +
   expand_limits(y = 0)

## -----------------------------------------------------------------------------
means <-
  aggregate(mpg$hwy,
            by = list(mpg$cyl), 
            FUN = mean, 
            na.rm = TRUE)[["x"]]

ggplot(mpg, aes(factor(cyl), hwy)) +
  stat_summary(fun.data = mean_se) +
  stat_multcomp(label.type = "letters",
                label.y = means,
                position = position_nudge(x = 0.22))

## -----------------------------------------------------------------------------
# Using other geometries
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33) +
  stat_multcomp(label.type = "letters",
                geom = "label")

## -----------------------------------------------------------------------------
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33) +
  stat_multcomp(aes(x = stage(start = factor(cyl),
                              after_stat = x.right.tip)),
                geom = "text",
                label.y = "bottom",
                vstep = 0,
                contrast.type = "Dunnet")

## -----------------------------------------------------------------------------
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33) +
  stat_multcomp(aes(x = stage(start = factor(cyl),
                              after_stat = x.right.tip),
                    label = after_stat(stars.label)),
                geom = "text",
                label.y = "bottom",
                vstep = 0,
                contrast.type = "Dunnet")

## -----------------------------------------------------------------------------
# use colour to show significance
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33) +
  stat_multcomp(aes(colour = after_stat(p.value) < 0.01),
                size = 2.75) +
  scale_colour_manual(values = c("grey60", "black")) +
  theme_bw()

## -----------------------------------------------------------------------------
# add arrow heads to segments and use fill to show significance
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33) +
  stat_multcomp(aes(fill = after_stat(p.value) < 0.01),
                size = 2.5,
                arrow = grid::arrow(angle = 90,
                                    length = unit(1, "mm"),
                                    ends = "both")) +
  scale_fill_manual(values = c("white", "green"))

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y, colour = group)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_fit_residuals(formula = formula)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y, colour = group)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_fit_residuals(formula = formula,
                     method = "rlm",
                     mapping = aes(size = sqrt(after_stat(weights))),
                     alpha = 2/3)

## ----eval = FALSE-------------------------------------------------------------
#  formula <- y ~ poly(x, 3, raw = TRUE)
#  ggplot(my.data, aes(x, y, colour = group)) +
#    geom_hline(yintercept = 0, linetype = "dashed") +
#    stat_fit_residuals(formula = formula, weighted = TRUE)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_deviations(formula = formula, colour = "red") +
  geom_point()

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_smooth(method = "lm", formula = formula) +
  geom_point() +
  stat_fit_deviations(formula = formula, colour = "red",
                      arrow = arrow(length = unit(0.015, "npc"), 
                                   ends = "both"))

## -----------------------------------------------------------------------------
my.data.outlier <- my.data
my.data.outlier[6, "y"] <- my.data.outlier[6, "y"] * 5
ggplot(my.data.outlier, aes(x, y)) +
  stat_smooth(method = MASS::rlm, formula = formula) +
  stat_fit_deviations(formula = formula, method = "rlm",
                      mapping = aes(colour = after_stat(weights)),
                      show.legend = TRUE) +
  scale_color_gradient(low = "red", high = "blue", limits = c(0, 1)) +
  geom_point()

## ----eval=eval_broom----------------------------------------------------------
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
                  aes(label = sprintf("italic(P)*\"-value = \"*%.3g", 
                                      after_stat(p.value))),
                  parse = TRUE)

## ----eval=eval_broom----------------------------------------------------------
micmen.formula <- y ~ SSmicmen(x, Vm, K) 
ggplot(Puromycin, aes(conc, rate, colour = state)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = micmen.formula,
              se = FALSE) +
  stat_fit_glance(method = "nls", 
                  method.args = list(formula = micmen.formula),
                  aes(label = paste("AIC = ", signif(after_stat(AIC), digits = 3), 
                                    ", BIC = ", signif(after_stat(BIC), digits = 3),
                                    sep = "")),
                  label.x = "centre", label.y = "bottom")

## ----eval=eval_broom----------------------------------------------------------
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

## ----eval=eval_broom----------------------------------------------------------
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

## ----eval=eval_broom----------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_tb(method = "lm",
              method.args = list(formula = formula),
              tb.type = "fit.coefs", parse = TRUE,
              label.y = "center", label.x = "left")

## ----eval=eval_broom----------------------------------------------------------
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

## ----eval=eval_broom----------------------------------------------------------
ggplot(chickwts, aes(factor(feed), weight)) +
  stat_summary(fun.data = "mean_se") +
  stat_fit_tb(tb.type = "fit.anova",
              label.x = "center",
              label.y = "bottom") +
  expand_limits(y = 0)

## ----eval=eval_broom----------------------------------------------------------
ggplot(chickwts, aes(factor(feed), weight)) +
  stat_summary(fun.data = "mean_se") +
  stat_fit_tb(tb.type = "fit.anova", label.x = "left", size = 3) +
  scale_x_discrete(expand = expansion(mult = c(0.2, 0.5))) +
  coord_flip()

## ----eval=(eval_flag && eval_broom)-------------------------------------------
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

## ----eval=eval_broom----------------------------------------------------------
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
                aes(label = paste("V[m]~`=`~", signif(after_stat(Vm_estimate), digits = 3),
                                  "%+-%", signif(after_stat(Vm_se), digits = 2),
                                  "~~~~K~`=`~", signif(after_stat(K_estimate), digits = 3),
                                  "%+-%", signif(after_stat(K_se), digits = 2),
                                  sep = "")),
                parse = TRUE)

## ----eval=eval_broom----------------------------------------------------------
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
                vstep = 0.12,
                aes(label = paste("V~`=`~frac(", signif(after_stat(Vm_estimate), digits = 2), "~C,",
                                  signif(after_stat(K_estimate), digits = 2), "+C)",
                                  sep = "")),
                parse = TRUE) +
  labs(x = "C", y = "V")

## ----eval=eval_broom----------------------------------------------------------
stat_micmen_eq <- function(vstep = 0.12,
                           size = 3,
                           ...) {
  stat_fit_tidy(method = "nls", 
                method.args = list(formula = micmen.formula),
                aes(label = paste("V~`=`~frac(", signif(after_stat(Vm_estimate), digits = 2), "~C,",
                                  signif(after_stat(K_estimate), digits = 2), "+C)",
                                  sep = "")),
                parse = TRUE,
                vstep = vstep,
                size = size,
                ...)
}

## ----eval=(eval_flag && eval_broom)-------------------------------------------
micmen.formula <- y ~ SSmicmen(x, Vm, K) 
ggplot(Puromycin, aes(conc, rate, colour = state)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = micmen.formula,
              se = FALSE) +
  stat_micmen_eq(label.x = "center",
                label.y = "bottom") +
  labs(x = "C", y = "V")

## ----eval=eval_broom----------------------------------------------------------
my_formula <- y ~ x

ggplot(mpg, aes(displ, 1 / hwy)) +
  geom_point() +
  geom_quantile(quantiles = 0.5, formula = my_formula) +
  stat_fit_tidy(method = "rq",
                method.args = list(formula = y ~ x, tau = 0.5), 
                tidy.args = list(se.type = "nid"),
                mapping = aes(label = sprintf('y~"="~%.3g+%.3g~x*", with "*italic(P)~"="~%.3f',
                                              after_stat(Intercept_estimate), 
                                              after_stat(x_estimate),
                                              after_stat(x_p.value))),
                parse = TRUE)

## ----eval=eval_broom----------------------------------------------------------
stat_rq_eqn <- 
  function(formula = y ~ x, 
           tau = 0.5,
           method = "br",
           mapping = aes(label = sprintf('y~"="~%.3g+%.3g~x*", with "*italic(P)~"="~%.3f',
                                         after_stat(Intercept_estimate), 
                                         after_stat(x_estimate),
                                         after_stat(x_p.value))),
           parse = TRUE,
           ...) {
    method.args <- list(formula = formula, tau = tau, method = method)
    stat_fit_tidy(method = "rq",
                  method.args = method.args, 
                  tidy.args = list(se.type = "nid"),
                  mapping = mapping,
                  parse = parse,
                  ...)
  }

## ----eval=(eval_flag && eval_broom)-------------------------------------------
ggplot(mpg, aes(displ, 1 / hwy)) +
  geom_point() +
  geom_quantile(quantiles = 0.5, formula = my_formula) +
  stat_rq_eqn(tau = 0.5, formula = my_formula)

## ----eval=eval_broom----------------------------------------------------------
# formula <- y ~ poly(x, 3, raw = TRUE)
# broom::augment does not handle poly correctly!
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_fit_augment(method = "lm",
                   method.args = list(formula = formula))

## ----eval=eval_broom----------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y, colour = group)) +
  geom_point() +
  stat_fit_augment(method = "lm", 
                   method.args = list(formula = formula))

## ----eval=(eval_flag && eval_broom)-------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  stat_fit_augment(method = "lm",
                   method.args = list(formula = formula),
                   geom = "point",
                   y.out = ".resid")

## ----eval=eval_broom----------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y, colour = group)) +
  stat_fit_augment(method = "lm",
                   method.args = list(formula = formula),
                   geom = "point",
                   y.out = ".std.resid")

## ----eval=(eval_flag && eval_broom)-------------------------------------------
args <- list(formula = y ~ k * e ^ x,
             start = list(k = 1, e = 2))
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  stat_fit_augment(method = "nls",
                   method.args = args)

## ----eval=(eval_flag && eval_broom)-------------------------------------------
args <- list(formula = y ~ k * e ^ x,
             start = list(k = 1, e = 2))
ggplot(mtcars, aes(wt, mpg)) +
  stat_fit_augment(method = "nls",
                   method.args = args,
                   geom = "point",
                   y.out = ".resid")

## ----eval=(eval_nlme && eval_broom_mixed)-------------------------------------
args <- list(model = y ~ SSlogis(x, Asym, xmid, scal),
             fixed = Asym + xmid + scal ~1,
             random = Asym ~1 | group,
             start = c(Asym = 200, xmid = 725, scal = 350))
ggplot(Orange, aes(age, circumference, colour = Tree)) +
  geom_point() +
  stat_fit_augment(method = "nlme",
                   method.args = args,
                   augment.args = list(data = quote(data)))

## ----eval=(eval_broom && eval_gginnards)--------------------------------------
# formula <- y ~ poly(x, 3, raw = TRUE)
# broom::augment does not handle poly() correctly!
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y, colour = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_glance(geom = "debug",
                  method = "lm", 
                  method.args = list(formula = formula),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = sprintf("italic(P)*\"-value = \"*%.3g", 
                                      after_stat(p.value))),
                  parse = TRUE)

## ----eval=(eval_broom && eval_gginnards)--------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_tb(geom = "debug",
              summary.fun = str,
              method = "lm",
              method.args = list(formula = formula),
              tb.vars = c(Parameter = "term", 
                          Estimate = "estimate", 
                          "s.e." = "std.error", 
                          "italic(t)" = "statistic", 
                          "italic(P)" = "p.value"),
              label.y = "top", label.x = "left",
              parse = TRUE)

## -----------------------------------------------------------------------------
head(volcano_example.df) 

## -----------------------------------------------------------------------------
ggplot(volcano_example.df, 
       aes(logFC, PValue, colour = outcome2factor(outcome))) +
  geom_point() +
  scale_x_logFC(name = "Transcript abundance%unit") +
  scale_y_Pvalue() +
  scale_colour_outcome() +
  stat_quadrant_counts(data = function(x) {subset(x, outcome != 0)})

## -----------------------------------------------------------------------------
ggplot(volcano_example.df, 
       aes(logFC, PValue, colour = outcome2factor(outcome, n.levels = 2))) +
  geom_point() +
  scale_x_logFC(name = "Transcript abundance%unit") +
  scale_y_Pvalue() +
  scale_colour_outcome() +
  stat_quadrant_counts(data = function(x) {subset(x, outcome != 0)})

## -----------------------------------------------------------------------------
head(quadrant_example.df)

## -----------------------------------------------------------------------------
  ggplot(subset(quadrant_example.df, 
                xy_outcomes2factor(outcome.x, outcome.y) != "none"),
         aes(logFC.x, logFC.y, 
             colour = outcome2factor(outcome.x), 
             fill = outcome2factor(outcome.y))) +
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
               aes(yintercept = yintercept),
               na.rm = TRUE,
               ...),
    geom_vline(data =  data.frame(outcome.xy.fct = factor(c("xy", "x", "y", "none"),
                                                          levels = c("xy", "x", "y", "none")),
                                  xintercept = c(0, 0, NA, NA)),
               aes(xintercept = xintercept),
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

