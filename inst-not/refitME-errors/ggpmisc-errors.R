library(ggplot2)
library(ggpmisc)
library(refitME)

# fails
rm(list = ls(pattern = "*"))

lm_EIV <- function(formula, data, ..., sigma.sq.u) {
  fm <- lm(formula = formula, data = data, ...)
  MCEMfit_glm(mod = fm, family = "gaussian", sigma.sq.u = sigma.sq.u)
}

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  stat_poly_line(method = "lm_EIV", method.args = c(sigma.sq.u = 0.1)) +
  stat_poly_eq(method = "lm_EIV", method.args = c(sigma.sq.u = 0.1),
               mapping = use_label("eq"),
               label.x = "right") +
  theme(legend.position = "bottom")
