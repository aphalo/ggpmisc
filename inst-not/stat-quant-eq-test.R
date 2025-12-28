library(ggpmisc)
library(gginnards)

set.seed(4321)
x <- 1:100
y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
my.data <- data.frame(x = x, y = y,
                      group = c("A", "B"),
                      y2 = y * c(0.5,2),
                      w = sqrt(x))


formula <- y ~ x + I(x^2) + I(x^3)
my.format <-
  "b[0]~`=`~%.3g*\", \"*b[1]~`=`~%.3g*\", \"*b[2]~`=`~%.3g*\", \"*b[3]~`=`~%.3g"

ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_quant_line(formula = formula, quantiles = 0.5) +
  stat_quant_eq(formula = formula,
                quantiles = 0.5,
                output.type = "numeric",
                parse = TRUE,
                mapping =
                  aes(label = sprintf(my.format,
                                      after_stat(b_0), after_stat(b_1),
                                      after_stat(b_2), after_stat(b_3)))) +
  facet_wrap(~group)



ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_eq(formula = y ~ poly(x, 3, raw = TRUE),
                geom = "debug_group",
                output.type = "markdown",
                mapping =
                  aes(label = paste(after_stat(eq.label),
                                    after_stat(rr.label),
                                    after_stat(AIC.label),
                                    sep = " ")))

