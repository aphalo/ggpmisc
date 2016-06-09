## Code included in P. J. Aphalo's answers in Stackoverflow
## If any of these stop working, the answers will need editing

library(ggplot2)
library(ggpmisc)
df <- data.frame(x = c(1:100))
df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)
my.formula <- y ~ x
p <- ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", formula = my.formula) +
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  geom_point()
p

library(ggplot2)
library(ggpmisc)
df <- data.frame(x = c(1:100))
df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)
my.formula <- y ~ x
p <- ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", formula = my.formula) +
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  geom_point()
p

p <- ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", formula = my.formula) +
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(h)~`=`~",
               eq.x.rhs = "~italic(z)",
               aes(label = ..eq.label..),
               parse = TRUE) +
  labs(x = expression(italic(z)), y = expression(italic(h))) +
  geom_point()
p

