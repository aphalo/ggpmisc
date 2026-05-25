library(ggpmisc)

my.data <- data.frame(x = 1:20, y = 1:20 + rnorm(20))

ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line() +
  stat_poly_eq(use_label("eq"))

ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = y ~ x + 0) +
  stat_poly_eq(use_label("eq"), formula = y ~ x + 0)

ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = y ~ x + 0, method = "rlm") +
  stat_poly_eq(use_label("eq"), formula = y ~ x + 0, method = "rlm")

ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = y ~ x + 0, method = "gls") +
  stat_poly_eq(use_label("eq"), formula = y ~ x + 0, method = "gls")

ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = y ~ x , method = "ma") +
  stat_poly_eq(use_label("eq"), formula = y ~ x, method = "ma")

# fails (fixed > 0.7.0)

ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = y ~ x, method = "sma") +
  stat_poly_eq(use_label("eq"), formula = y ~ x, method = "sma")

ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = y ~ x + 0, method = "ma") +
  stat_poly_eq(use_label("eq"), formula = y ~ x + 0, method = "ma")

ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = y ~ x + 0, method = "sma") +
  stat_poly_eq(use_label("eq"), formula = y ~ x + 0, method = "sma")

ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = y ~ x - 1, method = "ma") +
  stat_poly_eq(use_label("eq"), formula = y ~ x - 1, method = "ma")

ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(formula = y ~ x - 1, method = "sma") +
  stat_poly_eq(use_label("eq"), formula = y ~ x - 1, method = "sma")

