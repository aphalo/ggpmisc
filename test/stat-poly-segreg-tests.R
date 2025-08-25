# tests of stat_poly_eq()

library(ggpmisc)
library(segmented)

# generate artificial data
set.seed(4321)
x <- 1:100
y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
y <- y / max(y)
my.data <- data.frame(x = x, y = y,
                      group = c("A", "B"),
                      y2 = y * c(1, 2) + c(0, 0.1),
                      w = sqrt(x))

ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line() +
  stat_poly_eq()

ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_poly_line(method = segreg, formula = y ~ seg(x, npsi = 2)) +
  stat_poly_eq(method = segreg, formula = y ~ seg(x, npsi = 2))

