library(ggpmisc)


# Normal -> gaussian ------------------------------------------------------

set.seed(1)

df <- data.frame(x = 1:100,
                 y = rpois(100, seq(1, 5, length.out = 100)))

model <- glm(y ~ x, data = df, family = gaussian)

ggplot(df, aes(x, y)) +
  geom_point() +
  stat_poly_line(method = "lm", se = TRUE) +
  stat_poly_eq(method = "lm")

ggplot(df, aes(x, y)) +
  geom_point() +
  stat_poly_line(method = "glm", method.args = list(family = gaussian), se = FALSE) +
  stat_poly_eq(use_label("eq"), method = "glm", method.args = list(family = gaussian))

ggplot(df, aes(x, y)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = gaussian), se = TRUE) +
  stat_poly_eq(method = "glm", method.args = list(family = gaussian))

ggplot(df, aes(x, y)) +
  geom_point() +
  stat_poly_line(method = "glm", method.args = list(family = gaussian), se = TRUE) +
  stat_poly_eq(use_label("eq", "p"), method = "glm", method.args = list(family = gaussian))

# Poisson -> poisson ------------------------------------------------------
# example from https://stackoverflow.com/questions/69808033/how-can-i-create-a-ggplot-with-a-regression-line-based-on-the-predicted-values-o

set.seed(1)

df <- data.frame(x = 1:100,
                 y = rpois(100, seq(1, 5, length.out = 100)))

model <- glm(y ~ x, data = df, family = poisson)

ggplot(df, aes(x, y)) +
  geom_point() +
  stat_poly_line(method = "glm", method.args = list(family = poisson), se = FALSE) +
  stat_poly_eq(method = "glm", method.args = list(family = poisson))

ggplot(df, aes(x, y)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = poisson), se = TRUE) +
  stat_poly_eq(method = "glm", method.args = list(family = poisson))
