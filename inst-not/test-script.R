library(ggplot2)
library(ggpmisc)

model <- y ~ x

ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  stat_smooth(method = "lm", formula = model)

ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm", formula = model) +
  stat_fit_tb(label.x = 10, label.y = 110) +
  theme_bw()

ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm", formula = model) +
  stat_fit_tb(label.x = 10, label.y = 110, digits = 4) +
  theme_bw()

ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm", formula = model) +
  stat_fit_tb(label.x = 10, label.y = 110, tb.type = "fit.summary") +
  theme_bw()

ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm", formula = model) +
  stat_fit_tb(label.x = 10, label.y = 110, tb.type = "fit.anova") +
  theme_bw()

ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm", formula = model) +
  stat_fit_tb(label.x = 10, label.y = 110, tb.type = "fit.coefs") +
  theme_bw()

