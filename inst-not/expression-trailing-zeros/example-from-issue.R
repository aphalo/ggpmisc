library(tidyverse)
library(ggpmisc)

set.seed(2017)

tdf <- data.frame(
  x = 1:100,
  y = 1.04 * 1:100 + rnorm(100)
)

ggplot(aes(x = x, y = y), data = tdf) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(
    aes(label = paste(stat(eq.label), stat(rr.label), sep = "*plain(\",\")~")),
    formula = y ~ x,
    coef.digits = 3,
    rr.digits = 3,
    parse = TRUE
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = 2)


library(tidyverse)
library(ggpmisc)
library(ggtext)

diamonds %>%
  filter(color %in% c("E", "H", "I")) %>%
  ggplot(aes(x=carat, y=table)) +
  geom_point() +
  stat_poly_line() +
  stat_poly_eq(aes(label = after_stat(eq.label)),
               geom = "rich_text", output.type = "markdown",
               label.y = 72, label.x = 0.5, fill = NA, label.size = NA,
               hjust = 0) +
  facet_wrap(~color) +
  theme_bw()

diamonds %>%
  filter(color %in% c("E", "H", "I")) %>%
  ggplot(aes(x=carat, y=table)) +
  geom_point() +
  stat_poly_line() +
  stat_poly_eq(aes(label = after_stat(eq.label)),
               geom = "text", output.type = "expression",
               label.y = 72, label.x = 0.5, #fill = NA, label.size = NA,
               hjust = 0) +
  facet_wrap(~color) +
  theme_bw()
