library(ggpmisc)
library(ggrepel)
library(broom)

# plotmath

ggplot(cars, aes(speed, dist)) +
  geom_point() +
  stat_poly_line(formula = y ~ x) +
  stat_poly_eq(use_label("eq", "adj.R2"))

ggplot(cars, aes(speed, dist)) +
  geom_point() +
  stat_poly_line(formula = y ~ x) +
  stat_poly_eq(use_label("eq", "adj.R2"), geom = "label_npc")

ggplot(cars, aes(speed, dist)) +
  geom_point() +
  stat_poly_line(formula = y ~ x) +
  stat_poly_eq(use_label("eq", "adj.R2"), geom = "text",
               hjust = "inward", vjust = "inward")

ggplot(cars, aes(speed, dist)) +
  geom_point() +
  stat_poly_line(formula = y ~ x) +
  stat_poly_eq(use_label("eq", "adj.R2"), geom = "label",
               hjust = "inward", vjust = "inward")

# ASCII text

ggplot(cars, aes(speed, dist)) +
  geom_point() +
  stat_poly_line(formula = y ~ x) +
  stat_poly_eq(use_label("eq", "adj.R2", sep = ", "), geom = "text", output.type = "text",
               hjust = "inward", vjust = "inward")

# LaTeX

library(xdvir)
ggplot(ddd, aes(speed, dist)) +
  geom_point() +
  stat_poly_line(formula = y ~ x) +
  stat_poly_eq(use_label("eq", "adj.R2", sep = ", "), geom = "latex",
               hjust = "inward", vjust = 1) # bug? "inward" not working

# Markdown

library(marquee)
ggplot(cars, aes(speed, dist)) +
  geom_point() +
  stat_poly_line(formula = y ~ x) +
  stat_poly_eq(use_label("eq", "adj.R2", sep = ", "), geom = "marquee",
               hjust = "inward", vjust = 1) # bug? "inward" not working

library(ggtext)
ggplot(cars, aes(speed, dist)) +
  geom_point() +
  stat_poly_line(formula = y ~ x) +
  stat_poly_eq(use_label("eq", "adj.R2", sep = ", "), geom = "richtext",
               hjust = "inward", vjust = "inward")

# plotmath with repulsion (for the sake of it)

library(ggrepel)
ggplot(cars, aes(speed, dist)) +
  geom_point() +
  stat_poly_line(formula = y ~ x) +
  stat_poly_eq(use_label("eq", "adj.R2"), geom = "text_repel",
               hjust = "inward", vjust = "inward")

ggplot(cars, aes(speed, dist)) +
  geom_point() +
  stat_poly_line(formula = y ~ x) +
  stat_poly_eq(use_label("eq", "adj.R2"), geom = "label_repel",
               hjust = "inward", vjust = "inward")

