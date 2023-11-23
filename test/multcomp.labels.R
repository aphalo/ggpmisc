library(ggpmisc)

p1 <- ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33)

p1 +
  stat_multcomp(adjusted.type = "bonferroni", adj.method.tag = -3)

p1 +
  stat_multcomp(adjusted.type = "bonferroni", adj.method.tag = 0)

p1 +
  stat_multcomp(label.type = "LETTERS", adjusted.type = "holm")

p1 +
  stat_multcomp(label.type = "LETTERS", adjusted.type = "holm", adj.method.tag = -3)

p1 +
  stat_multcomp(label.type = "LETTERS", adjusted.type = "holm", adj.method.tag = 0)

p1 +
  stat_multcomp(label.type = "LETTERS", adjusted.type = "holm", adj.method.tag = FALSE)
