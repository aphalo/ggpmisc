## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      fig.align = 'center', 
                      fig.show = 'hold', fig.width = 7, fig.height = 4)

## -----------------------------------------------------------------------------
library(ggpmisc)
theme_set(theme_bw() + theme(panel.grid = element_blank()))

## -----------------------------------------------------------------------------
p1 <- ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_point() +
  geom_text_npc(data = data.frame(x = c("left", "left"),
                                  y = c("top", "bottom"),
                                  label = c("Most\nefficient",
                                            "Least\nefficient")),
                mapping = aes(npcx = x, npcy = y, label = label),
                size = 3)
p1


## -----------------------------------------------------------------------------
p1 + expand_limits(y = 0)

## -----------------------------------------------------------------------------
ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_point() +
  annotate(geom = "text_npc",
           npcx = c("left", "left"),
           npcy = c("top", "bottom"),
           label = c("Most\nefficient",
                     "Least\nefficient"),
           size = 3)

## -----------------------------------------------------------------------------
p2 <- ggplot(mtcars, aes(factor(cyl), mpg, colour = factor(cyl))) +
  stat_boxplot() +
  labs(y = NULL) +
  theme_bw(9) + 
  theme(legend.position = "none",
        panel.grid = element_blank())

ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
  geom_point() +
  annotate("plot_npc", npcx = "left", npcy = "bottom", label = p2) +
  expand_limits(y = 0, x = 0)


## -----------------------------------------------------------------------------
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_text_npc(data = data.frame(cyl = levels(factor(mtcars$cyl)),
                                  label = LETTERS[seq_along(levels(factor(mtcars$cyl)))],
                                  x = 0.90,
                                  y = 0.95),
                 mapping = aes(npcx = x, npcy = y, label = label),
                size = 4) +
  facet_wrap(~factor(cyl), scales = "free") +
  theme(strip.background = element_blank(),
        strip.text = element_blank())

