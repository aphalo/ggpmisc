library(ggplot2)
library(ggpmisc)

ggplot(mtcars, aes(mpg, hp)) +
  geom_line() +
  geom_point() +
  stat_peaks(span = NULL,
             geom = "text_s",
             mapping = aes(label = paste(after_stat(y.label), after_stat(x.label))),
             x.label.fmt = "at %.0f mpg",
             y.label.fmt = " Max hp = %.0f",
             segment.colour = "red",
             arrow = grid::arrow(length = unit(0.1, "inches")),
             position = position_nudge_keep(x = 1, y = 0),
             hjust = 0)

ggplot(mtcars, aes(mpg, hp, color = factor(vs))) +
  geom_line() +
  geom_point() +
  stat_peaks(span = NULL,
             geom = "text_s",
             mapping = aes(label = paste(after_stat(y.label), after_stat(x.label))),
             x.label.fmt = "at %.0f mpg ",
             y.label.fmt = " Max hp = %.0f",
             arrow = grid::arrow(length = unit(0.1, "inches")),
             position = position_nudge_keep(x = c(1, -1),  y = 10),
             hjust = c(0, 1))

ggplot(mtcars, aes(mpg, hp)) +
  geom_line() +
  geom_point() +
  stat_peaks(span = NULL,
             geom = "text_s",
             mapping = aes(label = paste(after_stat(y.label), after_stat(x.label))),
             x.label.fmt = "at %.0f mpg",
             y.label.fmt = " Max hp = %.0f",
             segment.colour = "blue",
             arrow = grid::arrow(length = unit(0.1, "inches")),
             position = position_nudge_keep(x = 2, y = 10),
             hjust = 0) +
  facet_wrap(~factor(vs))

ggplot(mtcars, aes(hp, mpg)) +
  geom_line(orientation = "y") +
  geom_point() +
  stat_peaks(span = NULL,
             strict = TRUE,
             geom = "text_s",
             mapping = aes(label = paste(after_stat(x.label), after_stat(y.label))),
             x.label.fmt = "Max hp = %.0f",
             y.label.fmt = "at %.0f mpg ",
             segment.colour = "red",
             arrow = grid::arrow(length = unit(0.1, "inches")),
             position = position_nudge_keep(x = 0, y = 1),
             hjust = 1,
             angle = -90,
             orientation = "y")

ggplot(mtcars, aes(mpg, hp)) +
  geom_line() +
  geom_point() +
  stat_valleys(span = 5,
               strict = TRUE,
               geom = "text_s",
               mapping = aes(label = paste(after_stat(y.label), after_stat(x.label))),
               x.label.fmt = "at %.1f mpg ",
               y.label.fmt = "hp = %.0f",
               segment.colour = "blue",
               arrow = grid::arrow(length = unit(0.1, "inches")),
               position = position_nudge_keep(x = -1, y = -20),
               hjust = 1) +
  stat_peaks(span = 5,
             strict = TRUE,
             geom = "text_s",
             mapping = aes(label = paste(after_stat(y.label), after_stat(x.label))),
             x.label.fmt = "at %.1f mpg",
             y.label.fmt = "hp = %.0f\n",
             segment.colour = "red",
             arrow = grid::arrow(length = unit(0.1, "inches")),
             position = position_nudge_keep(x = 1, y = 20),
             hjust = 0) +
 expand_limits(y = 0)




