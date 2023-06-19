library(gganimate)
library(gginnards)
library(ggpmisc)
library(tibble)

diamonds <- diamonds[sample.int(nrow(diamonds), nrow(diamonds) %/% 25), ]

# 'gganimate' converts group from integer into character
ggplot(diamonds, aes(x = carat, y = price)) +
  stat_debug_group(summary.fun = as_tibble) +
  transition_states(cut)

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq() +
  transition_states(cut, transition_length = 1, state_length = 1) +
  enter_fade() + exit_shrink() +
  labs(title = "Cut = {closest_state}")

# 'ggpmisc' future Version 0.5.3

library(gganimate)
library(ggpmisc)

# Animation with stat_poly_eq()
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  stat_poly_line() +
  stat_poly_eq() +
  transition_states(cut, transition_length = 1, state_length = 1) +
  enter_fade() + exit_shrink() +
  labs(title = "Cut = {closest_state}")

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  stat_poly_line() +
  stat_poly_eq(mapping = use_label(c("eq", "R2", "F"))) +
  transition_states(cut, transition_length = 1, state_length = 1) +
  enter_fade() + exit_shrink() +
  labs(title = "Cut = {closest_state}")

# Animation with stat_poly_eq()
ggplot(diamonds, aes(x = carat, y = price, color = color)) +
  geom_point() +
  stat_poly_line() +
  stat_poly_eq() +
  theme_bw() +
  transition_states(cut, transition_length = 1, state_length = 2) +
  enter_fade() + exit_shrink() +
  labs(title = "Cut = {closest_state}")

