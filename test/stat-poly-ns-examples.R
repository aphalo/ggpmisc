library(ggpmisc)

# First approach: faint lines for non-significant fits, with bands
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  stat_poly_eq(aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label),
                                 sep = "*\", \"*")),
               label.x = "right") +
  stat_poly_line(aes(colour = stage(after_scale = ifelse(p.value < 0.05,
                                                         alpha(colour, 1),
                                                         alpha(colour, 0.25)))),
                 se = TRUE,
                 fm.values = T) +
  facet_wrap(~class, ncol = 2) +
  theme_bw()

# Second approach: faint lines for non-significant fits, no-bands
# colour mapped to class
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  stat_poly_eq(aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label),
                                 sep = "*\", \"*")),
               label.x = "right") +
  stat_poly_line(aes(colour = stage(start = class,
                                    after_scale = ifelse(p.value < 0.05,
                                                         alpha(colour, 1),
                                                         alpha(colour, 0.25)))),
                 se = FALSE,
                 fm.values = T) +
  facet_wrap(~class, ncol = 2) +
  theme_bw()

# Third approach: no bands or lines for non-significant fits
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  stat_poly_eq(aes(label = stage(after_stat = ifelse(p.value < 0.05,
                                                     paste(eq.label,
                                                           p.value.label,
                                                           sep = "*\", \"*"),
                                                     paste(p.value.label,
                                                           sep = "*\", \"*")))),
                   label.x = "right") +
  stat_poly_line(aes(colour = stage(after_scale = ifelse(p.value < 0.05,
                                                        colour,
                                                        NA)),
                     fill = stage(after_scale = ifelse(p.value < 0.05,
                                                       fill,
                                                       NA))),
                 se = TRUE,
                 fm.values = T) +
  facet_wrap(~class, ncol = 2) +
  theme_bw()

ggplot(mpg, aes(displ, hwy, colour = class, grp.label = class)) +
  geom_point() +
  stat_poly_eq(aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label),
                                 sep = "*italic(\" with \")*")),
               label.x = "right") +
  stat_poly_line(aes(colour = stage(start  = class,
                                    after_scale = ifelse(p.value < 0.05,
                                                         alpha(colour, 1),
                                                         alpha(colour, 0.25)))),
                 se = FALSE,
                 fm.values = T) +
  theme_bw()

ggplot(mpg, aes(displ, hwy, colour = class, grp.label = class)) +
  geom_point() +
  stat_poly_eq(aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label),
                                 sep = "*italic(\" with \")*")),
               label.x = "right") +
  stat_poly_line(aes(colour = stage(start  = class,
                                    after_scale = ifelse(p.value < 0.05,
                                                         alpha(colour, 1),
                                                         alpha(colour, 0.25)))),
                 se = FALSE,
                 fm.values = T) +
  theme_bw()

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  stat_poly_line(formula = y ~ x, fullrange = TRUE) +
  stat_poly_line(formula = x ~ y, fullrange = TRUE, colour = "red") +
  expand_limits(x = c(0, 8), y = 0)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  stat_poly_line(formula = y ~ x, fullrange = TRUE) +
  stat_poly_line(formula = x ~ y, fullrange = TRUE, colour = "red") +
  stat_ma_line(fullrange = TRUE, method = "RMA",
               range.y = "relative", range.x = "relative", colour = "black") +
  expand_limits(x = c(0, 8), y = 0)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) +
  geom_point() +
  stat_ma_line() +
  stat_ma_eq()

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) +
  geom_point() +
  stat_ma_line(method = "MA") +
  stat_ma_eq(aes(label = after_stat(p.value.label)),
                 method = "MA", nperm = 999)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) +
  geom_point() +
  stat_ma_line(method = "RMA",
               range.y = "interval", range.x = "interval") +
  stat_ma_eq(method = "RMA",
             range.y = "interval", range.x = "interval", p.digits = 2)
