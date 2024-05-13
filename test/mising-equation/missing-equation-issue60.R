library(tidyverse)
library(ggpmisc)

test_df <- readRDS("test/mising-equation/ggpmisc_stat_poly_eq_df.rds")

test_df %>%
  subset(grp == "E") %>%
  ggplot(aes(x = ref1, y = ref2, colour = grp)) +
  geom_point() +
  geom_smooth(method="lm" , color="black", fill='grey', se=TRUE, alpha = 0.5, linewidth =0.5) +
  stat_poly_eq(use_label("eq"), label.x.npc = 0.055, label.y.npc = 0.88, color = "black",
               coef.keep.zeros = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue", alpha =0.5) +
  theme_bw() +
  facet_wrap(~grp)
