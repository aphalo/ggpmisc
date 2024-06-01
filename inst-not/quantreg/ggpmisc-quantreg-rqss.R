library(ggplot2)
library(ggpmisc)

p <-
  ggplot(mpg, aes(displ, 1 / hwy)) +
  geom_point()
p + stat_quant_line()

p + stat_quant_line(method = "rqss")

p + stat_quant_line(method = "rqss", lambda = 0.1)

p + stat_quant_band()

p + stat_quant_band(method = "rqss")

p + stat_quant_band(method = "rqss", lambda = 0.1)

p + stat_quant_eq()

p + stat_quant_eq(method = "rqss")

p + stat_quant_eq(method = "rqss", lambda = 0.1)

sessionInfo()
