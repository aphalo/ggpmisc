library(ggplot2)

m <-
  ggplot(mpg, aes(displ, 1 / hwy)) +
  geom_point()
m + stat_quantile()

m + stat_quantile(method = "rqss")

m + stat_quantile(method = "rqss", lambda = 0.1)

sessionInfo()

