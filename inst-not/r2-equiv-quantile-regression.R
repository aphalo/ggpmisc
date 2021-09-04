# https://stats.stackexchange.com/questions/129200/r-squared-in-quantile-regression

# answer by Dimitriy V. Masterov

library(quantreg)
data(engel)

fit0 <- rq(foodexp~1,tau=0.9,data=engel)
fit1 <- rq(foodexp~income,tau=0.9,data=engel)

# rho <- function(u,tau=.5)u*(tau - (u < 0))
R1 <- 1 - fit1$rho/fit0$rho

# R1 is a local equivalent to R2 as it depends on Tau. R2 in OLS is global, instead.

# Koenker and Machado (1999) JASA 94(448)1296-1310. https://doi.org/10.1080/01621459.1999.10473882

