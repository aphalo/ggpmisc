library(mixtools)
library(ggplot2)
library(ggpp)

data(faithful) # geisser in Yellowstone National Park

# A histogram

ggplot(faithful, aes(waiting)) +
  geom_histogram(bins = 20)

# Normal
# Fitting a mixture of two Normal distributions (k is the number of distributions)
Nmix.ls <- normalmixEM(faithful$waiting, k = 2)
class(Nmix.ls)
summary(Nmix.ls) # it is a print function!
plot(Nmix.ls) # Log-Likelihood vs. iteration. Diagnose convergence.

# posterior is the probability of each observation belonging to a component of the mixture
# column bind the input values in x and the posterior probabilities
data.tb <- with(Nmix.ls, cbind(x, posterior)) |> as.data.frame()
class(data.tb)

# for clarity I extract the parameter estimates mean (mu) and sd (sigma) and lambda
# lambda gives the fraction of the total probability density (or estimated
#  frequency of observations) in each component
Nparams <- Nmix.ls[setdiff(names(Nmix.ls), c("x", "posterior", "all.loglik"))]
Nparams

# using bootstrap, standard errors are computed for the parameter estimates
# B is the number of "trials" used to estimate the se
Nmix.param.se <- boot.se(Nmix.ls, B=100)
Nmix.param.se[grepv(".se$", names(Nmix.param.se))]

params.tb <- c(Nparams[c("lambda", "mu", "sigma")],
               list(mu.se = as.vector(Nmix.param.se[["mu.se"]])),
               Nmix.param.se[c("lambda.se", "sigma.se")]) |>
  as.data.frame()

# we use the parameter estimates to generate predictions for the distributions
# 1) we find frpm the paremeter estimate the range of x covered by the Normal distributions
x.range <- range(qnorm(p = 0.0005, mean = params.tb$mu, sd = params.tb$sigma),
                 qnorm(p = 0.0005, mean = params.tb$mu, sd = params.tb$sigma, lower.tail = FALSE))
# 2) we generate predictions for this range using the fitted parameter estimates
fm.tb <- list()
fm.tb$norm.x <- seq(from = x.range[1], to = x.range[2], length.out = 250)
fm.tb$norm.1 <- dnorm(fm.tb$norm.x, mean = Nparams$mu[1], sd = Nparams$sigma[1]) * Nparams$lambda[1]
fm.tb$norm.2 <- dnorm(fm.tb$norm.x, mean = Nparams$mu[2], sd = Nparams$sigma[2]) * Nparams$lambda[2]
fm.tb <- as.data.frame(fm.tb)
head(fm.tb, n = 10) # top ten rows

# we plot the predictions for both Normal distributions and their sum
# the area under the curves is 1, the total of the probability
ggplot(fm.tb, aes(x = norm.x)) +
  geom_line(aes(y = norm.1 + norm.2)) +
  geom_line(aes(y = norm.1), linetype = "dashed") +
  geom_line(aes(y = norm.2), linetype = "dotted")

# we plot a histogram behind the predicted Normal distributions
ggplot() +
  geom_histogram(aes(waiting, after_stat(density)), alpha = 0.3,
                 bins = 20, data = faithful) +
  geom_line(aes(x = norm.x, y = norm.1 + norm.2), data = fm.tb) +
  geom_line(aes(x = norm.x, y = norm.1), linetype = "dashed", data = fm.tb) +
  geom_line(aes(x = norm.x, y = norm.2), linetype = "dotted", data = fm.tb) +
  geom_pointrange(aes(x = mu, xmin = mu - mu.se, xmax = mu + mu.se, y = 0),
                  orientation = "y", data = params.tb, colour = "blue", size = 0.3) +
  expand_limits(x = c(38, 101)) +
  annotate(geom = "table", label = signif(params.tb, 3), x = 35, y = 0.05) +
            theme_classic()

