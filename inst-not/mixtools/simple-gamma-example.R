# Gamma

Gmix.ls <-gammamixEM(faithful$waiting)
class(Gmix.ls)
# summary(Gmix.ls) # there is no method!
plot(Gmix.ls) # Log-Likelihood vs iteration

data.tb <- with(Gmix.ls, cbind(x, posterior)) |> as.data.frame()
class(data.tb)

Gparams <- Gmix.ls[setdiff(names(Gmix.ls), c("x", "posterior", "all.loglik"))]

# Gmix.param.se <- boot.se(Gmix.ls, B=100, arbvar = FALSE)
# Gmix.param.se[grepv(".se$", names(Gmix.param.se))]

data.tb$gamma.1 <- dgamma(seq(from = min(data.tb$x), to = max(data.tb$x), length.out = nrow(data.tb)),
                          shape = Gparams$gamma.pars[1, 1], scale = Gparams$gamma.pars[2, 1])
data.tb$gamma.2 <- dgamma(seq(from = min(data.tb$x), to = max(data.tb$x), length.out = nrow(data.tb)),
                          shape = Gparams$gamma.pars[1, 2], scale = Gparams$gamma.pars[2, 2])
head(data.tb)
