library(refitME)

# works
rm(list = ls(pattern = "*"))

fm <- lm(formula = hwy ~ displ, data = ggplot2::mpg)
MCEMfit_glm(mod = fm, family = "gaussian", sigma.sq.u = 0.01)

# There is no 'data' field!!

fm$data

# works
rm(list = ls(pattern = "*"))

fm <- lm(formula = hwy ~ displ, data = ggplot2::mpg)
fm$data <- ggplot2::mpg
MCEMfit_glm(mod = fm, family = "gaussian", sigma.sq.u = 0.01)

# There is data

fm$data

# fails
rm(list = ls(pattern = "*"))

lm_EIV <- function(formula, data, ..., sigma.sq.u) {
  fm <- lm(formula = formula, data = data, ...)
  MCEMfit_glm(mod = fm, family = "gaussian", sigma.sq.u = sigma.sq.u)
}

lm_EIV(formula = hwy ~ displ, data = ggplot2::mpg, sigma.sq.u = 0.01)

# fails
rm(list = ls(pattern = "*"))

lm_EIV <- function(formula, data, ..., sigma.sq.u) {
  my.data <- data
  fm <- lm(formula = formula, data = my.data, ...)
  MCEMfit_glm(mod = fm, family = "gaussian", sigma.sq.u = sigma.sq.u)
}

lm_EIV(formula = hwy ~ displ, data = ggplot2::mpg, sigma.sq.u = 0.01)

# fails
rm(list = ls(pattern = "*"))

lm_EIV <- function(formula, data, ..., sigma.sq.u) {
  fm <- lm(formula = formula, data = data, ...)
  fm$data <- data
  MCEMfit_glm(mod = fm, family = "gaussian", sigma.sq.u = sigma.sq.u)
}

lm_EIV(formula = hwy ~ displ, data = ggplot2::mpg, sigma.sq.u = 0.01)

# works at the command prompt
# but fails when function is called within a package
rm(list = ls(pattern = "*"))

lm_EIV <- function(formula, data, ..., sigma.sq.u) {
  assign("data", value = data, envir = globalenv())
  fm <- lm(formula = formula, data = data, ...)
  MCEMfit_glm(mod = fm, family = "gaussian", sigma.sq.u = sigma.sq.u)
}

lm_EIV(formula = hwy ~ displ, data = ggplot2::mpg, sigma.sq.u = 0.01)

# works at the command prompt
# but fails when function is called within a package
rm(list = ls(pattern = "*"))

lm_EIV <- function(formula, data, ..., sigma.sq.u) {
  assign("my.data", value = data, envir = globalenv())
  fm <- lm(formula = formula, data = my.data, ...)
  MCEMfit_glm(mod = fm, family = "gaussian", sigma.sq.u = sigma.sq.u)
}

lm_EIV(formula = hwy ~ displ, data = ggplot2::mpg, sigma.sq.u = 0.01)



