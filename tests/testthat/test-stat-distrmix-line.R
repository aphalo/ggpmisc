context("stat_distrmix_line")

set.seed(4321)
# generate artificial data
my.data <- data.frame(
  x = rnorm(n = 100, mean = 1, sd = 1),
  x.2means = c(rnorm(n = 50, mean = 1, sd = 1),
               rnorm(n = 50, mean = 4, sd = 1)),
  x.4means = c(rnorm(n = 25, mean = 2, sd = 1/2),
               rnorm(n = 25, mean = 4, sd = 1/2),
               rnorm(n = 25, mean = -2, sd = 1/2),
               rnorm(n = 25, mean = -4, sd = 1/2)),
  x.2SDs  = c(rnorm(n = 50, mean = 1, sd = 1),
              rnorm(n = 50, mean = 1, sd = 3)),
  x.2each  = c(rnorm(n = 50, mean = 1, sd = 1),
               rnorm(n = 50, mean = 4, sd = 3)))

if (isNamespaceLoaded(name = "package:ggpmisc")) detach(package:ggpmisc, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggpp")) detach(package:ggpp, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggplot2")) detach(package:ggplot2, unload = TRUE)

test_that("stat_distrmix_line works not attached", {
  vdiffr::expect_doppelganger("stat_distrmix_line_2means",
                              ggplot2::ggplot(my.data, ggplot2::aes(x.2means)) +
                                ggpmisc::stat_distrmix_line(fit.seed = 123)
  )

  vdiffr::expect_doppelganger("stat_distrmix_line_2SDs",
                              ggplot2::ggplot(my.data, ggplot2::aes(x.2SDs)) +
                                ggpmisc::stat_distrmix_line(fit.seed = 123)
  )

  vdiffr::expect_doppelganger("stat_distrmix_line_2means2SDs",
                              ggplot2::ggplot(my.data, ggplot2::aes(x.2each)) +
                                ggpmisc::stat_distrmix_line(fit.seed = 123)
  )

})


library(ggpmisc)

test_that("normalmix_line works attached", {
  vdiffr::expect_doppelganger("stat_distrmix_line_geom_point",
                              ggplot(my.data, aes(x.2means)) +
                                stat_distrmix_line(geom = "point", fit.seed = 123)
  )

  vdiffr::expect_doppelganger("stat_distrmix_line_n",
                              ggplot(my.data, aes(x.2means)) +
                                stat_distrmix_line(geom = "point", n = 30, fit.seed = 123)
  )

  vdiffr::expect_doppelganger("stat_distrmix_line_geom",
                              ggplot(my.data, aes(x.2means)) +
                                stat_distrmix_line(geom = "point", n = 30, fit.seed = 123)
  )

  vdiffr::expect_doppelganger("stat_distrmix_line_fullrange",
                              ggplot(my.data, aes(x.2means)) +
                                stat_distrmix_line(fullrange = FALSE, fit.seed = 123)
  )

  vdiffr::expect_doppelganger("stat_distrmix_line_k4",
                              ggplot(my.data, aes(x.4means)) +
                                stat_distrmix_line(k = 4, fit.seed = 123)
  )

  vdiffr::expect_doppelganger("stat_distrmix_line_k1",
                              ggplot(my.data, aes(x)) +
                                stat_distrmix_line(k = 1, fit.seed = 123)
  )

  vdiffr::expect_doppelganger("stat_distrmix_line_free_sd",
                              ggplot(my.data, aes(x.2means)) +
                                stat_distrmix_line(free.sd = FALSE, fit.seed = 123)
  )

  vdiffr::expect_doppelganger("stat_distrmix_line_free_mean",
                              ggplot(my.data, aes(x.2SDs)) +
                                stat_distrmix_line(free.mean = FALSE, fit.seed = 123)
  )

  vdiffr::expect_doppelganger("stat_distrmix_line_components_all",
                              ggplot(my.data, aes(x.2means)) +
                                stat_distrmix_line(components = "all", fit.seed = 123)
  )

  vdiffr::expect_doppelganger("stat_distrmix_line_components_sum",
                              ggplot(my.data, aes(x.2means)) +
                                stat_distrmix_line(components = "sum", fit.seed = 123)
  )

  vdiffr::expect_doppelganger("stat_distrmix_line_components_members",
                              ggplot(my.data, aes(x.2means)) +
                                stat_distrmix_line(components = "members", fit.seed = 123)
  )

})

# Even when the errors, warnings and messages are triggered´they are not "seen"
# by the tests!!
test_that("normalmix_line messages, warnings and errors", {

  skip(message = "Skipping tests of messages, warnings and errors")

  expect_message(
    ggplot(my.data, aes(x.2means)) +
      stat_distrmix_line(n.min = 101L)
  )

  expect_message(
    ggplot(my.data, aes(x.2means)) +
      stat_distrmix_line(k = 1)
  )

  expect_warning(
    ggplot(my.data, aes(x.2means)) +
      stat_distrmix_line(components = "bad_components")
  )

  expect_warning(
    ggplot(my.data, aes(x.2means)) +
      stat_distrmix_line(method = "bad_method")
  )

  expect_silent(
    ggplot(my.data, aes(x.2means)) +
      stat_distrmix_line(method = function(...) {NA})
  )

  expect_silent(
    ggplot(my.data, aes(x.2means)) +
      stat_distrmix_line(method = function(...) {NULL})
  )

  expect_silent(
    ggplot(my.data, aes(x.2means)) +
      stat_distrmix_line(method = function(...) {list()})
  )

  expect_warning(
    ggplot(my.data, aes(x.2means)) +
      stat_distrmix_line(method = function(...) {1L})
  )

  expect_warning(
    ggplot(my.data, aes(x.2means)) +
      stat_distrmix_line(method = function(...) {list(1L)})
  )

  expect_error(
    ggplot(my.data, aes(x.2means)) +
      stat_distrmix_line(method.args = list(x = NA))
  )

})
