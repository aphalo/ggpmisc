context("stat_normalmix_line")

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

test_that("stat_normalmix_line works not attached", {
  vdiffr::expect_doppelganger("stat_normalmix_line_2means",
                              ggplot2::ggplot(my.data, ggplot2::aes(x.2means)) +
                                ggpmisc::stat_normalmix_line(seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_line_2SDs",
                              ggplot2::ggplot(my.data, ggplot2::aes(x.2SDs)) +
                                ggpmisc::stat_normalmix_line(seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_line_2means2SDs",
                              ggplot2::ggplot(my.data, ggplot2::aes(x.2each)) +
                                ggpmisc::stat_normalmix_line(seed = 123)
  )

})


library(ggpmisc)

test_that("normalmix_line works attached", {
  vdiffr::expect_doppelganger("stat_normalmix_line_geom_point",
                              ggplot(my.data, aes(x.2means)) +
                                stat_normalmix_line(geom = "point", seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_line_n",
                              ggplot(my.data, aes(x.2means)) +
                                stat_normalmix_line(geom = "point", n = 30, seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_line_geom",
                              ggplot(my.data, aes(x.2means)) +
                                stat_normalmix_line(geom = "point", n = 30, seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_line_fullrange",
                              ggplot(my.data, aes(x.2means)) +
                                stat_normalmix_line(fullrange = FALSE, seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_line_k4",
                              ggplot(my.data, aes(x.4means)) +
                                stat_normalmix_line(k = 4, seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_line_k1",
                              ggplot(my.data, aes(x)) +
                                stat_normalmix_line(k = 1, seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_line_free_sd",
                              ggplot(my.data, aes(x.2means)) +
                                stat_normalmix_line(free.sd = FALSE, seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_line_free_mean",
                              ggplot(my.data, aes(x.2SDs)) +
                                stat_normalmix_line(free.mean = FALSE, seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_line_components_all",
                              ggplot(my.data, aes(x.2means)) +
                                stat_normalmix_line(components = "all", seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_line_components_sum",
                              ggplot(my.data, aes(x.2means)) +
                                stat_normalmix_line(components = "sum", seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_line_components_members",
                              ggplot(my.data, aes(x.2means)) +
                                stat_normalmix_line(components = "members", seed = 123)
  )

})

# Even when the errors, warnings and messages are triggered´they are not "seen"
# by the tests!!
test_that("normalmix_line messages, warnings and errors", {

  skip(message = "Skipping tests of messages, warnings and errors")

  expect_message(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_line(n.min = 101L)
  )

  expect_message(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_line(k = 1)
  )

  expect_warning(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_line(components = "bad_components")
  )

  expect_warning(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_line(method = "bad_method")
  )

  expect_silent(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_line(method = function(...) {NA})
  )

  expect_silent(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_line(method = function(...) {NULL})
  )

  expect_silent(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_line(method = function(...) {list()})
  )

  expect_warning(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_line(method = function(...) {1L})
  )

  expect_warning(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_line(method = function(...) {list(1L)})
  )

  expect_error(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_line(method.args = list(x = NA))
  )

})
