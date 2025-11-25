context("stat_normalmix_eq")

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

test_that("stat_normalmix_eq works not attached", {
  vdiffr::expect_doppelganger("stat_normalmix_eq_2means",
                              ggplot2::ggplot(my.data, ggplot2::aes(x.2means)) +
                                stat_normalmix_line(seed = 123) +
                                ggpmisc::stat_normalmix_eq(seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_eq_2SDs",
                              ggplot2::ggplot(my.data, ggplot2::aes(x.2SDs)) +
                                stat_normalmix_line(seed = 123) +
                                ggpmisc::stat_normalmix_eq(seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_eq_2means2SDs",
                              ggplot2::ggplot(my.data, ggplot2::aes(x.2each)) +
                                stat_normalmix_line(seed = 123) +
                                ggpmisc::stat_normalmix_eq(seed = 123)
  )

})


library(ggpmisc)

test_that("normalmix_eq works attached", {
  vdiffr::expect_doppelganger("stat_normalmix_eq_geom_text",
                              ggplot(my.data, aes(x.2means)) +
                                stat_normalmix_line(seed = 123) +
                                stat_normalmix_eq(geom = "text", seed = 123, hjust = 0)
  )

  vdiffr::expect_doppelganger("stat_normalmix_eq_geom_label",
                              ggplot(my.data, aes(x.2means)) +
                                stat_normalmix_line(seed = 123) +
                                stat_normalmix_eq(geom = "label", seed = 123, hjust = 0)
  )

  vdiffr::expect_doppelganger("stat_normalmix_eq_geom_label_npc",
                              ggplot(my.data, aes(x.2means)) +
                                stat_normalmix_line() +
                                stat_normalmix_eq(geom = "label_npc", seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_eq_fullrange",
                              ggplot(my.data, aes(x.2means)) +
                                stat_normalmix_line(fullrange = FALSE, seed = 123) +
                                stat_normalmix_eq(seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_eq_k4",
                              ggplot(my.data, aes(x.4means)) +
                                stat_normalmix_line(k = 4, seed = 123) +
                                stat_normalmix_eq(k = 4, seed = 123) +
                                expand_limits(y = 0.21)
  )

  vdiffr::expect_doppelganger("stat_normalmix_eq_k1",
                              ggplot(my.data, aes(x)) +
                                stat_normalmix_line(k = 1, seed = 123) +
                                stat_normalmix_eq(k = 1, seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_eq_free_sd",
                              ggplot(my.data, aes(x.2means)) +
                                stat_normalmix_line(free.sd = FALSE, seed = 123) +
                                stat_normalmix_eq(free.sd = FALSE, seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_eq_free_mean",
                              ggplot(my.data, aes(x.2SDs)) +
                                stat_normalmix_eq(free.mean = FALSE, seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_eq_components_all",
                              ggplot(my.data, aes(x.2means)) +
                                stat_normalmix_line(components = "all", seed = 123) +
                                stat_normalmix_eq(components = "all", seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_eq_components_sum",
                              ggplot(my.data, aes(x.2means)) +
                                stat_normalmix_line(components = "sum", seed = 123) +
                                stat_normalmix_eq(components = "sum", seed = 123)
  )

  vdiffr::expect_doppelganger("stat_normalmix_eq_components_members",
                              ggplot(my.data, aes(x.2means)) +
                                stat_normalmix_line(components = "members", seed = 123) +
                                stat_normalmix_eq(components = "members", seed = 123)
  )

})

# Even when the errors, warnings and messages are triggered´they are not "seen"
# by the tests!!
test_that("normalmix_eq messages, warnings and errors", {

  skip(message = "Skipping tests of messages, warnings and errors")

  expect_message(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_eq(n.min = 101L)
  )

  expect_message(
    ggplot(my.data, aes(x)) +
      stat_normalmix_eq(k = 1)
  )

  expect_warning(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_eq(components = "bad_components")
  )

  expect_warning(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_eq(method = "bad_method")
  )

  expect_silent(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_eq(method = function(...) {NA})
  )

  expect_silent(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_eq(method = function(...) {NULL})
  )

  expect_silent(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_eq(method = function(...) {list()})
  )

  expect_warning(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_eq(method = function(...) {1L})
  )

  expect_warning(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_eq(method = function(...) {list(1L)})
  )

  expect_error(
    ggplot(my.data, aes(x.2means)) +
      stat_normalmix_eq(method.args = list(x = NA))
  )

})
