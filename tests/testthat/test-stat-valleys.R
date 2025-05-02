context("stat_valleys")

library(lubridate)

make_data_tbl <- function(nrow = 100, rfun = rnorm, ...) {
  if (nrow %% 2) {
    nrow <- nrow + 1
  }

  set.seed(101)

  data.frame(
    x = 0:(nrow - 1),
    x.date = ydm("2000-01-01") + days(0:(nrow - 1)),
    x.datetime = ydm_hms("2000-01-01 00:00:00", tz = "EET") + days(0:(nrow - 1)),
    y = rfun(nrow, ...),
    group = rep(c("A", "B"), nrow / 2)
  )
}

data.rows <- 30
my.data <- make_data_tbl(data.rows)

shuffle_data <- function(data) {
  set.seed(123)
  data[sample(1:nrow(data), nrow(data)), ]
}

if (isNamespaceLoaded(name = "package:ggpmisc")) detach(package:ggpmisc, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggpp")) detach(package:ggpp, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggplot2")) detach(package:ggplot2, unload = TRUE)

test_that("valleys_noload", {
  vdiffr::expect_doppelganger("stat_valleys_numeric_noload",
                              ggplot2::ggplot(data = my.data, aes(x, y)) +
                                ggplot2::geom_point() +
                                ggplot2::geom_line() +
                                ggpmisc::stat_valleys(colour = "red") +
                                ggplot2::expand_limits(y = c(-2.5, 2.5))
  )
})

library(ggpmisc)

test_that("numbers_tb", {
  # defaults
  vdiffr::expect_doppelganger("stat_valleys_numeric_01",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  # row order insensitive
  vdiffr::expect_doppelganger("stat_valleys_numeric_s01",
                              ggplot(data = shuffle_data(my.data), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  # labels
  vdiffr::expect_doppelganger("stat_valleys_numeric__02",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(geom = "text", vjust = +1.5) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_numeric__03",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(geom = "text", vjust = +1.5,
                                           x.label.fmt = "x = %.0f") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_numeric__04",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(mapping = aes(label = after_stat(y.label)),
                                           geom = "text", hjust = 1.1,
                                           y.label.fmt = "lambda~`=`~%.2f",
                                           angle = 90,
                                           parse = TRUE) +
                              expand_limits(y = c(-3.5, 2.5))
  )
  # span
  # message triggered by handled by 'ggplot2'
  # expect_message(ggplot(data = my.data, aes(x, y)) +
  #                  stat_valleys(colour = "red", span = 10))
  vdiffr::expect_doppelganger("stat_valleys_numeric__05",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red", span = NULL) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_numeric__05a",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red", span = data.rows) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_numeric__05b",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red", span = data.rows + 1) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_numeric__06",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red", span = 11) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  # global threshold
  vdiffr::expect_doppelganger("stat_valleys_numeric_07",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red",
                                           global.threshold = 0.9) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_numeric_08",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red",
                                           global.threshold = -0.95) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  # error triggered by handled by 'ggplot2'
  # expect_error(ggplot(data = my.data, aes(x, y)) +
  #                stat_valleys(colour = "red",
  #                           global.threshold = 0.5,
  #                           threshold.scaling = "bad.input"))
  vdiffr::expect_doppelganger("stat_valleys_numeric_09",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red",
                                             global.threshold = 0.5,
                                             threshold.scaling = "none") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_numeric_09a",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red",
                                           global.threshold = -Inf,
                                           threshold.scaling = "none") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_numeric_10",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red",
                                           global.threshold = 1,
                                           threshold.scaling = "none") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_numeric_10a",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red",
                                             global.threshold = I(-1),
                                             threshold.scaling = "none") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_numeric_11",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red",
                                           global.threshold = 0.5,
                                           threshold.scaling = "data.range") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_numeric_12",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red",
                                           global.threshold = 0.5,
                                           threshold.scaling = "scale.range") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  # local threshold
  vdiffr::expect_doppelganger("stat_valleys_numeric_13",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red",
                                           local.threshold = 0.02) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_numeric_14",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red",
                                           local.threshold = 0.07,
                                           threshold.scaling = "none") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_numeric_15",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red",
                                           local.threshold = 0.02,
                                           threshold.scaling = "data.range") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_numeric_16",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red",
                                           local.threshold = 0.07,
                                           threshold.scaling = "scale.range") +
                                expand_limits(y = c(-2.5, 2.5))
  )
})

test_that("dates_tb", {
  vdiffr::expect_doppelganger("stat_valleys_date_01",
                              ggplot(data = my.data, aes(x.date, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_date_s01",
                              ggplot(data = shuffle_data(my.data), aes(x.date, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_date_02",
                              ggplot(data = my.data, aes(x.date, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(geom = "text", vjust = +1.5) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_date_03",
                              ggplot(data = my.data, aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(geom = "text", vjust = +1.5,
                                           x.label.fmt = "%b-%d") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_date_04",
                              ggplot(data = my.data, aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(geom = "text", vjust = +1.5,
                                           x.label.fmt = "lambda~`=`~\"%b-%d\"",
                                           parse = TRUE) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_date_05",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(mapping = aes(label = after_stat(y.label)),
                                           geom = "text", hjust = 1.1,
                                           y.label.fmt = "lambda~`=`~%.2f",
                                           angle = 90,
                                           parse = TRUE) +
                                expand_limits(y =  c(-3.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_date_06",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red", span = NULL) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_date_07",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red", span = 11) +
                                expand_limits(y = c(-2.5, 2.5))
  )
})

test_that("datetimes_tb", {
  vdiffr::expect_doppelganger("stat_valleys_datetime_01",
                              ggplot(data = my.data, aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_datetime_s01",
                              ggplot(data = shuffle_data(my.data), aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_datetime_02",
                              ggplot(data = my.data, aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(geom = "text", vjust = +1.5) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_datetime_03",
                              ggplot(data = my.data, aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(geom = "text", vjust = +1.5,
                                           x.label.fmt = "%b-%d") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_datetime_04",
                              ggplot(data = my.data, aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(geom = "text", vjust = +1.5,
                                           x.label.fmt = "lambda~`=`~\"%b-%d\"",
                                           parse = TRUE) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_datetime_05",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(mapping = aes(label = after_stat(y.label)),
                                           geom = "text", hjust = 1.1,
                                           y.label.fmt = "lambda~`=`~%.2f",
                                           angle = 90,
                                           parse = TRUE) +
                                expand_limits(y = c(-3.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_datetime_06",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red", span = NULL) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_valleys_datetime_07",
                              ggplot(data = my.data, aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_valleys(colour = "red", span = 11) +
                                expand_limits(y = c(-2.5, 2.5))
  )
})

test_that("many_layers", {
  if (packageVersion("ggpp") >= "0.4.4") {
    vdiffr::expect_doppelganger("stat_valleys_ml_01",
                                ggplot(mtcars, aes(mpg, hp)) +
                                  geom_line() +
                                  geom_point() +
                                  stat_valleys(span = NULL,
                                               strict = TRUE,
                                               geom = "text_s",
                                               mapping = aes(label = paste(after_stat(y.label), after_stat(x.label))),
                                               x.label.fmt = "at %.1f mpg ",
                                               y.label.fmt = "hp = %.1f",
                                               colour = "blue",
                                               colour.target = "segment",
                                               arrow = grid::arrow(length = unit(0.1, "inches")),
                                               position = position_nudge_keep(x = -1, y = -20),
                                               hjust = 1) +
                                  stat_peaks(span = 5,
                                             strict = TRUE,
                                             geom = "text_s",
                                             mapping = aes(label = paste(after_stat(y.label), after_stat(x.label))),
                                             x.label.fmt = "at %.1f mpg",
                                             y.label.fmt = "hp = %.1f\n",
                                             colour = "red",
                                             colour.target = "segment",
                                             arrow = grid::arrow(length = unit(0.1, "inches")),
                                             position = position_nudge_keep(x = 1, y = 0),
                                             hjust = 0) +
                                  expand_limits(y = 0)
    )
  }
})

