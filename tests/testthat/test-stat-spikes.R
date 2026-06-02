context("stat_spikes")

# versioning of snaps
ggplot2_version <- packageVersion("ggplot2")
if (grepl("^3\\.5\\.2\\.9|^4\\.0\\.[0-1]", ggplot2_version)) {
  ggplot2_version <- "gg-4.0.x"
} else if (grepl("^3\\.5\\.[0-2]", ggplot2_version)) {
  ggplot2_version <- "gg-3.5.x"
} else {
  ggplot2_version <- paste("gg", ggplot2_version, sep = "-")
}
R_version <- paste("R",
                   substr(as.character(getRversion()), start = 1, stop = 3),
                   sep = "-")
snap_version <- paste(R_version, ggplot2_version, sep = "_")

n = 500
set.seed(45678)
my.data <- data.frame(x = 1:n,
                      y = rep(sin((0:19)/20 * 2 * pi), n / 20) +
                          stats::rnorm(n, sd = 0.5))
my.data$y2 <- my.data$y1 <- my.data$y
selector <- sample(seq_len(n), 5)
my.data$y1[selector] <- my.data$y[selector] + 10
my.data$y2[selector] <- my.data$y[selector] - 10

if (isNamespaceLoaded(name = "package:ggpmisc")) detach(package:ggpmisc, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggpp")) detach(package:ggpp, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggplot2")) detach(package:ggplot2, unload = TRUE)

test_that("peaks_noload", {
  vdiffr::expect_doppelganger("stat_spikes1_numeric_noload",
                              ggplot2::ggplot(data = my.data, aes(x, y1)) +
                                ggplot2::geom_line() +
                                ggpmisc::stat_spikes(colour = "red")
  )
  vdiffr::expect_doppelganger("stat_spikes2_numeric_noload",
                              ggplot2::ggplot(data = my.data, aes(x, y2)) +
                                ggplot2::geom_line() +
                                ggpmisc::stat_spikes(colour = "blue")
  )
})

library(ggpmisc)

test_that("numbers_tb", {
  # defaults
  vdiffr::expect_doppelganger("stat_spikes_numeric_01",
                              ggplot(data = my.data, aes(x, y1)) +
                                geom_line() +
                                stat_spikes(colour = "red") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  # labels
  vdiffr::expect_doppelganger("stat_spikes_numeric__02",
                              ggplot(data = my.data, aes(x, y1)) +
                                geom_line() +
                                stat_spikes(geom = "text", vjust = -0.5) +
                                expand_limits(y = c(-2.5, 12)),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_spikes_numeric__03",
                              ggplot(data = my.data, aes(x, y1)) +
                                geom_line() +
                                stat_spikes(geom = "text", vjust = -0.5,
                                           x.label.fmt = "x = %.0f") +
                                expand_limits(y = c(-2.5, 12)),
                              variant = snap_version
  )
  vdiffr::expect_doppelganger("stat_peaks_numeric__04",
                              ggplot(data = my.data, aes(x, y1)) +
                                geom_line() +
                                stat_spikes(mapping = aes(label = after_stat(y.label)),
                                            geom = "text", hjust = -0.1,
                                            y.label.fmt = "x~`=`~%.2f",
                                            angle = 90,
                                            parse = TRUE) +
                                expand_limits(y = c(-2.5, 14)),
                              variant = snap_version
  )
  # z.threshold
  vdiffr::expect_doppelganger("stat_spikes_numeric__05",
                              ggplot(data = my.data, aes(x, y1)) +
                                geom_line() +
                                stat_spikes(colour = "red", z.threshold = 2)
  )
  vdiffr::expect_doppelganger("stat_spikes_numeric__05a",
                              ggplot(data = my.data, aes(x, y1)) +
                                geom_line() +
                                stat_spikes(colour = "red", z.threshold = 50)
  )
  # height.threshold
  vdiffr::expect_doppelganger("stat_spikes_numeric__06",
                              ggplot(data = my.data, aes(x, y1)) +
                                geom_line() +
                                stat_spikes(colour = "red", height.threshold = 2)
  )
  vdiffr::expect_doppelganger("stat_spikes_numeric__06a",
                              ggplot(data = my.data, aes(x, y1)) +
                                geom_line() +
                                stat_spikes(colour = "red", height.threshold = 400)
  )
  vdiffr::expect_doppelganger("stat_spikes_numeric__06c",
                              ggplot(data = my.data, aes(x, y1)) +
                                geom_line() +
                                stat_spikes(colour = "red",
                                            height.threshold = NA)
  )
  # spike.direction
  vdiffr::expect_doppelganger("stat_spikes_numeric__07",
                              ggplot(data = my.data, aes(x, y1)) +
                                geom_line() +
                                stat_spikes(colour = "red",
                                            spike.direction = "up")
  )
  vdiffr::expect_doppelganger("stat_spikes_numeric__07a",
                              ggplot(data = my.data, aes(x, y2)) +
                                geom_line() +
                                stat_spikes(colour = "blue",
                                            spike.direction = "down")
  )
  vdiffr::expect_doppelganger("stat_spikes_numeric__07b",
                              ggplot(data = my.data, aes(x, y1)) +
                                geom_line() +
                                stat_spikes(colour = "orange",
                                            spike.direction = "both")
  )
  vdiffr::expect_doppelganger("stat_spikes_numeric__07c",
                              ggplot(data = my.data, aes(x, y2)) +
                                geom_line() +
                                stat_spikes(colour = "orange",
                                            spike.direction = "both")
  )
  vdiffr::expect_doppelganger("stat_spikes_numeric__07d",
                              ggplot(data = my.data, aes(x, y1)) +
                                geom_line() +
                                stat_spikes(spike.direction = "skip")
  )

})

test_that("works with time series", {
  vdiffr::expect_doppelganger("stat_spikes_ts_01",
                              ggplot(Nile) +
                                geom_line() +
                                stat_spikes()
  )
  vdiffr::expect_doppelganger("stat_spikes_ts_02",
                              ggplot(Nile, as.numeric = FALSE) +
                                geom_line() +
                                stat_spikes()
  )
  vdiffr::expect_doppelganger("stat_spikes_ts_03",
                              ggplot(Nile, as.numeric = FALSE, time.resolution = "hour") +
                                geom_line() +
                                stat_spikes()
  )
})
