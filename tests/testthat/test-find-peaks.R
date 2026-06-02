context("find_spikes")

test_that("returned value is good", {
  expect_type(find_spikes(rivers),
              "integer")
  expect_equal(length(find_spikes(rivers)),
               length(rivers))
})

test_that("thresholds works", {
  expect_equal(which(as.logical(find_spikes(rivers))),
               68)
  expect_equal(sum(as.logical(find_spikes(rivers, z.threshold = 30))),
               0)
  expect_equal(sum(as.logical(find_spikes(rivers, z.threshold = 2))),
               8)
  expect_equal(sum(as.logical(find_spikes(rivers, height.threshold = 30))),
               1)
  expect_equal(sum(as.logical(find_spikes(rivers, height.threshold = 2))),
               1)
  expect_equal(which(as.logical(find_spikes(rivers, z.threshold = 4))),
               c(66, 68))
  expect_equal(which(as.logical(find_spikes(rivers, z.threshold = 2.5))),
               c(8, 66, 68, 101))
})

test_that("direction works", {
  expect_equal(sum(as.logical(find_spikes(rivers, spike.direction = "up"))),
               1)
  expect_equal(sum(as.logical(find_spikes(rivers, spike.direction = "down"))),
               0)
  expect_equal(sum(as.logical(find_spikes(rivers, spike.direction = "both"))),
               1)
  expect_equal(sum(as.logical(find_spikes(rivers, spike.direction = "skip"))),
               0)
})

