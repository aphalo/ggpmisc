context("find peaks")

test_that("span works", {
  expect_equal(which(find_peaks(rivers,
                                span = NULL)),
               68)
  expect_equal(which(find_peaks(rivers,
                                span = 200)),
               68)
  expect_equal(length(find_peaks(rivers,
                                 span = 1)),
               length(rivers))
  expect_equal(which(find_peaks(rivers,
                                span = 31)),
               c(23, 68, 101))
  expect_equal(which(find_peaks(rivers,
                                span = 11)),
               c(7, 23, 32, 38, 44, 50, 68, 83, 89, 101, 115, 121, 131))
})

test_that("global.threshold works", {
  expect_equal(which(find_peaks(rivers,
                                span = 11,
                                global.threshold = 1)),
               integer(0))
  expect_equal(which(find_peaks(rivers,
                                span = 11,
                                global.threshold = 0)),
               c(7, 23, 32, 38, 44, 50, 68, 83, 89, 101, 115, 121, 131))
  expect_equal(which(find_peaks(rivers,
                                span = 11,
                                global.threshold = 0.5)),
               68)
  expect_equal(which(find_peaks(rivers,
                                span = 11,
                                global.threshold = 0.35)),
               c(7, 23, 68, 101))
  expect_equal(which(find_peaks(rivers,
                                span = 11,
                                global.threshold = -0.25)),
               c(32, 38, 44, 50, 131))
  expect_equal(which(find_peaks(rivers,
                                span = 11,
                                global.threshold = 1000,
                                threshold.range = 0:1)),
               c(7, 23, 68, 83, 89, 101, 115, 121))
  expect_equal(which(find_peaks(rivers,
                                span = 11,
                                global.threshold = -750,
                                threshold.range = 0:1)),
               c(32, 44, 50))
})

test_that("local.threshold works", {
  expect_equal(which(find_peaks(rivers,
                                span = 11,
                                local.threshold = 1)),
               integer(0))
  expect_equal(which(find_peaks(rivers,
                                span = 11,
                                local.threshold = 0)),
               c(7, 23, 32, 38, 44, 50, 68, 83, 89, 101, 115, 121, 131))
  expect_equal(which(find_peaks(rivers,
                                span = 5,
                                local.threshold = 0.95)),
               integer(0))
  expect_equal(which(find_peaks(rivers,
                                span = 5,
                                local.threshold = 0.5,
                                local.reference = "farthest")),
               68)
  expect_equal(which(find_peaks(rivers,
                                span = 11,
                                local.threshold = 0.5,
                                local.reference = "farthest")),
               68)
  expect_equal(which(find_peaks(rivers,
                                span = 11,
                                local.threshold = 0.5,
                                local.reference = "median")),
               68)
  expect_equal(which(find_peaks(rivers,
                                span = 11,
                                local.threshold = 0.25,
                                local.reference = "farthest")),
               c(7, 23, 68, 83, 101, 115))
  expect_equal(which(find_peaks(rivers,
                                span = 11,
                                local.threshold = 0.25,
                                local.reference = "median")),
               c(7, 68, 101))
  expect_equal(which(find_peaks(rivers,
                                span = 5,
                                local.threshold = 1000,
                                threshold.range = 0:1)),
               c(7, 68, 101))
  expect_equal(which(find_peaks(rivers,
                                span = 11,
                                local.threshold = 2000,
                                threshold.range = 0:1)),
               68)
  expect_equal(which(find_peaks(rivers,
                                span = 11,
                                local.threshold = 2000,
                                threshold.range = 1:0)),
               68)
  expect_error(find_peaks(rivers,
                          span = 11,
                          local.threshold = 2000))
  expect_equal(which(find_peaks(rivers,
                                span = 5,
                                local.threshold = 0.5)),
               68)
})
