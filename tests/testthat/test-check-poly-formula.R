context("check_poly_formula")

test_that("check poly passed with constants", {
  expect_true(check_poly_formula(y ~ 1))
  expect_true(check_poly_formula(y ~ 0))
 })

test_that("check poly passed with x", {
  expect_true(check_poly_formula(y ~ x))
  expect_true(check_poly_formula(y ~ 1 + x))
  expect_true(check_poly_formula(y ~ 0 + x))
  expect_true(check_poly_formula(y ~ -1 + x))
  expect_true(check_poly_formula(y ~ x + 0))
  expect_true(check_poly_formula(y ~ x - 1))
})

test_that("check poly passed with increasing powers", {
  expect_true(check_poly_formula(y ~ x + I(x^2)))
  expect_true(check_poly_formula(y ~ 1 + x + I(x^2)))
  expect_true(check_poly_formula(y ~ 0 + x + I(x^2)))
  expect_true(check_poly_formula(y ~ -1 + x + I(x^2)))
  expect_true(check_poly_formula(y ~ x + I(x^2) + 0))
  expect_true(check_poly_formula(y ~ x + I(x^2) - 1))
  expect_true(check_poly_formula(y ~ x + I(x^2) + I(x^3)))
  expect_true(check_poly_formula(y ~ x + I(x^2) + I(x^3) + I(x^4)))
  expect_true(check_poly_formula(y ~ I(x) + I(x^2) + I(x^3) + I(x^4)))
})

test_that("check poly fails with decreasing powers", {
  expect_false(suppressWarnings(check_poly_formula(y ~ I(x^2) + x)))
  expect_false(suppressWarnings(check_poly_formula(y ~ I(x^3) + I(x^2) + x)))
  expect_false(check_poly_formula(y ~ I(x^3) + I(x^2) + x, warning.text = NULL))
})

test_that("check poly warns with decreasing powers", {
  expect_warning(check_poly_formula(y ~ I(x^2) + x))
  expect_warning(check_poly_formula(y ~ I(x^3) + I(x^2) + x))
  expect_no_warning(check_poly_formula(y ~ I(x^3) + I(x^2) + x, warning.text = NULL))
})

test_that("check poly errors with mixed power and poly", {
  expect_error(check_poly_formula(y ~ I(x^2) + poly(x, 3, raw = TRUE)))
  expect_error(check_poly_formula(y ~ poly(x, 3, raw = TRUE) + I(x^2)))
})

test_that("check poly fails with missing as is", {
  expect_false(suppressWarnings(check_poly_formula(y ~ x + x^2)))
  expect_false(suppressWarnings(check_poly_formula(y ~ 1 + x + x^2)))
  expect_false(suppressWarnings(check_poly_formula(y ~ 0 + x + x^2)))
  expect_false(suppressWarnings(check_poly_formula(y ~ -1 + x + x^2)))
  expect_false(suppressWarnings(check_poly_formula(y ~ x + x^2 + I(x^3))))
  expect_false(suppressWarnings(check_poly_formula(y ~ x + I(x^2) + I(x^3) + x^4)))
})

test_that("check poly warns with misisng as is", {
  expect_warning(check_poly_formula(y ~ x + x^2))
  expect_warning(check_poly_formula(y ~ 1 + x + x^2))
  expect_warning(check_poly_formula(y ~ 0 + x + x^2))
  expect_warning(check_poly_formula(y ~ -1 + x + x^2))
  expect_warning(check_poly_formula(y ~ x + x^2 + I(x^3)))
  expect_warning(check_poly_formula(y ~ x + I(x^2) + I(x^3) + x^4))
})


test_that("check poly passed with varied white space", {
  expect_true(check_poly_formula(y~x+I(x^2)))
  expect_true(check_poly_formula(y~x+I(x^2)+I(x^3)))
  expect_true(check_poly_formula(y~x+I(x^2)+I(x^3)+I(x^4)))
  expect_true(check_poly_formula(y  ~  x +  I( x ^ 2)))
})

test_that("check poly passed with poly()", {
  expect_true(check_poly_formula(y ~ poly(x, 2, raw = TRUE)))
  expect_true(check_poly_formula(y ~ 1 + poly(x, 2, raw = TRUE)))
  expect_true(check_poly_formula(y ~ 0 + poly(x, 2, raw = TRUE)))
  expect_true(check_poly_formula(y ~ -1 + poly(x, 2, raw = TRUE)))
  expect_true(check_poly_formula(y ~ poly(x, 4, raw = TRUE)))
})

test_that("check poly gives warning without raw named parameter", {
  expect_warning(check_poly_formula(y ~ poly(x, 2)))
  expect_warning(check_poly_formula(y ~ 1 + poly(x, 2)))
  expect_warning(check_poly_formula(y ~ 0 + poly(x, 2)))
  expect_warning(check_poly_formula(y ~ -1 + poly(x, 2)))
})

