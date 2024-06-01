context("equation label utilities")

options(OutDec = ".")

test_that("eq rhs well formatted", {
  expect_equal(build_eq.x.rhs(output.type = "expression", orientation = "x"),
               "~italic(x)")
  expect_equal(build_eq.x.rhs(output.type = "expression", orientation = "y"),
               "~italic(y)")
  expect_equal(build_eq.x.rhs(output.type = "latex", orientation = "x"),
               " x")
  expect_equal(build_eq.x.rhs(output.type = "latex", orientation = "y"),
               " y")
  expect_equal(build_eq.x.rhs(output.type = "markdown", orientation = "x"),
               "_x_")
  expect_equal(build_eq.x.rhs(output.type = "markdown", orientation = "y"),
               "_y_")
  expect_equal(build_eq.x.rhs(output.type = "text", orientation = "x"),
               " x")
  expect_equal(build_eq.x.rhs(output.type = "text", orientation = "y"),
               " y")
})

test_that("eq lhs well formatted", {
  expect_equal(build_lhs(output.type = "expression", orientation = "x"),
               "italic(y)~`=`~")
  expect_equal(build_lhs(output.type = "expression", orientation = "y"),
               "italic(x)~`=`~")
  expect_equal(build_lhs(output.type = "latex", orientation = "x"),
               "y = ")
  expect_equal(build_lhs(output.type = "latex", orientation = "y"),
               "x = ")
  expect_equal(build_lhs(output.type = "markdown", orientation = "x"),
               "_y_ = ")
  expect_equal(build_lhs(output.type = "markdown", orientation = "y"),
               "_x_ = ")
  expect_equal(build_lhs(output.type = "text", orientation = "x"),
               "y = ")
  expect_equal(build_lhs(output.type = "text", orientation = "y"),
               "x = ")
})

test_that("polynomial as character well formatted", {
  expect_equal(poly2character(x = 1:5, decreasing = FALSE, digits = 3, keep.zeros = TRUE),
               poly2character(x = 1:5))
  expect_equal(poly2character(x = 1:5, decreasing = FALSE, digits = 3, keep.zeros = TRUE),
               "1.00 + 2.00*x + 3.00*x^2 + 4.00*x^3 + 5.00*x^4")
  expect_equal(poly2character(x = 1:5, decreasing = FALSE, digits = 2, keep.zeros = TRUE),
               "1.0 + 2.0*x + 3.0*x^2 + 4.0*x^3 + 5.0*x^4")
  expect_equal(poly2character(x = 1:5, decreasing = FALSE, digits = 3, keep.zeros = FALSE),
               "1 + 2*x + 3*x^2 + 4*x^3 + 5*x^4")
  expect_equal(poly2character(x = 1:5, decreasing = FALSE, digits = 2, keep.zeros = FALSE),
               "1 + 2*x + 3*x^2 + 4*x^3 + 5*x^4")
  expect_equal(poly2character(x = 1:5, decreasing = TRUE, digits = 3, keep.zeros = TRUE),
               "5.00*x^4 + 4.00*x^3 + 3.00*x^2 + 2.00*x + 1.00")
  expect_equal(poly2character(x = (1:5) * 1e-3, decreasing = TRUE, digits = 3, keep.zeros = TRUE),
               "0.00500*x^4 + 0.00400*x^3 + 0.00300*x^2 + 0.00200*x + 0.00100")
  expect_equal(poly2character(x = (1:5) * 1e-6, decreasing = TRUE, digits = 3, keep.zeros = TRUE),
               "5.00e-06*x^4 + 4.00e-06*x^3 + 3.00e-06*x^2 + 2.00e-06*x + 1.00e-06")
  expect_equal(poly2character(x = (1:5) * 1e+3, decreasing = TRUE, digits = 3, keep.zeros = TRUE),
               "5.00e+03*x^4 + 4.00e+03*x^3 + 3.00e+03*x^2 + 2.00e+03*x + 1.00e+03")
  expect_warning(poly2character(x = NA))
  expect_true(is.na(suppressWarnings(poly2character(x = NA))))
  expect_equal(poly2character(x = NULL), character(0))
  expect_equal(poly2character(x = numeric()), character(0))
})


test_that("well-formatted polynomial numbers", {
  expect_equal(typeset_numbers("1.00 + 2.00*x + 3.00*x^2 + 4.00*x^3 + 5.00*x^4",
                              output.type = "expression"),
              "1.00 + 2.00*x + 3.00*x^2 + 4.00*x^3 + 5.00*x^4")
  expect_equal(typeset_numbers("0.00500*x^4 + 0.00400*x^3 + 0.00300*x^2 + 0.00200*x + 0.00100",
                              output.type = "expression"),
              "0.00500*x^4 + 0.00400*x^3 + 0.00300*x^2 + 0.00200*x + 0.00100")
  expect_equal(typeset_numbers("5.00e-06*x^4 + 4.00e-06*x^3 + 3.00e-06*x^2 + 2.00e-06*x + 1.00e-06",
                               output.type = "expression"),
               "5.00%*% 10^{-06}*x^4 + 4.00%*% 10^{-06}*x^3 + 3.00%*% 10^{-06}*x^2 + 2.00%*% 10^{-06}*x + 1.00%*% 10^{-06}")
  expect_equal(typeset_numbers("5.00e+03*x^4 + 4.00e+03*x^3 + 3.00e+03*x^2 + 2.00e+03*x + 1.000e+03",
                              output.type = "expression"),
              "5.00%*% 10^{+03}*x^4 + 4.00%*% 10^{+03}*x^3 + 3.00%*% 10^{+03}*x^2 + 2.00%*% 10^{+03}*x + 1.000%*% 10^{+03}")

  expect_equal(typeset_numbers("1.00 + 2.00*x + 3.00*x^2 + 4.00*x^3 + 5.00*x^4",
                               output.type = "latex"),
               "1.00 + 2.00x + 3.00x^2 + 4.00x^3 + 5.00x^4")
  expect_equal(typeset_numbers("0.00500*x^4 + 0.00400*x^3 + 0.00300*x^2 + 0.00200*x + 0.00100",
                               output.type = "latex"),
               "0.00500x^4 + 0.00400x^3 + 0.00300x^2 + 0.00200x + 0.00100")
  expect_equal(typeset_numbers("5.00e-06*x^4 + 4.00e-06*x^3 + 3.00e-06*x^2 + 2.00e-06*x + 1.00e-06",
                               output.type = "latex"),
               "5.00\\times{} 10^{-06}x^4 + 4.00\\times{} 10^{-06}x^3 + 3.00\\times{} 10^{-06}x^2 + 2.00\\times{} 10^{-06}x + 1.00\\times{} 10^{-06}")
  expect_equal(typeset_numbers("5.00e+03*x^4 + 4.00e+03*x^3 + 3.00e+03*x^2 + 2.00e+03*x + 1.000e+03",
                               output.type = "latex"),
               "5.00\\times{} 10^{+03}x^4 + 4.00\\times{} 10^{+03}x^3 + 3.00\\times{} 10^{+03}x^2 + 2.00\\times{} 10^{+03}x + 1.000\\times{} 10^{+03}")

  expect_equal(typeset_numbers("1.00 + 2.00*x + 3.00*x^2 + 4.00*x^3 + 5.00*x^4",
                               output.type = "markdown"),
               "1.00+2.00&nbsp;x+3.00&nbsp;x<sup>2</sup>+4.00&nbsp;x<sup>3</sup>+5.00&nbsp;x<sup>4</sup>")
  expect_equal(typeset_numbers("0.00500*x^4 + 0.00400*x^3 + 0.00300*x^2 + 0.00200*x + 0.00100",
                               output.type = "markdown"),
               "0.00500&nbsp;x<sup>4</sup>+0.00400&nbsp;x<sup>3</sup>+0.00300&nbsp;x<sup>2</sup>+0.00200&nbsp;x+0.00100")
  expect_equal(typeset_numbers("5.00e-06*x^4 + 4.00e-06*x^3 + 3.00e-06*x^2 + 2.00e-06*x + 1.00e-06",
                               output.type = "markdown"),
               "5.00&times;10<sup>-6</sup>&nbsp;x<sup>4</sup>+4.00&times;10<sup>-6</sup>&nbsp;x<sup>3</sup>+3.00&times;10<sup>-6</sup>&nbsp;x<sup>2</sup>+2.00&times;10<sup>-6</sup>&nbsp;x+1.00&times;10<sup>-6</sup>")
  expect_equal(typeset_numbers("5.00e+03*x^4 + 4.00e+03*x^3 + 3.00e+03*x^2 + 2.00e+03*x + 1.000e+03",
                               output.type = "markdown"),
               "5.00&times;10<sup>+3</sup>&nbsp;x<sup>4</sup>+4.00&times;10<sup>+3</sup>&nbsp;x<sup>3</sup>+3.00&times;10<sup>+3</sup>&nbsp;x<sup>2</sup>+2.00&times;10<sup>+3</sup>&nbsp;x+1.000&times;10<sup>+3</sup>")

  expect_equal(typeset_numbers("1.00 + 2.00*x + 3.00*x^2 + 4.00*x^3 + 5.00*x^4",
                               output.type = "text"),
               "1.00 + 2.00 x + 3.00 x^2 + 4.00 x^3 + 5.00 x^4")
  expect_equal(typeset_numbers("0.00500*x^4 + 0.00400*x^3 + 0.00300*x^2 + 0.00200*x + 0.00100",
                               output.type = "text"),
               "0.00500 x^4 + 0.00400 x^3 + 0.00300 x^2 + 0.00200 x + 0.00100")
  expect_equal(typeset_numbers("5.00e-06*x^4 + 4.00e-06*x^3 + 3.00e-06*x^2 + 2.00e-06*x + 1.00e-06",
                               output.type = "text"),
               "5.00 10^-06 x^4 + 4.00 10^-06 x^3 + 3.00 10^-06 x^2 + 2.00 10^-06 x + 1.00 10^-06")
  expect_equal(typeset_numbers("5.00e+03*x^4 + 4.00e+03*x^3 + 3.00e+03*x^2 + 2.00e+03*x + 1.000e+03",
                               output.type = "text"),
               "5.00 10^+03 x^4 + 4.00 10^+03 x^3 + 3.00 10^+03 x^2 + 2.00 10^+03 x + 1.000 10^+03")
})

test_that("well-formatted equation labels", {
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             coef.digits = 3L,
                             coef.keep.zeros = TRUE,
                             decreasing = FALSE,
                             eq.x.rhs = "x",
                             lhs = "y~`=`~",
                             output.type = "expression",
                             decimal.mark = "."),
               coefs2poly_eq(coefs = 1:5))
  expect_equal(coefs2poly_eq(coefs = 1:5),
               "y~`=`~1.00 + 2.00*x + 3.00*x^2 + 4.00*x^3 + 5.00*x^4")
  expect_equal(coefs2poly_eq(coefs = 5),
               "y~`=`~5.00")
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             coef.digits = 4L),
               "y~`=`~1.000 + 2.000*x + 3.000*x^2 + 4.000*x^3 + 5.000*x^4")
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             coef.keep.zeros = FALSE),
               "y~`=`~1 + 2*x + 3*x^2 + 4*x^3 + 5*x^4")
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             decreasing = TRUE),
               "y~`=`~5.00*x^4 + 4.00*x^3 + 3.00*x^2 + 2.00*x + 1.00")
  expect_equal(coefs2poly_eq(coefs = 5,
                             decreasing = TRUE),
               "y~`=`~5.00")
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             lhs = NULL),
               "1.00 + 2.00*x + 3.00*x^2 + 4.00*x^3 + 5.00*x^4")
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             lhs = ""),
               "1.00 + 2.00*x + 3.00*x^2 + 4.00*x^3 + 5.00*x^4")
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             eq.x.rhs = "a",
                             lhs = "b~`=`~"),
               "b~`=`~1.00 + 2.00*a + 3.00*a^2 + 4.00*a^3 + 5.00*a^4")
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             lhs = "y = ",
                             output.type = "latex"),
               "y = 1.00 + 2.00x + 3.00x^2 + 4.00x^3 + 5.00x^4")
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             lhs = "y = ",
                             output.type = "markdown"),
               "y = 1.00+2.00&nbsp;x+3.00&nbsp;x<sup>2</sup>+4.00&nbsp;x<sup>3</sup>+5.00&nbsp;x<sup>4</sup>")
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             lhs = "y = ",
                             output.type = "text"),
               "y = 1.00 + 2.00 x + 3.00 x^2 + 4.00 x^3 + 5.00 x^4")
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             decimal.mark = ","),
               "y~`=`~1.00 + 2.00*x + 3.00*x^2 + 4.00*x^3 + 5.00*x^4")
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             lhs = "y = ",
                             output.type = "latex",
                             decimal.mark = ","),
               "y = 1,00 + 2,00x + 3,00x^2 + 4,00x^3 + 5,00x^4")
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             eq.x.rhs = "_x_",
                             lhs = "_y_ = ",
                             output.type = "markdown",
                             decimal.mark = ","),
               "_y_ = 1,00+2,00&nbsp;_x_+3,00&nbsp;_x_<sup>2</sup>+4,00&nbsp;_x_<sup>3</sup>+5,00&nbsp;_x_<sup>4</sup>")
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             lhs = "y = ",
                             output.type = "text",
                             decimal.mark = ","),
               "y = 1,00 + 2,00 x + 3,00 x^2 + 4,00 x^3 + 5,00 x^4")
})

test_that("checks for bad arguments work", {
  expect_warning(coefs2poly_eq(coefs = 1:5, coef.digits = 2))
  expect_error(coefs2poly_eq(coefs = 1:5, coef.digits = 0))
})

