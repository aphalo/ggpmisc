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
               "5.00e+03*x^4 + 4.00e+03*x^3 + 3.00e+03*x^2 + 2.00e+03*x + 1.000e+03")
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
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             coef.digits = 4L),
               "y~`=`~1.000 + 2.000*x + 3.000*x^2 + 4.000*x^3 + 5.000*x^4")
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             coef.keep.zeros = FALSE),
               "y~`=`~1 + 2*x + 3*x^2 + 4*x^3 + 5*x^4")
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             decreasing = TRUE),
               "y~`=`~5.00*x^4 + 4.00*x^3 + 3.00*x^2 + 2.00*x + 1.00")
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
                             lhs = "y = ",
                             output.type = "markdown",
                             decimal.mark = ","),
               "y = 1,00+2,00&nbsp;x+3,00&nbsp;x<sup>2</sup>+4,00&nbsp;x<sup>3</sup>+5,00&nbsp;x<sup>4</sup>")
  expect_equal(coefs2poly_eq(coefs = 1:5,
                             lhs = "y = ",
                             output.type = "text",
                             decimal.mark = ","),
               "y = 1,00 + 2,00 x + 3,00 x^2 + 4,00 x^3 + 5,00 x^4")
})

test_that("well-formatted F-value labels", {
  expect_equal(f_value_label(value = 1e2),
               "italic(F)~`=`~\"100.0\"")
  expect_equal(f_value_label(value = 1e2, df1 = 1, df2 = 25),
               "italic(F)[1*\",\"*25]~`=`~\"100.0\"")
  expect_equal(f_value_label(value = -1e-14),
               "italic(F)~`=`~\"0.000\"")
  expect_warning(f_value_label(value = -1))
  expect_true(is.na(suppressWarnings(f_value_label(value = -1))))
  expect_equal(f_value_label(value = 100, output.type = "expression"),
               "italic(F)~`=`~\"100.0\"")
  expect_equal(f_value_label(value = 100, digits = 2, output.type = "expression"),
               "italic(F)~`=`~\"1.0\" %*% 10^{\"+02\"}")
  expect_equal(f_value_label(value = 100, digits = 5, output.type = "expression"),
               "italic(F)~`=`~\"100.00\"")
  expect_equal(f_value_label(value = 100, digits = Inf, output.type = "expression"),
               "italic(F)~`=`~\"1.0\" %*% 10^{\"+02\"}")

  expect_equal(f_value_label(value = 100, output.type = "latex"),
               "\\mathit{F} = 100.0")
  expect_equal(f_value_label(value = 100, df1 = 1, df2 = 25, output.type = "latex"),
               "F_{1,25} = 100.0")

  expect_equal(f_value_label(value = 100, output.type = "markdown"),
               "_F_ = 100.0")
  expect_equal(f_value_label(value = 100, df1 = 1, df2 = 25, output.type = "markdown"),
               "_F_<sub>1,25</sub> = 100.0")

  expect_equal(f_value_label(value = 100, output.type = "text"),
               "F = 100.0")
  expect_equal(f_value_label(value = 100, df1 = 1, df2 = 25, output.type = "text"),
               "F(1,25) = 100.0")
})

test_that("well-formatted t-value labels", {
  expect_equal(t_value_label(value = 12),
               "italic(t)~`=`~\"12.00\"")
  expect_true(is.na(t_value_label(value = NA)))
  expect_true(is.na(t_value_label(value = 0/0)))
  expect_equal(t_value_label(value = 12, output.type = "expression"),
               "italic(t)~`=`~\"12.00\"")
  expect_equal(t_value_label(value = 12, df = 5, output.type = "expression"),
               "italic(t)[5]~`=`~\"12.00\"")
  expect_equal(t_value_label(value = 100, df = 10, output.type = "expression"),
               "italic(t)[10]~`=`~\"100.0\"")
  expect_equal(t_value_label(value = 100, digits = 2, output.type = "expression"),
               "italic(t)~`=`~\"1.0\" %*% 10^{\"+02\"}")
  expect_equal(t_value_label(value = 100, digits = 5, output.type = "expression"),
               "italic(t)~`=`~\"100.00\"")
  expect_equal(t_value_label(value = 100, digits = Inf, output.type = "expression"),
               "italic(t)~`=`~\"1.0\" %*% 10^{\"+02\"}")
  expect_equal(t_value_label(value = 100, df = 10, digits = Inf, output.type = "expression"),
               "italic(t)[10]~`=`~\"1.0\" %*% 10^{\"+02\"}")

  expect_equal(t_value_label(value = 12, output.type = "latex"),
               "\\mathit{t} = 12.00")
  expect_equal(t_value_label(value = 12, output.type = "markdown"),
               "_t_ = 12.00")
  expect_equal(t_value_label(value = 12, df = 5, output.type = "markdown"),
               "_t_<sub>5</sub> = 12.00")

  expect_equal(t_value_label(value = 12, output.type = "text"),
               "t = 12.00")
  expect_equal(t_value_label(value = 12, df = 5, output.type = "text"),
               "t(5) = 12.00")
})

test_that("well-formatted z labels", {
  expect_equal(z_value_label(value = 12),
               "italic(z)~`=`~\"12.00\"")
})

test_that("well-formatted S labels", {
  expect_equal(S_value_label(value = 12),
               "italic(S)~`=`~\"12.00\"")
})

test_that("well-formatted sd labels", {
  expect_equal(sd_value_label(value = 12),
               "italic(sigma)~`=`~\"12.00\"")
  expect_equal(sd_value_label(value = 12, output.type = "latex"),
               "\\mathit{\\sigma} = 12.00")
  expect_equal(sd_value_label(value = 12, output.type = "markdown"),
               "_&sigma;_ = 12.00")
  expect_equal(sd_value_label(value = 12, output.type = "text"),
               "s.d. = 12.00")
})

test_that("well-formatted se labels", {
  expect_equal(se_value_label(value = 12),
               "italic(s.e.)~`=`~\"12.00\"")
  expect_equal(se_value_label(value = 12, output.type = "latex"),
               "\\mathit{s.e.} = 12.00")
  expect_equal(se_value_label(value = 12, output.type = "markdown"),
               "_s.e._ = 12.00")
  expect_equal(se_value_label(value = 12, output.type = "text"),
               "s.e. = 12.00")
})

test_that("well-formatted var labels", {
  expect_equal(var_value_label(value = 12),
               "italic(sigma^2)~`=`~\"12.00\"")
  expect_equal(var_value_label(value = 12, output.type = "latex"),
               "\\mathit{\\sigma^2} = 12.00")
  expect_equal(var_value_label(value = 12, output.type = "markdown"),
               "_&sigma;<sup>2</sup>_ = 12.00")
  expect_equal(var_value_label(value = 12, output.type = "text"),
               "s^2 = 12.00")
})

test_that("well-formatted mean labels", {
  expect_equal(mean_value_label(value = 12),
               "italic(bar(x))~`=`~\"12.00\"")
  expect_equal(mean_value_label(value = 12, output.type = "latex"),
               "\\mathit{\\bar{x}} = 12.00")
  expect_equal(mean_value_label(value = 12, output.type = "markdown"),
               "_mean(x)_ = 12.00")
  expect_equal(mean_value_label(value = 12, output.type = "text"),
               "mean(x) = 12.00")
})

test_that("well-formatted R labels", {
  expect_equal(r_label(value = 0.5),
               "italic(R)~`=`~\"0.500\"")
  expect_true(is.na(r_label(value = NA)))
  expect_true(is.na(r_label(value = 0/0)))
  expect_equal(r_label(value = -1e-14),
               "|italic(R)|~ < ~\"0.001\"")
  expect_equal(r_label(value = -1-1e-14),
               "italic(R)~`=`~\"-1.000\"")
  expect_equal(r_label(value = +1+1e-14),
               "italic(R)~`=`~\"1.000\"")
  expect_equal(r_label(value = 0.5, method = "pearson"),
               "italic(R)~`=`~\"0.500\"")
  expect_equal(r_label(value = 0.5, method = "kendall"),
               "italic(tau)~`=`~\"0.500\"")
  expect_equal(r_label(value = 0.5, method = "spearman"),
               "italic(rho)~`=`~\"0.500\"")
  expect_equal(r_label(value = 0.5, method = "abc"),
               "abc~`=`~\"0.500\"")
  expect_equal(r_label(value = -0.5),
               "italic(R)~`=`~\"-0.500\"")

  expect_equal(r_label(value = 0.5, output.type = "expression"),
               "italic(R)~`=`~\"0.500\"")
  expect_equal(r_label(value = -1e-14, output.type = "expression"),
               "|italic(R)|~ < ~\"0.001\"")
  expect_equal(r_label(value = 0.5, digits = 2, output.type = "expression"),
               "italic(R)~`=`~\"0.50\"")
  expect_equal(r_label(value = 0.5, digits = 5, output.type = "expression"),
               "italic(R)~`=`~\"0.50000\"")
  expect_equal(r_label(value = 0.5, digits = Inf, output.type = "expression"),
               "italic(R)~`=`~\"0.50\"")
  expect_equal(r_label(value = 0.5, output.type = "expression", small.r = TRUE),
               "italic(r)~`=`~\"0.500\"")

  expect_equal(r_label(value = 0.5, output.type = "latex"),
               "R = 0.500")
  expect_equal(r_label(value = -1e-14, output.type = "latex"),
               "|R| < 0.001")
  expect_equal(r_label(value = 0.5, method = "kendall", output.type = "latex"),
               "\tau = 0.500")
  expect_equal(r_label(value = 0.5, method = "spearman", output.type = "latex"),
               "\rho = 0.500")
  expect_equal(r_label(value = 0.5, digits = 2, output.type = "latex"),
               "R = 0.50")
  expect_equal(r_label(value = 0.5, digits = 5, output.type = "latex"),
               "R = 0.50000")
  expect_equal(r_label(value = 0.5, digits = Inf, output.type = "latex"),
               "R = 0.50")
  expect_equal(r_label(value = 0.5, output.type = "latex", small.r = TRUE),
               "r = 0.500")

  expect_equal(r_label(value = 0.5, output.type = "markdown"),
               "_R_ = 0.500")
  expect_equal(r_label(value = -1e-14, output.type = "markdown"),
               "|_R_| < 0.001")
  expect_equal(r_label(value = 0.5, method = "kendall", output.type = "markdown"),
               "_&rho;_ = 0.500")
  expect_equal(r_label(value = 0.5, method = "spearman", output.type = "markdown"),
               "_&tau;_ = 0.500")
  expect_equal(r_label(value = 0.5, method = "abc", output.type = "markdown"),
               "abc = 0.500")
  expect_equal(r_label(value = 0.5, digits = 2, output.type = "markdown"),
               "_R_ = 0.50")
  expect_equal(r_label(value = 0.5, digits = 5, output.type = "markdown"),
               "_R_ = 0.50000")
  expect_equal(r_label(value = 0.5, digits = Inf, output.type = "markdown"),
               "_R_ = 0.50")
  expect_equal(r_label(value = 0.5, output.type = "markdown", small.r = TRUE),
               "_r_ = 0.500")

  expect_equal(r_label(value = 0.5, output.type = "text"),
               "R = 0.500")
  expect_equal(r_label(value = -1e-14, output.type = "text"),
               "|R| < 0.001")
  expect_equal(r_label(value = 0.5, method = "kendall", output.type = "text"),
               "tau = 0.500")
  expect_equal(r_label(value = 0.5, method = "spearman", output.type = "text"),
               "rho = 0.500")
  expect_equal(r_label(value = 0.5, method = "abc", output.type = "text"),
               "abc = 0.500")

  expect_warning(r_label(value = 0.5, digits = 1))
  expect_warning(r_label(value = 1.1))
  expect_true(is.na(suppressWarnings(r_label(value = 1.1))))
  expect_warning(r_label(value = -1.1))
  expect_true(is.na(suppressWarnings(r_label(value = -1.1))))
})

test_that("well-formatted R2 labels", {
  expect_equal(rr_label(value = 0.5),
               "italic(R)^2~`=`~\"0.500\"")
  expect_true(is.na(rr_label(value = NA)))
  expect_true(is.na(rr_label(value = 0/0)))
  expect_equal(rr_label(value = -1e-14),
               "italic(R)^2~`=`~\"0.000\"")
  expect_equal(rr_label(value = +1+1e-14),
               "italic(R)^2~`=`~\"1.000\"")
  expect_equal(rr_label(value = 1e-14),
               "italic(R)^2~`<`~\"0.001\"")
  expect_equal(rr_label(value = 0.5, output.type = "expression"),
               "italic(R)^2~`=`~\"0.500\"")
  expect_equal(rr_label(value = 0.5, digits = 2, output.type = "expression"),
               "italic(R)^2~`=`~\"0.50\"")
  expect_equal(rr_label(value = 0.5, digits = 5, output.type = "expression"),
               "italic(R)^2~`=`~\"0.50000\"")
  expect_equal(rr_label(value = 0.5, digits = Inf, output.type = "expression"),
               "italic(R)^2~`=`~\"0.50\"")
  expect_equal(rr_label(value = 0.5, output.type = "expression", small.r = TRUE),
               "italic(r)^2~`=`~\"0.500\"")

  expect_equal(rr_label(value = 0.5, output.type = "latex"),
               "R^2 = 0.500")
  expect_equal(rr_label(value = 1e-14, output.type = "latex"),
               "R^2 < 0.001")
  expect_equal(rr_label(value = 0.5, digits = 2, output.type = "latex"),
               "R^2 = 0.50")
  expect_equal(rr_label(value = 0.5, digits = 5, output.type = "latex"),
               "R^2 = 0.50000")
  expect_equal(rr_label(value = 0.5, digits = Inf, output.type = "latex"),
               "R^2 = 0.50")
  expect_equal(rr_label(value = 0.5, output.type = "latex", small.r = TRUE),
               "r^2 = 0.500")

  expect_equal(rr_label(value = 0.5, output.type = "markdown"),
               "_R_<sup>2</sup> = 0.500")
  expect_equal(rr_label(value = 1e-14, output.type = "markdown"),
               "_R_<sup>2</sup> < 0.001")
  expect_equal(rr_label(value = 0.5, digits = 2, output.type = "markdown"),
               "_R_<sup>2</sup> = 0.50")
  expect_equal(rr_label(value = 0.5, digits = 5, output.type = "markdown"),
               "_R_<sup>2</sup> = 0.50000")
  expect_equal(rr_label(value = 0.5, digits = Inf, output.type = "markdown"),
               "_R_<sup>2</sup> = 0.50")
  expect_equal(rr_label(value = 0.5, output.type = "markdown", small.r = TRUE),
               "_r_<sup>2</sup> = 0.500")

  expect_equal(rr_label(value = 0.5, output.type = "text"),
               "R^2 = 0.500")
  expect_equal(rr_label(value = 1e-14, output.type = "text"),
               "R^2 < 0.001")
  expect_equal(rr_label(value = 0.5, digits = 2, output.type = "text"),
               "R^2 = 0.50")
  expect_equal(rr_label(value = 0.5, digits = 5, output.type = "text"),
               "R^2 = 0.50000")
  expect_equal(rr_label(value = 0.5, digits = Inf, output.type = "text"),
               "R^2 = 0.50")
  expect_equal(rr_label(value = 0.5, output.type = "text", small.r = TRUE),
               "r^2 = 0.500")

  expect_warning(rr_label(value = 0.5, digits = 1))
  expect_warning(rr_label(value = 1.1))
  expect_warning(rr_label(value = -0.1))
  expect_true(is.na(suppressWarnings(rr_label(value = 1.1))))
  expect_true(is.na(suppressWarnings(rr_label(value = -0.1))))
})

test_that("well-formatted adjusted R2 labels", {
  expect_equal(adj_rr_label(value = 0.5),
               "italic(R)[adj]^2~`=`~\"0.500\"")
  expect_true(is.na(adj_rr_label(value = NA)))
  expect_true(is.na(adj_rr_label(value = 0/0)))
  expect_equal(adj_rr_label(value = -1e-14),
               "italic(R)[adj]^2~`<`~\"0.001\"")
  expect_equal(adj_rr_label(value = +1+1e-14),
               "italic(R)[adj]^2~`=`~\"1.000\"")
  expect_equal(adj_rr_label(value = 0.5, output.type = "expression"),
               "italic(R)[adj]^2~`=`~\"0.500\"")
  expect_equal(adj_rr_label(value = 0.5, output.type = "latex"),
               "R_{adj}^2 = 0.500")
  expect_equal(adj_rr_label(value = 0.5, output.type = "markdown"),
               "_R_<sup>2</sup><sub>adj</sub> = 0.500")
  expect_equal(adj_rr_label(value = 0.5, output.type = "text"),
               "R_{adj}^2 = 0.500")
  expect_warning(adj_rr_label(value = 0.5, digits = 1))
  expect_warning(adj_rr_label(value = 1.1))
  expect_no_warning(adj_rr_label(value = -0.1))
  expect_true(is.na(suppressWarnings(adj_rr_label(value = 1.1))))
  expect_false(is.na(suppressWarnings(adj_rr_label(value = -0.1))))
})

test_that("well-formatted R2 CI labels", {
  expect_equal(rr_ci_label(value = c(0.5, 0.7), conf.level = .95),
               "\"95% CI [0.50, 0.70]\"")
  expect_equal(rr_ci_label(value = c(0.5, 0.7), conf.level = .995),
               "\"99.5% CI [0.50, 0.70]\"")
  expect_equal(rr_ci_label(value = c(0.5, 0.7), conf.level = .95, range.brackets = c("(", ")")),
               "\"95% CI (0.50, 0.70)\"")
  expect_equal(rr_ci_label(value = c(-1e-14, 1+1e-14), conf.level = .95),
               "\"95% CI [0.00, 1.00]\"")
  expect_warning(rr_ci_label(value = c(0.7, 0.5), conf.level = .95))
  expect_equal(suppressWarnings(rr_ci_label(value = c(0, 1.1), conf.level = .95)),
               "\"95% CI [0.00, NA]\"")
  expect_warning(rr_ci_label(value = c(0, 1.1), conf.level = .95))
  expect_equal(suppressWarnings(rr_ci_label(value = c(0, 1.1), conf.level = .95)),
               "\"95% CI [0.00, NA]\"")
  expect_warning(rr_ci_label(value = c(-0.1, 1), conf.level = .95))
  expect_equal(suppressWarnings(rr_ci_label(value = c(-0.1, 1), conf.level = .95)),
               "\"95% CI [NA, 1.00]\"")

  expect_equal(rr_ci_label(value = c(0.5, 0.7), conf.level = .95, output.type = "expression"),
               "\"95% CI [0.50, 0.70]\"")
  expect_equal(rr_ci_label(value = c(0.5, 0.7), conf.level = .95, output.type = "latex"),
               "95% CI [0.50, 0.70]")
  expect_equal(rr_ci_label(value = c(0.5, 0.7), conf.level = .95, output.type = "markdown"),
               "95% CI [0.50, 0.70]")
  expect_equal(rr_ci_label(value = c(0.5, 0.7), conf.level = .95, output.type = "text"),
               "95% CI [0.50, 0.70]")
  expect_warning(rr_ci_label(value = c(0.5, 0.7), conf.level = .95, digits = 1))
})

test_that("well-formatted R CI labels", {
  expect_equal(r_ci_label(value = c(0.5, 0.7), conf.level = .95),
               "\"95% CI [0.50, 0.70]\"")
  expect_equal(r_ci_label(value = c(0.5, 0.7), conf.level = .995),
               "\"99.5% CI [0.50, 0.70]\"")
  expect_equal(r_ci_label(value = c(0.5, 0.7), conf.level = .95, range.brackets = c("(", ")")),
               "\"95% CI (0.50, 0.70)\"")
  expect_equal(r_ci_label(value = c(-1-1e-14, 1+1e-14), conf.level = .95),
               "\"95% CI [-1.00, 1.00]\"")
  expect_warning(r_ci_label(value = c(0.7, 0.5), conf.level = .95))
  expect_equal(suppressWarnings(r_ci_label(value = c(-1, 1.1), conf.level = .95)),
               "\"95% CI [-1.00, NA]\"")
  expect_warning(r_ci_label(value = c(-1.1, 1), conf.level = .95))
  expect_equal(suppressWarnings(r_ci_label(value = c(-1.1, 1), conf.level = .95)),
               "\"95% CI [NA, 1.00]\"")
  expect_warning(r_ci_label(value = c(-1, 1.1), conf.level = .95))
  expect_equal(suppressWarnings(r_ci_label(value = c(-1.1, 1.1), conf.level = .95)),
               "\"95% CI [NA, NA]\"")
  expect_true(is.na(r_ci_label(value = NA, conf.level = .95)))
  expect_true(is.na(r_ci_label(value = c(1, 0/0), conf.level = .95)))

  expect_equal(r_ci_label(value = c(0.5, 0.7), conf.level = .95, output.type = "expression"),
               "\"95% CI [0.50, 0.70]\"")
  expect_equal(r_ci_label(value = c(0.5, 0.7), conf.level = .95, output.type = "latex"),
               "95% CI [0.50, 0.70]")
  expect_equal(r_ci_label(value = c(0.5, 0.7), conf.level = .95, output.type = "markdown"),
               "95% CI [0.50, 0.70]")
  expect_equal(r_ci_label(value = c(0.5, 0.7), conf.level = .95, output.type = "text"),
               "95% CI [0.50, 0.70]")
  expect_warning(r_ci_label(value = c(-0.5, 0.7), conf.level = .95, digits = 1))
})

