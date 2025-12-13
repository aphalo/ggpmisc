context("label utilities")

options(OutDec = ".")

test_that("decimal.mark is obeyed", {
  expect_equal(sprintf_dm("%.3f", 100.123, decimal.mark = NA),
               sprintf("%.3f", 100.123))
  expect_equal(sprintf_dm("%.3f", 100.123, decimal.mark = NULL),
               sprintf("%.3f", 100.123))
  expect_equal(sprintf_dm("%.3f", 100.123, decimal.mark = "."),
               "100.123")
  expect_equal(sprintf_dm("%.3f", 100.123, decimal.mark = ","),
               "100,123")
  old.OutDec <- options(OutDec = ",")
  expect_equal(sprintf_dm("%.3f", 100.123),
               "100,123")
  options(OutDec = ".")
  expect_equal(sprintf_dm("%.3f", 100.123),
               "100.123")
  options(old.OutDec)
})

test_that("digits is obeyed", {
  expect_equal(value2char(100.123, format = "f", digits = 3),
               "\"100.123\"")
  expect_equal(value2char(100.123, format = "f", digits = 0),
               "\"100.\"")
  expect_equal(value2char(100.123, format = "f", digits = Inf),
               "\"100.12\"")
  expect_equal(value2char(100.123, format = "e", digits = 3),
               "\"1.001\" %*% 10^{\"+02\"}")
  expect_equal(value2char(100.123, format = "e", digits = 0),
               "\"1.\" %*% 10^{\"+02\"}")
  expect_equal(value2char(100.123, format = "e", digits = Inf),
               "\"1.00\" %*% 10^{\"+02\"}")
  expect_equal(value2char(100.123, format = "g", digits = 3),
               "\"100.\"")
  expect_equal(value2char(100.123, format = "g", digits = 6),
               "\"100.123\"")
  expect_equal(value2char(100.123, format = "g", digits = 0),
               "\"1.\" %*% 10^{\"+02\"}")
  expect_equal(value2char(100.123, format = "g", digits = Inf),
               "\"1.0\" %*% 10^{\"+02\"}")

  expect_equal(value2char(100.123, format = "f", digits = 3, output.type = "markdown"),
               "100.123")
  expect_equal(value2char(100.123, format = "f", digits = 0, output.type = "markdown"),
               "100.")
  expect_equal(value2char(100.123, format = "f", digits = Inf, output.type = "markdown"),
               "100.12")
  expect_equal(value2char(100.123, format = "e", digits = 3, output.type = "markdown"),
               "1.001e+02")
  expect_equal(value2char(100.123, format = "g", digits = 3, output.type = "markdown"),
               "100.")

  expect_equal(value2char(100.123, format = "f", digits = 3, output.type = "latex"),
               "100.123")
  expect_equal(value2char(100.123, format = "f", digits = 0, output.type = "latex"),
               "100.")
  expect_equal(value2char(100.123, format = "f", digits = Inf, output.type = "latex"),
               "100.12")
  expect_equal(value2char(100.123, format = "e", digits = 3, output.type = "latex"),
               "1.001 \\times 10^{+02}")
  expect_equal(value2char(100.123, format = "g", digits = 3, output.type = "latex"),
               "100.")
})


test_that("well-formatted simple labels", {
  expect_true(is.na(plain_label(value = NA, value.name = "n", output.type = "expression")))
  expect_true(is.na(plain_label(value = NULL, value.name = "n", output.type = "expression")))
  expect_true(is.na(plain_label(value = 0/0, value.name = "n", output.type = "expression")))
  expect_equal(plain_label(value = 123L, value.name = "n", output.type = "expression"),
               "plain(n)~`=`~123")
  expect_equal(plain_label(value = 123, value.name = "n", output.type = "expression"),
               "plain(n)~`=`~\"123.\"")
  expect_equal(plain_label(value = 123, value.name = "n", output.type = "markdown"),
               "n = 123.")
  expect_equal(plain_label(value = 123, value.name = "n", output.type = "latex"),
               "\\mathrm{n} = 123.")
  expect_equal(plain_label(value = 123, value.name = "n", output.type = "latex.eqn"),
               "$ \\mathrm{n} = 123. $")
  expect_equal(plain_label(value = 123, value.name = "n", output.type = "latex.deqn"),
               "$$ \\mathrm{n} = 123. $$")
  expect_equal(plain_label(value = 123, value.name = "n", output.type = "text"),
               "n = 123.")

  expect_true(is.na(italic_label(value = NA, value.name = "n", output.type = "expression")))
  expect_true(is.na(italic_label(value = NULL, value.name = "n", output.type = "expression")))
  expect_true(is.na(italic_label(value = 0/0, value.name = "n", output.type = "expression")))
  expect_equal(italic_label(value = 123L, value.name = "n", output.type = "expression"),
               "italic(n)~`=`~123")
  expect_equal(italic_label(value = 123, value.name = "n", output.type = "expression"),
               "italic(n)~`=`~\"123.\"")
  expect_equal(italic_label(value = 123, value.name = "n", output.type = "markdown"),
               "_n_ = 123.")
  expect_equal(italic_label(value = 123, value.name = "n", output.type = "latex"),
               "n = 123.")
  expect_equal(italic_label(value = 123, value.name = "n", output.type = "latex.eqn"),
               "$ n = 123. $")
  expect_equal(italic_label(value = 123, value.name = "n", output.type = "latex.deqn"),
               "$$ n = 123. $$")
  expect_equal(italic_label(value = 123, value.name = "n", output.type = "text"),
               "n = 123.")

  expect_true(is.na(bold_label(value = NA, value.name = "n", output.type = "expression")))
  expect_true(is.na(bold_label(value = NULL, value.name = "n", output.type = "expression")))
  expect_true(is.na(bold_label(value = 0/0, value.name = "n", output.type = "expression")))
  expect_equal(bold_label(value = 123L, value.name = "n", output.type = "expression"),
               "bold(n)~`=`~123")
  expect_equal(bold_label(value = 123, value.name = "n", output.type = "expression"),
               "bold(n)~`=`~\"123.\"")
  expect_equal(bold_label(value = 123, value.name = "n", output.type = "markdown"),
               "**n** = 123.")
  expect_equal(bold_label(value = 123, value.name = "n", output.type = "latex"),
               "\\mathbf{n} = 123.")
  expect_equal(bold_label(value = 123, value.name = "n", output.type = "text"),
               "n = 123.")
})

test_that("well-formatted P-value labels", {
  expect_equal(p_value_label(value = 0.5),
               "italic(P)~`=`~\"0.5000\"")
  expect_equal(p_value_label(value = -1e-14),
               "italic(P)~`<`~\"0.0001\"")
  expect_equal(p_value_label(value = 1 + 1e-14),
               "italic(P)~`=`~\"1.0000\"")
  expect_equal(p_value_label(value = 0.5, subscript = "abc"),
               "italic(P)[abc]~`=`~\"0.5000\"")
  expect_equal(p_value_label(value = 0.5, subscript = NA),
               "italic(P)~`=`~\"0.5000\"")
  expect_equal(p_value_label(value = 0.5, subscript = ""),
               "italic(P)~`=`~\"0.5000\"")
  expect_equal(p_value_label(value = 0.5, output.type = "expression"),
               "italic(P)~`=`~\"0.5000\"")
  expect_equal(p_value_label(value = 0.5, digits = 2, output.type = "expression"),
               "italic(P)~`=`~\"0.50\"")
  expect_equal(p_value_label(value = 0.5, digits = 5, output.type = "expression"),
               "italic(P)~`=`~\"5.00000\" %*% 10^{\"−01\"}")
  expect_equal(p_value_label(value = 0.5, digits = Inf, output.type = "expression"),
               "italic(P)~`=`~\"0.500\"")
  expect_equal(p_value_label(value = 0.5, output.type = "expression", small.p = TRUE),
               "italic(p)~`=`~\"0.5000\"")

  expect_equal(p_value_label(value = 0.5, output.type = "latex"),
               "P = 0.5000")
  expect_equal(p_value_label(value = 0.5, output.type = "latex.eqn"),
               "$ P = 0.5000 $")
  expect_equal(p_value_label(value = 0.5, output.type = "latex.deqn"),
               "$$ P = 0.5000 $$")
  expect_equal(p_value_label(value = 0.5, digits = 2, output.type = "latex"),
               "P = 0.50")
  expect_equal(p_value_label(value = 0.5, digits = 5, output.type = "latex"),
               "P = 5.00000 \\times 10^{-01}")
  expect_equal(p_value_label(value = 0.5, digits = Inf, output.type = "latex"),
               "P = 0.500")
  expect_equal(p_value_label(value = 0.5, output.type = "latex", small.p = TRUE),
               "p = 0.5000")

  expect_equal(p_value_label(value = 0.5, output.type = "markdown"),
               "_P_ = 0.5000")
  expect_equal(p_value_label(value = 0.5, digits = 2, output.type = "markdown"),
               "_P_ = 0.50")
  expect_equal(p_value_label(value = 0.5, digits = 5, output.type = "markdown"),
               "_P_ = 5.00000e-01")
  expect_equal(p_value_label(value = 0.5, digits = Inf, output.type = "markdown"),
               "_P_ = 0.500")
  expect_equal(p_value_label(value = 0.5, output.type = "markdown", small.p = TRUE),
               "_p_ = 0.5000")

  expect_equal(p_value_label(value = 0.5, output.type = "text"),
               "P = 0.5000")

  expect_warning(p_value_label(value = 0.5, digits = 1))
  expect_warning(p_value_label(value = 1.1))
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
               "F = 100.0")
  expect_equal(f_value_label(value = 100, output.type = "latex.eqn"),
               "$ F = 100.0 $")
  expect_equal(f_value_label(value = 100, output.type = "latex.deqn"),
               "$$ F = 100.0 $$")
  expect_equal(f_value_label(value = 100, df1 = 1, df2 = 25, output.type = "latex"),
               "F_{1,25} = 100.0")
  expect_equal(f_value_label(value = 100, df1 = 1, df2 = 25, output.type = "latex.eqn"),
               "$ F_{1,25} = 100.0 $")
  expect_equal(f_value_label(value = 100, df1 = 1, df2 = 25, output.type = "latex.deqn"),
               "$$ F_{1,25} = 100.0 $$")

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
               "t = 12.00")
  expect_equal(t_value_label(value = 12, df = 5, output.type = "latex"),
               "t_{5} = 12.00")
  expect_equal(t_value_label(value = 12, output.type = "latex.eqn"),
               "$ t = 12.00 $")
  expect_equal(t_value_label(value = 12, output.type = "latex.deqn"),
               "$$ t = 12.00 $$")
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
               "\\sigma = 12.00")
  expect_equal(sd_value_label(value = 12, output.type = "latex.eqn"),
               "$ \\sigma = 12.00 $")
  expect_equal(sd_value_label(value = 12, output.type = "latex.deqn"),
               "$$ \\sigma = 12.00 $$")
  expect_equal(sd_value_label(value = 12, output.type = "markdown"),
               "_&sigma;_ = 12.00")
  expect_equal(sd_value_label(value = 12, output.type = "text"),
               "s.d. = 12.00")
})

test_that("well-formatted se labels", {
  expect_equal(se_value_label(value = 12),
               "italic(s.e.)~`=`~\"12.00\"")
  expect_equal(se_value_label(value = 12, output.type = "latex"),
               "s.e. = 12.00")
  expect_equal(se_value_label(value = 12, output.type = "markdown"),
               "_s.e._ = 12.00")
  expect_equal(se_value_label(value = 12, output.type = "text"),
               "s.e. = 12.00")
})

test_that("well-formatted var labels", {
  expect_equal(var_value_label(value = 12),
               "italic(sigma^2)~`=`~\"12.00\"")
  expect_equal(var_value_label(value = 12, output.type = "latex"),
               "\\sigma^2 = 12.00")
  expect_equal(var_value_label(value = 12, output.type = "markdown"),
               "_&sigma;<sup>2</sup>_ = 12.00")
  expect_equal(var_value_label(value = 12, output.type = "text"),
               "s^2 = 12.00")
})

test_that("well-formatted mean labels", {
  expect_equal(mean_value_label(value = 12),
               "italic(bar(x))~`=`~\"12.00\"")
  expect_equal(mean_value_label(value = 12, output.type = "latex"),
               "\\bar{x} = 12.00")
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
  expect_equal(r_label(value = 0.5, output.type = "latex.eqn"),
               "$ R = 0.500 $")
  expect_equal(r_label(value = 0.5, output.type = "latex.deqn"),
               "$$ R = 0.500 $$")
  expect_equal(r_label(value = -1e-14, output.type = "latex"),
               "|R| < 0.001")
  expect_equal(r_label(value = 0.5, method = "kendall", output.type = "latex"),
               "\\tau = 0.500")
  expect_equal(r_label(value = 0.5, method = "spearman", output.type = "latex"),
               "\\rho = 0.500")
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
  expect_equal(rr_label(value = 0.5, output.type = "latex.eqn"),
               "$ R^2 = 0.500 $")
  expect_equal(rr_label(value = 0.5, output.type = "latex.deqn"),
               "$$ R^2 = 0.500 $$")
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
  expect_equal(adj_rr_label(value = 0.5, output.type = "latex.eqn"),
               "$ R_{adj}^2 = 0.500 $")
  expect_equal(adj_rr_label(value = 0.5, output.type = "latex.deqn"),
               "$$ R_{adj}^2 = 0.500 $$")
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
               "95\\% \\mathrm{CI} [0.50, 0.70]")
  expect_equal(rr_ci_label(value = c(0.5, 0.7), conf.level = .95, output.type = "latex.eqn"),
               "$ 95\\% \\mathrm{CI} [0.50, 0.70] $")
  expect_equal(rr_ci_label(value = c(0.5, 0.7), conf.level = .95, output.type = "latex.deqn"),
               "$$ 95\\% \\mathrm{CI} [0.50, 0.70] $$")
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
               "95\\% \\mathrm{CI} [0.50, 0.70]")
  expect_equal(r_ci_label(value = c(0.5, 0.7), conf.level = .95, output.type = "latex.eqn"),
               "$ 95\\% \\mathrm{CI} [0.50, 0.70] $")
  expect_equal(r_ci_label(value = c(0.5, 0.7), conf.level = .95, output.type = "latex.deqn"),
               "$$ 95\\% \\mathrm{CI} [0.50, 0.70] $$")
  expect_equal(r_ci_label(value = c(0.5, 0.7), conf.level = .95, output.type = "markdown"),
               "95% CI [0.50, 0.70]")
  expect_equal(r_ci_label(value = c(0.5, 0.7), conf.level = .95, output.type = "text"),
               "95% CI [0.50, 0.70]")
  expect_warning(r_ci_label(value = c(-0.5, 0.7), conf.level = .95, digits = 1))
})

test_that("check_output_type validates", {
  expect_equal(check_output_type(NULL), "expression")
  expect_equal(check_output_type("text"), "text")
  expect_equal(check_output_type("latex"), "latex")
  expect_equal(check_output_type("LaTex"), "latex")
  expect_equal(check_output_type("latex.eqn"), "latex.eqn")
  expect_equal(check_output_type("latex.deqn"), "latex.deqn")
  expect_equal(check_output_type("tex"), "latex")
  expect_equal(check_output_type("TeX"), "latex")
  expect_equal(check_output_type("tikz"), "latex")
  expect_equal(check_output_type("markdown"), "markdown")
  expect_equal(check_output_type("Markdown"), "markdown")
  expect_error(check_output_type("rmarkdown"))
  expect_error(check_output_type("bad"))
})

test_that("check_output_type replaces NULL", {
  expect_equal(check_output_type(NULL), "expression")
  expect_equal(check_output_type(NULL, geom = "text"), "expression")
  expect_equal(check_output_type(NULL, geom = "anything else"), "expression")
  expect_equal(check_output_type(NULL, geom = "marquee"), "markdown")
  expect_equal(check_output_type(NULL, geom = "richtext"), "markdown")
  expect_equal(check_output_type(NULL, geom = "textbox"), "markdown")
  expect_equal(check_output_type(NULL, "latex"), "latex.eqn")
})
