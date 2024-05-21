context("label utilities")

test_that("well-formatted simple labels", {
  expect_equal(plain_label(value = 123, value.name = "n", output.type = "expression"),
               "plain(n)~`=`~\"123.\"")
  expect_equal(plain_label(value = 123, value.name = "n", output.type = "markdown"),
               "n = 123.")
  expect_equal(plain_label(value = 123, value.name = "n", output.type = "latex"),
               "\\mathrm{n} = 123.")
  expect_equal(plain_label(value = 123, value.name = "n", output.type = "tex"),
               "\\mathrm{n} = 123.")
  expect_equal(plain_label(value = 123, value.name = "n", output.type = "tikz"),
               "\\mathrm{n} = 123.")
  expect_equal(italic_label(value = 123, value.name = "n", output.type = "expression"),
               "italic(n)~`=`~\"123.\"")
  expect_equal(italic_label(value = 123, value.name = "n", output.type = "markdown"),
               "_n_ = 123.")
  expect_equal(italic_label(value = 123, value.name = "n", output.type = "latex"),
               "\\mathit{n} = 123.")
  expect_equal(italic_label(value = 123, value.name = "n", output.type = "tex"),
               "\\mathit{n} = 123.")
  expect_equal(italic_label(value = 123, value.name = "n", output.type = "tikz"),
               "\\mathit{n} = 123.")
  expect_equal(bold_label(value = 123, value.name = "n", output.type = "expression"),
               "bold(n)~`=`~\"123.\"")
  expect_equal(bold_label(value = 123, value.name = "n", output.type = "markdown"),
               "**n** = 123.")
  expect_equal(bold_label(value = 123, value.name = "n", output.type = "latex"),
               "\\mathbf{n} = 123.")
  expect_equal(bold_label(value = 123, value.name = "n", output.type = "tex"),
               "\\mathbf{n} = 123.")
  expect_equal(bold_label(value = 123, value.name = "n", output.type = "tikz"),
               "\\mathbf{n} = 123.")
})

test_that("well-formatted P-value labels", {
  expect_equal(p_value_label(value = 0.5),
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
  expect_warning(p_value_label(value = 0.5, output.type = "expression", digits = 1))
  expect_warning(p_value_label(value = 1.1))
})

test_that("well-formatted F-value labels", {
  expect_equal(f_value_label(value = 1e2),
               "italic(F)~`=`~\"100.0\"")
  expect_equal(f_value_label(value = 100, output.type = "expression"),
               "italic(F)~`=`~\"100.0\"")
  expect_equal(f_value_label(value = 100, digits = 2, output.type = "expression"),
               "italic(F)~`=`~\"1.0\" %*% 10^{\"+02\"}")
  expect_equal(f_value_label(value = 100, digits = 5, output.type = "expression"),
               "italic(F)~`=`~\"100.00\"")
  expect_equal(f_value_label(value = 100, digits = Inf, output.type = "expression"),
               "italic(F)~`=`~\"1.0\" %*% 10^{\"+02\"}")
})

test_that("well-formatted t-value labels", {
  expect_equal(t_value_label(value = 12),
               "italic(t)~`=`~\"12.00\"")
  expect_equal(t_value_label(value = 12, output.type = "expression"),
               "italic(t)~`=`~\"12.00\"")
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
})

test_that("well-formatted sd labels", {
  expect_equal(sd_value_label(value = 12),
               "italic(sigma)~`=`~\"12.00\"")
})

test_that("well-formatted var labels", {
  expect_equal(var_value_label(value = 12),
               "italic(sigma^2)~`=`~\"12.00\"")
})

test_that("well-formatted mean labels", {
  expect_equal(mean_value_label(value = 12),
               "italic(bar(x))~`=`~\"12.00\"")
})

test_that("well-formatted R labels", {
  expect_equal(r_label(value = 0.5),
               "italic(R)~`=`~\"0.500\"")
  expect_equal(r_label(value = 0.5, method = "pearson"),
               "italic(R)~`=`~\"0.500\"")
  expect_equal(r_label(value = 0.5, method = "kendall"),
               "italic(tau)~`=`~\"0.500\"")
  expect_equal(r_label(value = 0.5, method = "spearman"),
               "italic(rho)~`=`~\"0.500\"")
  expect_equal(r_label(value = -0.5),
               "italic(R)~`=`~\"-0.500\"")
  expect_equal(r_label(value = 0.5, output.type = "expression"),
               "italic(R)~`=`~\"0.500\"")
  expect_equal(r_label(value = 0.5, digits = 2, output.type = "expression"),
               "italic(R)~`=`~\"0.50\"")
  expect_equal(r_label(value = 0.5, digits = 5, output.type = "expression"),
               "italic(R)~`=`~\"0.50000\"")
  expect_equal(r_label(value = 0.5, digits = Inf, output.type = "expression"),
               "italic(R)~`=`~\"0.50\"")
  expect_equal(r_label(value = 0.5, output.type = "expression", small.r = TRUE),
               "italic(r)~`=`~\"0.500\"")
  expect_warning(r_label(value = 0.5, output.type = "expression", digits = 1))
  expect_warning(r_label(value = 1.1))
  expect_true(is.na(suppressWarnings(r_label(value = 1.1))))
})

test_that("well-formatted R2 labels", {
  expect_equal(rr_label(value = 0.5),
               "italic(R)^2~`=`~\"0.500\"")
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
  expect_warning(rr_label(value = 0.5, output.type = "expression", digits = 1))
  expect_warning(rr_label(value = 1.1))
  expect_warning(rr_label(value = -0.1))
  expect_true(is.na(suppressWarnings(rr_label(value = 1.1))))
  expect_true(is.na(suppressWarnings(rr_label(value = -0.1))))
})

test_that("well-formatted adjusted R2 labels", {
  expect_equal(adj_rr_label(value = 0.5),
               "italic(R)[adj]^2~`=`~\"0.500\"")
  expect_equal(adj_rr_label(value = 0.5, output.type = "expression"),
               "italic(R)[adj]^2~`=`~\"0.500\"")
  expect_warning(adj_rr_label(value = 0.5, output.type = "expression", digits = 1))
  expect_warning(adj_rr_label(value = 1.1))
  expect_no_warning(adj_rr_label(value = -0.1))
  expect_true(is.na(suppressWarnings(adj_rr_label(value = 1.1))))
  expect_false(is.na(suppressWarnings(adj_rr_label(value = -0.1))))
})

test_that("well-formatted R2 CI labels", {
  expect_equal(rr_ci_label(value = c(0.5, 0.7), conf.level = .95),
               "\"95% CI [0.50, 0.70]\"")
  expect_equal(rr_ci_label(value = c(0.5, 0.7), conf.level = .95, range.brackets = c("(", ")")),
               "\"95% CI (0.50, 0.70)\"")
})

test_that("well-formatted R CI labels", {
  expect_equal(r_ci_label(value = c(0.5, 0.7), conf.level = .95),
               "\"95% CI [0.50, 0.70]\"")
  expect_equal(r_ci_label(value = c(0.5, 0.7), conf.level = .95, range.brackets = c("(", ")")),
               "\"95% CI (0.50, 0.70)\"")
})

