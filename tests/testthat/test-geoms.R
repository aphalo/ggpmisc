context("geoms")

library(ggplot2)
library(tibble)

# test_that("data.frame", {
#   my.df <- data.frame(x = 1:10, y = 1:10, tb = letters[1:10])
#   expect_warning(ggplot(my.df, aes(x, y, label = tb)) +
#                    geom_table())
#   vdiffr::expect_doppelganger("geom_table_text_label",
#                               suppressWarnings(
#                                 ggplot(my.df, aes(x, y, label = tb)) +
#                                   geom_table()
#                               )
#   )
# })

test_that("multiple_rows_tb", {
  tb <- tibble(Z1 = LETTERS[2:4], z1 = letters[4:2])
  tbb <- tibble(Z2 = LETTERS[2:4], z2 = letters[4:2])
  tbbb <- tibble(Z3 = LETTERS[2:4], z3 = letters[4:2])
  my.tb <- tibble(x = 2:4, y = 3:5, tb = list(t1 = tb, t2 = tbb, t3 = tbbb))
  vdiffr::expect_doppelganger("geom_table_multi_row",
                              ggplot(my.tb, aes(x, y, label = tb)) +
                                geom_table() +
                                lims(x = c(0, 6), y = c(0, 6)))
})

test_that("numbers_tb", {
  my_data.tb <- tibble(x = -5:5, y = -5:5)
  tb <- tibble(a = 2:4, b = 4:2)
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  vdiffr::expect_doppelganger("geom_table_num1",
                              ggplot() +
                                geom_table(data = my.tb, mapping = aes(x, y, label = tb)))
  vdiffr::expect_doppelganger("geom_table_num2",
                              ggplot(data = my.tb, mapping = aes(x, y, label = tb)) +
                                geom_table())
  vdiffr::expect_doppelganger("geom_table_num3",
                              ggplot() +
                                geom_table(data = my.tb, mapping = aes(x, y, label = tb),
                                           vjust = 1))
  vdiffr::expect_doppelganger("geom_table_num4",
                              ggplot(data = my.tb, mapping = aes(x, y, label = tb)) +
                                geom_table(vjust = 1, hjust = 0))
  vdiffr::expect_doppelganger("geom_table_num5",
                              ggplot(data = my.tb, mapping = aes(x, y, label = tb)) +
                                geom_table(vjust = 0, hjust = 1))
  vdiffr::expect_doppelganger("geom_table_num6",
                              ggplot(my_data.tb, aes(x, y)) +
                                geom_point() +
                                geom_table(data = my.tb, mapping = aes(x, y, label = tb)))
})

test_that("letters_tb", {
  tb <- tibble(a = LETTERS[2:4], b = letters[4:2])
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  vdiffr::expect_doppelganger("geom_table_letters",
                              ggplot(my.tb, aes(x, y, label = tb)) +
                                geom_table())
})

test_that("parsed_tb", {
  tb <- tibble("alpha" = c("x[2]~\"=\"~a^2", "sqrt(y)"), "beta" = c("x[2]~\"=\"~b^2", "sqrt(1/y)"))
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  vdiffr::expect_doppelganger("geom_table_parsed_all",
                              ggplot(my.tb, aes(x, y, label = tb)) +
                                geom_table(parse = TRUE))

  tb <- tibble("alpha" = c("x[2]~\"=\"~a^2", "text"), "beta" = c("x[2]~\"=\"~b^2", "1200"))
  my.tb <- tibble(x = 0, y = 0, tb = list(tb))
  vdiffr::expect_doppelganger("geom_table_parsed_partial",
                              ggplot(my.tb, aes(x, y, label = tb)) +
                                geom_table(parse = TRUE))
})

