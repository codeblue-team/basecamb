library(basecamb)
library(testthat)

test_that("Simple Box-Cox transformation works", {
  data <- data.frame("value" = c(1:10), "dummy_var" = rep(1, 10))
  lambdas <- stratified_boxcox(data = data, value_col = "value", strat_cols = c("dummy_var"), return = "lambdas")
  expect_equal(lambdas[["dummy_var_1"]], 0.7)
  boxed <- stratified_boxcox(data = data, value_col = "value", strat_cols = c("dummy_var"), return = "values")
  expect_equal(round(boxed, digits = 2), c(0, 0.89, 1.65, 2.34, 2.98, 3.58, 4.15, 4.7, 5.22, 5.73))
})

test_that("Stratified Box-Cox transformation works", {
  data <- data.frame("value" = c(1:50, rep(c(1, 1, 2, 3, 5), 5), rep(100, 25)), "strat_var" = rep(c(1,2), each = 50), "strat_var2" = rep(c(1, 2, 1, 2), 25))
  lambdas <- stratified_boxcox(data = data, value_col = "value", strat_cols = c("strat_var", "strat_var2"), return = "lambdas")
  expect_equal(unlist(lambdas, use.names = FALSE), c(0.7, 0.7, 0, 0.1))
  boxed <- stratified_boxcox(data = data, value_col = "value", strat_cols = c("strat_var", "strat_var2"), return = "values")
  expect_equal(round(boxed[1], digits = 2), 0)
  expect_equal(round(boxed[10], digits = 2), 5.73)
  expect_equal(round(boxed[26], digits = 2), 12.55)
  expect_equal(round(boxed[36], digits = 2), 16.12)
  expect_equal(round(boxed[51], digits = 2), 0)
  expect_equal(round(boxed[53], digits = 2), 0.69)
  expect_equal(round(boxed[76], digits = 2), 5.85)
  expect_equal(round(boxed[86], digits = 2), 5.85)
})

test_that("Box-Cox round trip works", {
  data <- data.frame("value" = c(1:50, rnorm(50, 100, 10)), "strat_var" = rep(c(1,2), each = 50), "strat_var2" = rep(c(1, 2), 50))
  lambdas <- stratified_boxcox(data = data, value_col = "value", strat_cols = c("strat_var", "strat_var2"), return = "lambdas")
  data$value_boxed <- stratified_boxcox(data = data, value_col = "value", strat_cols = c("strat_var", "strat_var2"), return = "values")
  data$value_unboxed <- stratified_boxcox(data = data, value_col = "value_boxed", strat_cols = c("strat_var", "strat_var2"), inverse = TRUE, lambdas = lambdas)
  expect_true(all(round(data$value, digits = 4) == round(data$value_unboxed, digits = 4)))
})
