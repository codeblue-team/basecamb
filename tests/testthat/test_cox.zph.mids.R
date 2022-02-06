library(basecamb)

test_that("cox.zph.mids handles cases correctly", {

  # create data
  data <- data.frame(time = 101:200, status = rep(c(0,1), 50), pred = rep(c(1:9, NA), 10))
  imputed_data <- mice::mice(data, seed = 1)
  cox_mod <- Hmisc::fit.mult.impute(survival::Surv(time, status) ~ pred,
  fitter = rms::cph, xtrans = imputed_data)

  cox.zph.mids(cox_mod, imputed_data, verbose = FALSE)

  expect_equal(mean(cox.zph.mids(cox_mod, imputed_data)), 0.6588979)
  expect_message(cox.zph.mids(cox_mod, imputed_data), '0 out of 5 global p-values were smaller than 0.05')
  expect_message(cox.zph.mids(cox_mod, imputed_data, p_level = 0.5), '1 out of 5 global p-values were smaller than 0.5')

  expect_equal(names(cox.zph.mids(cox_mod, imputed_data, global_only = FALSE)), c('imputation', 'pred', 'GLOBAL'))
  expect_equal(dim(cox.zph.mids(cox_mod, imputed_data, global_only = FALSE)), c(5, 3))
  expect_message(cox.zph.mids(cox_mod, imputed_data, global_only = FALSE), '0 out of 5 global p-values were smaller than 0.05')
  expect_message(cox.zph.mids(cox_mod, imputed_data, global_only = FALSE), '0 out of 5 individual p-values were smaller than 0.05')

  expect_warning(cox.zph.mids(cox_mod, imputed_data, p_only = FALSE), 'P_only cannot be FALSE when global_only is TRUE.')

  expect_equal(names(cox.zph.mids(cox_mod, imputed_data, p_only = FALSE, global_only = FALSE)),
               c('imputation', 'pred_p', 'pred_chisq', 'pred_df', 'GLOBAL_p', 'GLOBAL_chisq', 'GLOBAL_df'))
  expect_equal(dim(cox.zph.mids(cox_mod, imputed_data, p_only = FALSE, global_only = FALSE)), c(5,7))
  expect_message(cox.zph.mids(cox_mod, imputed_data, p_only = FALSE, global_only = FALSE),
                 '0 out of 5 global p-values were smaller than 0.05')
  expect_message(cox.zph.mids(cox_mod, imputed_data, p_only = FALSE, global_only = FALSE),
                 '0 out of 5 individual p-values were smaller than 0.05')

  expect_equal(length(cox.zph.mids(cox_mod, imputed_data, return_raw = TRUE)), 5)
  expect_equal(class(cox.zph.mids(cox_mod, imputed_data, return_raw = TRUE)[[1]]), 'cox.zph')

  expect_error(expect_message(cox.zph.mids(cox_mod, imputed_data, verbose = FALSE)))
  expect_error(expect_message(cox.zph.mids(cox_mod, imputed_data, global_only = FALSE, verbose = FALSE)))
  expect_error(expect_message(cox.zph.mids(cox_mod, imputed_data, global_only = FALSE, p_only = FALSE, verbose = FALSE)))

})

