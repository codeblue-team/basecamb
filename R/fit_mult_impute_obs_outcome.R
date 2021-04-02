#' Fit a proportional odds model on multiply imputed data
#'
#' This function fits an proportional odds logistic regression (a type of
#'   ordinal logistic regression) on a multiply imputed dataset that was
#'   generated with mice::mice(). Cases with a missing outcome before imputation
#'   are removed from the fitting process.
#'
#' @param imp mids object containing multiple imputations of data generated with
#'   mice::mice()
#' @param outcome the outcome column as a string
#' @param predictors a vector of strings containing the predictors/covariables
#' @param fitter the fitter used for fitting the model. For details see
#' @importFrom Hmisc fit.mult.impute
#'
#' @return est the pooled estimate
#'
#' @author Till D. Best
fit_mult_impute_obs_outcome <- function(imp,
                                        outcome,
                                        predictors,
                                        fitter = orm) {
  # calculate the cases with a missing outcome
  n_miss_outcome <- is.na(imp$data[[outcome]])
  # filter for complete outcomes
  imp_filtered <- mice::filter(imp, !n_miss_outcome)
  # print the number of cases that have a missing outcome
  message(sprintf("%s cases had a missing outcome and were removed.\n",
                  sum(n_miss_outcome)))

  # generate the model formula
  formula <- as.formula(paste(outcome,
    " ~ ",
    paste(predictors, collapse = " + "),
    sep = ""
  ))

  # fit models
  mod <- Hmisc::fit.mult.impute(
    formula = formula, # model formula
    fitter = fitter, # the type of model fitted
    xtrans = imp_filtered, # the imputed dataset
    pr = FALSE, x = TRUE, y = TRUE
  )

  return(mod)
}
