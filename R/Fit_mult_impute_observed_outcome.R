#' Fit a model on multiply imputed data using only non-missing outcomes
#'
#' This function fits a regression model using Hmisc::fit.mult.impute() on a
#'   multiply imputed dataset generated with mice::mice(). Cases with a
#'   missing outcome are removed from the dataset before model fitting.
#'
#' @param mids a mids object, i.e. the imputed dataset
#' @param formula a formula object that describes the model to be fit
#' @param fitter the fitter (i.e. the type of model) used by
#'   Hmisc::fit.mult.impute to fit the model.
#' @param ... any other arguments to Hmisc::fit.mult.impute()
#'
#' @return mod a fit.mult.impute object
#'
#' @examples
#' \dontrun{
#' fit_mult_impute_obs_outcome(mids = imputed_data, formula = "y ~ x1 + x2", fitter = rms::orm)
#' }
#'
#' @importFrom Hmisc fit.mult.impute
#' @importFrom stats formula
#'
#' @export
#'
#' @author Till D. Best
fit_mult_impute_obs_outcome <- function(mids,
                                        formula,
                                        fitter,
                                        ...) {

  # get the outcome from the formula
  y_var <- deconstruct_formula(formula = formula)$outcome
  # remove missing cases of the outcome from the variable
  mids_filtered <- remove_missing_from_mids(mids = mids, var = y_var)

  # fit models
  mod <- Hmisc::fit.mult.impute(
    formula = formula, # model formula
    fitter = fitter, # the type of model fitted
    xtrans = mids_filtered, # the imputed dataset
    pr = FALSE,
    x = TRUE,
    y = TRUE,
    ... # other arguments to fit.mult.impute()
  )

  return(mod)
}


#' Remove missing cases from a mids object
#'
#' remove_missing_from_mids is used to filter a mids object for missing cases
#'   in the original dataset in the variable var. This is useful for situations
#'   where you want to use as many observations as possible for imputation but
#'   only fit your model on a subset of these. Or, if you want to create one
#'   large imputed datset from which multiple analyses with multiple outcomes
#'   are derived.
#'
#' @param mids mids objects that is filtered.
#' @param var a string indicating the variable in the dataset for which missing
#'     values are removed.
#'
#' @return a mids object filtered for observed cases of var
#'
#' @examples
#' \dontrun{
#' remove_missing_from_mids(mids = imputed_data, var = "var_with_missing_value")
#' }
#'
#' @importFrom mice filter
#'
#' @export
#'
#' @author Till D. Best
remove_missing_from_mids <- function(mids, var) {
  # assert that inputed is as expected
  assertive.types::assert_is_all_of(x = mids, classes = "mids")
  assertive.types::assert_is_character(var)

  # calculate a logical vector indicating cases with missing var
    miss_matrix <- is.na(mids$data[var])

    any_miss <- miss_matrix[, 1]

    for (i in seq(ncol(miss_matrix))) {
      any_miss <- any_miss | miss_matrix[, i]
      }

  # filter mids object for cases where var is not missing
  mids_filtered <- mice::filter(mids, !any_miss)
  # message the number of cases that have a missing outcome
  message(sprintf("%s missing case(s) in [%s] found and removed.\n",
                  sum(any_miss),
                  paste(var, collapse = ", ")))

  return(mids_filtered)
}
