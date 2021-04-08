#' Fit a model on multiply imputed data using only observatoin with non-missing
#'   outcome(s)
#'
#' This function fits a regression model using `Hmisc::fit.mult.impute()` on a
#'   multiply imputed dataset generated with `mice::mice()`. Cases with a
#'   missing outcome in the original dataset are removed from the mids object
#'   before model fitting.
#'
#' @param mids a mids object, i.e. the imputed dataset.
#' @param formula a formula that describes the model to be fit. The outcome (y
#'   variable) in the formula will be used to remove missing cases.
#' @param fitter a modeling function (not in quotes) that is compatible with
#'   `Hmisc::fit.mult.impute()`.
#' @param ... additional arguments to `Hmisc::fit.mult.impute()`.
#'
#' @return mod a fit.mult.impute object.
#'
#' @examples
#' # create an imputed dataset
#' imputed_data <- mice::mice(airquality)
#'
#' fit_mult_impute_obs_outcome(mids = imputed_data, formula = Ozone ~ Solar.R + Wind, fitter = glm)
#'
#' @importFrom assertive.types assert_is_all_of
#' @importFrom assertive.types assert_is_formula
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
  assertive.types::assert_is_all_of(x = mids, classes = "mids")
  assertive.types::assert_is_formula(x = formula)

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
#' Remove_missing_from_mids is used to filter a mids object for missing cases
#'   in the original dataset in the variable var. This is useful for situations
#'   where you want to use as many observations as possible for imputation but
#'   only fit your model on a subset of these. Or, if you want to create one
#'   large imputed datset from which multiple analyses with multiple outcomes
#'   are derived.
#'
#' @param mids mids objects that is filtered.
#' @param var a string or vector of strings specifying the variable(s). All
#'   cases (i.e. rows) for which there are missing values are removed.
#'
#' @return a mids object filtered for observed cases of var.
#'
#' @importFrom mice filter
#' @importFrom assertive.types assert_is_all_of
#' @importFrom assertive.types assert_is_character
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
