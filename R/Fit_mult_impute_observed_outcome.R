#' Fit a model on multiply imputed data using only observations with non-missing
#'   outcome(s)
#'
#' This function is a wrapper for fitting models with `Hmisc::fit.mult.impute()` on a
#'   multiply imputed dataset generated with `mice::mice()`. Cases with a
#'   missing outcome in the original dataset are removed from the mids object
#'   by using the "subset" argument in `Hmisc::fit.mult.impute()`.
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
#' @importFrom assertthat assert_that
#' @importFrom mice is.mids
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
  assertthat::assert_that(mice::is.mids(mids))
  assertthat::assert_that(class(formula) == "formula")

  # get the outcome from the formula
  y_var <- deconstruct_formula(formula = formula)$outcome

  # fit models
  if('fitargs' %in% names(formals(fit.mult.impute))) {
    mod <- Hmisc::fit.mult.impute(
      formula = formula, # model formula
      fitter = fitter, # the type of model fitted
      xtrans = mids, # the imputed dataset
      pr = FALSE,
      fitargs = list(x = TRUE, y = TRUE),
      subset = !is.na(y_var), # subset the data for only those observations that have an observed outcome
      ... # other arguments to fit.mult.impute()
    )
  }
  else {
    mod <- Hmisc::fit.mult.impute(
    formula = formula, # model formula
    fitter = fitter, # the type of model fitted
    xtrans = mids, # the imputed dataset
    pr = FALSE,
    x = TRUE,
    y = TRUE,
    subset = !is.na(y_var), # subset the data for only those observations that have an observed outcome
    ... # other arguments to fit.mult.impute()
  )
  }



  return(mod)
}


#' Remove missing cases from a mids object
#'
#' Deprecated, use \code{\link{apply_function_to_imputed_data}} instead.
#'
#'   Remove_missing_from_mids is used to filter a mids object for missing cases
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
#' @importFrom mice filter is.mids
#' @importFrom assertthat assert_that is.string
#'
#' @export
#'
#' @seealso \code{\link{apply_function_to_imputed_data}}
#'
#' @author Till D. Best
remove_missing_from_mids <- function(mids, var) {
  .Deprecated(new='apply_function_to_imputed_data', package='basecamb')

  # assert that inputed is as expected
  assertthat::assert_that(mice::is.mids(mids))
  assertthat::is.string(var)

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
