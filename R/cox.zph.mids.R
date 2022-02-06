# Function to test for violation of proportional odds for a cox-ph model on multiple imputations using cox.zph


#' Test cox proportional odds assumption on models using multiple imputation.
#'
#' Constructs a model and conducts a cox.zph test for each imputation of the data set.
#'
#' @param model cox proportional model to be evaluated
#' @param imputations mids object containing imputations
#' @param p_level value below which violation of proportional odds assumption is assumed. Defaults to .05
#' @param global_only return global p-value only. Implies p_only to be TRUE
#' @param return_raw return cox.zph objects in a list. If TRUE, function will not return anything else
#' @param p_only returns p-values of test only. If FALSE returns Chi² and degrees of freedom as well
#' @param verbose Set to FALSE to deactivate messages
#'
#' @return depending on specified options, this function can return
#'   - default: A vector of global p-values
#'   - global_only = FALSE: a data.frame with p-values for all variables plus the global
#'   - return_raw = TRUE: list of cox.zph objects
#'
#' @examples
#' data <- data.frame(time = 101:200, status = rep(c(0,1), 50), pred = rep(c(1:9, NA), 10))
#' imputed_data <- mice::mice(data)
#' cox_mod <- Hmisc::fit.mult.impute(survival::Surv(time, status) ~ pred,
#' fitter = rms::cph, xtrans = imputed_data)
#' cox.zph.mids(cox_mod, imputed_data)
#'
#' @importFrom dplyr setdiff
#' @importFrom survival cox.zph coxph
#' @importFrom stats formula
#' @importFrom mice complete
#'
#' @export
#'
#' @author J. Peter Marquardt
cox.zph.mids <- function(model, imputations, p_level = 0.05, global_only = TRUE, return_raw = FALSE, p_only = TRUE, verbose = TRUE) {

  # assertions
  stopifnot('coxph' %in% class(model))
  stopifnot('mids' %in% class(imputations))
  stopifnot(0 < p_level && p_level < 1)

  # initialise list of cox.zph evaluations
  cox.zph_list <- list()

  # loop over imputations
  for (i in 1:imputations[['m']]) {

    # build new model
    data <- mice::complete(imputations, action = i)
    formula_parts <- deconstruct_formula(stats::formula(model))
    frml <- build_model_formula(formula_parts$outcome, formula_parts$predictors, formula_parts$censor_event, env = environment())
    cox_mod <- survival::coxph(formula = frml, data = data)
    zph <- survival::cox.zph(cox_mod)
    # append new cox.zph to list
    cox.zph_list <- append(cox.zph_list, list(zph))
  }

  # if return_raw is enabled we're already done
  if (return_raw) return(cox.zph_list)

  # extract variable p-values to data.frame
  zph_df <- data.frame()
  for (i in 1:length(cox.zph_list)) {

    zph <- cox.zph_list[[i]]
    zph_df[i, 'imputation'] <- i
    for (var in rownames(zph$table)) {

      if (p_only) {
        zph_df[i, var] <- zph$table[var, 'p']
      }
      else {
        zph_df[i, paste0(var, '_p')] <- zph$table[var, 'p']
        zph_df[i, paste0(var, '_chisq')] <- zph$table[var, 'chisq']
        zph_df[i, paste0(var, '_df')] <- zph$table[var, 'df']
      }

    }
  }

  # returns and messages
  if (global_only) {
    if (!p_only) {
      warning('P_only cannot be FALSE when global_only is TRUE.')
      return(zph_df$GLOBAL_p)
    }
    else {
      if (verbose) message(paste(sum(zph_df$GLOBAL < p_level) , 'out of', length(zph_df$GLOBAL), 'global p-values were smaller than', p_level))
      return(zph_df$GLOBAL)
    }
  }

  if (p_only) {
    if (verbose) message(paste(sum(zph_df$GLOBAL < p_level) , 'out of', length(zph_df$GLOBAL), 'global p-values were smaller than', p_level))
    vars_only <- data.frame(zph_df[, dplyr::setdiff(names(zph_df), c('imputation', 'GLOBAL'))])
    if (verbose) message(paste(sum(vars_only < p_level) , 'out of', dim(vars_only)[1] * dim(vars_only)[2], 'individual p-values were smaller than', p_level))
    return(zph_df)
  }

  # p_only & global_only = FALSE
  if (verbose) message(paste(sum(zph_df$GLOBAL_p < p_level) , 'out of', length(zph_df$GLOBAL_p), 'global p-values were smaller than', p_level))
  vars_only <- data.frame(zph_df[, grepl('*_p', names(zph_df)) & names(zph_df) != 'GLOBAL_p'])
  if (verbose) message(paste(sum(vars_only < p_level) , 'out of', dim(vars_only)[1] * dim(vars_only)[2], 'individual p-values were smaller than', p_level))
  return(zph_df)

}
