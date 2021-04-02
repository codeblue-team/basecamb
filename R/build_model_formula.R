#' Build formula for statistical models
#' 
#' Build formula used in statistical models from vectors of strings
#' 
#' @param outcome character denoting the column with the outcome
#' @param predictors vector of characters denoting the columns with the predictors
#' @param censor_event character denoting the column with the censoring event, for use in Survival-type models
#' 
#' @return formula for use in statistical models
#' 
#' @example build_model_formula('outcome', c('pred_1', 'pred_2'))
#' @example build_model_formula('outcome', c('pred_1', 'pred_2'), censor_event='cens_event')
#' 
#' @importFrom assertive.types assert_is_character
#' 
#' @export
#' 
#' @author J. Peter Marquardt
build_model_formula <- function(outcome, predictors, censor_event=NULL) {
  
  assertive.types::assert_is_character(outcome)
  assertive.types::assert_is_character(predictors)
  
  if(is.null(censor_event)) {  # standard formula
    frml <- as.formula(paste(outcome,
                             ' ~ ',
                             paste(predictors, collapse = ' + '),
                             sep = ''))
  }
  
  else {  # Survival-type formula
    assertive.types::assert_is_character(censor_event)
    frml <- as.formula(paste('Surv(',
                             outcome,
                             ', ',
                             censor_event,
                             ')~',
                             paste(predictors, collapse = ' + '),
                             sep = ''))
  }
  
  return(frml)
}
