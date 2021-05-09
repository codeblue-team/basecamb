#' Scale continuous predictors
#'
#' This function linearly scales variables in data objects according to a data dictionary.
#'   The data dictionary has at least two columns, "variable" and "scaling_denominator".
#'   "Variable" is divided by "scaling_denominator".
#'
#' @param data a data object with variables.
#' @param scaling_dictionary a data.frame with two columns that are called
#'   "variable" and "scaling_denominator".
#'
#' @return The data with the newly scaled 'variables'.
#'
#' @importFrom dplyr pull
#' @importFrom assertive.types assert_is_numeric assert_is_character
#'
#' @export
#'
#' @author Till D. Best
scale_continuous_predictors <- function(data, scaling_dictionary) {
  UseMethod("scale_continuous_predictors")
}


#' @export
scale_continuous_predictors.default <- function(data, scaling_dictionary) {
  warning(sprintf('Objects of class %s are not supported by scale_continuous_predictors()', class(data)[1]))
}


#' @export
scale_continuous_predictors.data.frame <- function(data, scaling_dictionary) {

  # check scaling_dictionary data types
  assertive.types::assert_is_numeric(x = scaling_dictionary[["scaling_denominator"]])
  assertive.types::assert_is_character(x = scaling_dictionary[["variable"]])
  # remove cases where scaling is NA
  dict_df <- scaling_dictionary[!is.na(scaling_dictionary[['scaling_denominator']]), ]

  # find remaining cases where variable is NA
  if (sum(is.na(dict_df[["variable"]])) >= 1L) {
    stop("In your scaling dictionary there is at least 1 cell in the column 'scaling_denominator' that has no entry in the column 'variable'.\nPlease check your scaling dictionary and fix this.")
  }

  # turn into list
  dict_list <- dplyr::pull(.data = dict_df, var = 'scaling_denominator', name = variable)

  # scale all variables in dict_list according to the scaling value
  data_scaled <- data
  # update variables that shall be scaled
  for (var in names(dict_list)) {
    data_scaled <- .scale_variable(data = data_scaled,
                                   variable = var,
                                   scaling_denominator = dict_list[[var]])
  }

  # return
  return(data_scaled)
}


#' @export
scale_continuous_predictors.mids <- function(data, scaling_dictionary) {
  data_scaled <- apply_function_to_imputed_data(mice_data = data,
                                                fun = scale_continuous_predictors,
                                                scaling_dictionary = scaling_dictionary)

  return(data_scaled)
}


#' Scaling a variable
#'
#' A helper function to scale a variable in a dataframe.
#' Divides 'variable' by 'scaling_denominator'.
#'
#' @param data data.frame
#' @param variable a char indicating the variable to be scaled
#' @param scaling_denominator a numeric indicating the scaling. The variable is
#'   divided by the scaling_denominator.
#'
#' @return the input dataframe with the newly scaled 'variable'
.scale_variable <- function(data, variable, scaling_denominator) {
  if (is.factor(data[[variable]])) {
    warning(sprintf("Column '%s' is of type factor and cannot be scaled.", variable))
  }
  else {
    data[[variable]] <- data[[variable]] / scaling_denominator
  }
  return(data)
}
