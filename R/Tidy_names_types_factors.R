#' Assign tidy types and names to a data.frame
#'
#' Verbosely assign tidy name and data type for each column of a data.frame and
#'   get rid of superfluous columns. Uses a .csv file for assignments to
#'   encourage a data dictionary based workflow.
#'   CAVE! Requires 'Date' type columns to already be read in as Date.
#'
#' @param data data.frame to be tidied. Dates must already be of type date.
#' @param meta_data data.frame specifying old column names, new column names and
#'   datatypes of data. Has the following columns:
#' * old_column_name : character with the old column name.
#' * new_data_type : character denoting the tidy data type. Supported types are:
#'   * character (will be coerced using `as.character()`).
#'   * integer (will be coerced using `as.integer()`).
#'   * float (will be coerced using `as.double()`).
#'   * factor (will be coerced using `as.factor()`).
#'   * date (can only confirm correct datatype assignment, not coerce other types into date. Uses `as.Date()`).
#' * new_column_name : tidy column name. Can be left blank to keep the old column name.
#' * Optional other columns (do not affect behavior).
#'
#' @return clean data.frame
#'
#' @importFrom assertive.types assert_is_data.frame
#' @importFrom assertive.types assert_is_data.frame
#'
#' @export
#'
#' @author J. Peter Marquardt
assign_types_names <- function(data, meta_data) {

  assertive.types::assert_is_data.frame(data)
  assertive.types::assert_is_data.frame(meta_data)

  # filtering out unused columns
  data_to_use <- meta_data[!is.na(meta_data$new_data_type),
                      c("old_column_name", "new_data_type", "new_column_name")
                      ]
  filtered_data <- data[, data_to_use$old_column_name]

  # assigning new names
  data_to_use$new_column_name <- ifelse(!is.na(data_to_use$new_column_name),
                                        data_to_use$new_column_name,
                                        data_to_use$old_column_name
                                        )
  colnames(filtered_data) <- data_to_use$new_column_name

  # assigning correct datatypes
  for (i in seq_len(nrow(data_to_use))) {
    if (data_to_use$new_data_type[i] == "character") {
      filtered_data[data_to_use$new_column_name[i]] <-
        as.character(filtered_data[[data_to_use$new_column_name[i]]])
    }
    else if (data_to_use$new_data_type[i] == "integer") {
      filtered_data[data_to_use$new_column_name[i]] <-
        as.integer(filtered_data[[data_to_use$new_column_name[i]]])
    }
    else if (data_to_use$new_data_type[i] == "float") {
      filtered_data[data_to_use$new_column_name[i]] <-
        as.double(filtered_data[[data_to_use$new_column_name[i]]])
    }
    else if (data_to_use$new_data_type[i] == "factor") {
      filtered_data[data_to_use$new_column_name[i]] <-
        as.factor(filtered_data[[data_to_use$new_column_name[i]]])
    }
    else if (data_to_use$new_data_type[i] == "date") {
      filtered_data[data_to_use$new_column_name[i]] <-
        as.Date(filtered_data[[data_to_use$new_column_name[i]]])
    }
    else {
      warning(sprintf('Type %s not recognized for column %s. Type remains %s.',
                     as.character(data_to_use$new_data_type[i]),
                     as.character(data_to_use$old_column_name[i]),
                     as.character(typeof(filtered_data[data_to_use$new_column_name[i]]))
                     )
              )
    }
  }

  #return
  return(filtered_data)
}


#' Assign custom values for key levels in factorial columns
#'
#' Use a named vector of keys (current value) and values for factorial columns
#' to assign meaningful levels and/or group levels
#'
#' @param data data.frame to modify
#' @param factor_keys_values named list with:
#' * Keys: Names of factor columns
#' * values: Named vectors with
#'    * keys: current value (string representation)
#'    * values: new value to be assigned
#'    * if a 'default' key is passed, all values not conforming to the new scheme will be converted to the 'default' value
#'
#' @return data frame with new levels
#'
#' @examples
#' data <- data.frame(col1 = as.factor(rep(c('1', '2', '4'), 5)))
#' keys_1 <- list('col1' = c('1' = 'One', '2' = 'Two', '4' = 'Four'))
#' data_1 <- assign_factorial_levels(data, keys_1)
#' keys_2 <- list('col1' = c('1' = 'One', 'default' = 'Not_One'))
#' data_2 <- assign_factorial_levels(data, keys_2)
#'
#' @importFrom assertive.types assert_is_data.frame
#' @importFrom assertive.types assert_is_list
#'
#' @export
#'
#' @author J. Peter Marqurdt
assign_factorial_levels <- function(data, factor_keys_values) {

  assertive.types::assert_is_data.frame(data)
  assertive.types::assert_is_list(factor_keys_values)

  for (col in names(factor_keys_values)) {
    data[col] <- as.character.factor(data[[col]])
    for (level in names(factor_keys_values[[col]])) {
      data[col] <- ifelse(data[[col]] == level,
                          factor_keys_values[[col]][[level]],
                          data[[col]])
      if (level == "default") { # assign default value to all fields that don't have a value yet
        data[col] <- ifelse(!data[[col]] %in% unname(factor_keys_values[[col]]),
                            factor_keys_values[[col]][[level]],
                            data[[col]])
      }
    }
  }
  return(data)
}
