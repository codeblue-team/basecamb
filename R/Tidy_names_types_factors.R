#' Assign tidy types and names to a data.frame
#'
#' Verbosely assign tidy name and data type for each column of a data.frame and get rid of superfluous columns
#' Uses a .csv file for assignments to encourage a data dictionary based workflow.
#' Requires 'Date' type columns to already be read in as Date.
#'
#' @param data data.frame to be tidied. Dates must already be of type date
#' @param meta_data data.frame specifying old and new column names and datatypes of data. Has columns
#' * old_column_name : character with the old column name
#' * new_data_type : character denoting the tidy data type. Supported types are:
#'   * character
#'   * integer
#'   * float
#'   * factor
#'   * date (can only confirm correct datatype assignment, not coerce other types into date)
#' * new_column_name : tidy column name. Can be left blank to keep the old column name
#' * Optional other columns (do not affect behaviour)
#'
#' @return clean data.frame
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


#' Parse a string to create a named list
#'
#' Create a named list from a standardised string of the follwoing format:
#'   * key-value pairs are separated from other key-value-pairs by a comma (",")
#'   * key and value of the same pair are separated by an equal sign ("=")
#'   * quotations around individual keys and values are recommended for clarity, but do not affect functionality.
#'   * all values will be coerced to type character, with the exception of "NA", "TRUE" and "FALSE"
#'
#' @param str character with standardised pattern to be parsed
#'
#' @return named vector
#'
#' @examples
#' .parse_string_to_named_vector("'key1' = 'val1', 'key2' = 'val2'")
#' .parse_string_to_named_vector("'key1' = 'val1', 'key2' = 'val2', 'default' = 'NA'")
#'
#' @importFrom assertive.types assert_is_character
#'
#' @author J. Peter Marquardt
.parse_string_to_named_vector <- function(str) {

  assertive.types::assert_is_character(str)

  key_value_pairs <- strsplit(gsub("\"|\'| ", '', str), split = ",")[[1]]

  vct <- vector()
  for (kvp in key_value_pairs) {
    if(strsplit(kvp, split = "=")[[1]][2] == "NA") {
      vct[strsplit(kvp, split = "=")[[1]][1]] <- NA
    }
    else if(strsplit(kvp, split = "=")[[1]][2] == "TRUE") {
      vct[strsplit(kvp, split = "=")[[1]][1]] <- TRUE
    }
    else if(strsplit(kvp, split = "=")[[1]][2] == "FALSE") {
      vct[strsplit(kvp, split = "=")[[1]][1]] <- FALSE
    }
    else{
      vct[strsplit(kvp, split = "=")[[1]][1]] <- strsplit(kvp, split = "=")[[1]][2]
    }
  }

  return(vct)
}


#' Clean column names, types and levels
#'
#' Use a data dictionary data.frame to apply the follwowing tidying steps to your data.frame:
#'   * Remove superfluous columns
#'   * Rename columns
#'   * Ensure/coerce correct data type for each column
#'   * Assign factorial levels, including renaming and grouping
#'
#' @param data data.frame to be cleaned
#' @param data_dictionary data.frame with the following columns:
#' * old_column_name : character with the old column name
#' * new_data_type : character denoting the tidy data type. Supported types are:
#'   * character
#'   * integer
#'   * float
#'   * factor
#'   * date (can only confirm correct datatype assignment, not coerce other types into date)
#' * new_column_name : tidy column name. Can be left blank to keep the old column name
#' * coding (factor columns only) : character denoting old value (key) and new value (value) in a standardised fashion:
#'   * key-value pairs are separated from other key-value-pairs by a comma (",")
#'   * key and value of the same pair are separated by an equal sign ("=")
#'   * quotations around individual keys and values are recommended for clarity, but do not affect functionality.
#'   * all values will be coerced to type character, with the exception of "NA" being parsed as type NA
#'   * using "default" as a key will assign the specified value to all current values that do not match any of the specified keys, including NA
#'   * example coding: "'key1' = 'val1', 'key2' = 'val2', 'default' = NA"
#'   * if no coding is specified for a column, the coding remains unchanged
#' * Optional other columns (do not affect behaviour)
#'
#' @return clean data.frame
#'
#' @importFrom assertive.types assert_is_character
#'
#' @export
#'
#' @author J. Peter Marquardt
apply_data_dictionary <- function(data, data_dictionary) {

  assertive.types::is_data.frame(data)
  assertive.types::is_data.frame(data_dictionary)

  # assign correct data types and new column names
  data <- assign_types_names(data = data, meta_data = data_dictionary)

  # factor columns only: Assign updated factor levels
  fact_cols_only <- data_dictionary[data_dictionary[["new_data_type"]] == "factor" & !is.na(data_dictionary[["new_data_type"]]), ]

  fact_coding_list <- list()
  for (i in seq(1, nrow(fact_cols_only))) {

    if(is.na(fact_cols_only[[i, "coding"]])) {  # skip columns without specified coding
      next
    }

    col_name <- ifelse(!is.na(fact_cols_only[[i, "new_column_name"]]),
                       fact_cols_only[[i, "new_column_name"]],
                       fact_cols_only[[i, "old_column_name"]])

    fact_coding_list[[col_name]] <- .parse_string_to_named_vector(fact_cols_only[[i, "coding"]])
  }

  data <- assign_factorial_levels(data = data, factor_keys_values = fact_coding_list)

  return(data)
}
