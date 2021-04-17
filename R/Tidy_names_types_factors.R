#' Clean column names, types and levels
#'
#' Use a data dictionary data.frame to apply the following tidying steps to your data.frame:
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
#'   * date
#' * new_column_name : tidy column name. Can be left blank to keep the old column name
#' * coding (factor and date columns only):
#'   * factor columns: character denoting old value (key) and new value (value) in a standardised fashion:
#'     * key-value pairs are separated from other key-value-pairs by a comma (",")
#'     * key and value of the same pair are separated by an equal sign ("=")
#'     * quotations around individual keys and values are recommended for clarity, but do not affect functionality.
#'     * all values will be coerced to type character, with the exception of "NA" being parsed as type NA
#'     * using "default" as a key will assign the specified value to all current values that do not match any of the specified keys, excluding NA
#'     * using "NA" as a key will assign the specified value to all current NA values
#'     * example coding: "'key1' = 'val1', 'key2' = 'val2', 'default' = 'Other', 'NA' = NA"
#'     * if no coding is specified for a column, the coding remains unchanged
#'   * date columns: character denoting coding (see format argument in `as.Date`)
#' * Optional other columns (do not affect behaviour)
#' @param na_action_default character: Specify what to do with NA values. Defaults to 'keep_NA'. Options are:
#' * 'keep_NA' NA values remain NA values
#' * 'assign_default' NA values are assigned the value specified as 'default'. Requires a 'default' value to be specified
#' Can be overwritten for individal columns by specifying a value for key 'NA'
#'
#' @return clean data.frame
#'
#' @importFrom assertive.types is_data.frame
#' @importFrom assertthat assert_that
#'
#' @export
#'
#' @author J. Peter Marquardt
apply_data_dictionary <- function(data, data_dictionary, na_action_default='keep_NA') {

  assertive.types::is_data.frame(data)
  assertive.types::is_data.frame(data_dictionary)
  assertthat::assert_that(na_action_default %in% c("keep_NA", "assign_default"))

  # date columns only: Parse dates in the correct format
  for (rw in seq(1, nrow(data_dictionary))) {
    if (data_dictionary[[rw, "new_data_type"]] == "date" & !is.na(data_dictionary[[rw, "new_data_type"]])) {
      date_format <- list()
      date_format[[data_dictionary[[rw, "old_column_name"]]]] <- data_dictionary[[rw, "coding"]]
      data <- parse_date_columns(data = data, date_formats = date_format)
    }
  }

  # assign correct data types and new column names
  data <- assign_types_names(data = data, meta_data = data_dictionary)

  # factor columns only: Assign updated factor levels
  fact_cols_only <- data_dictionary[data_dictionary[["new_data_type"]] == "factor" & !is.na(data_dictionary[["new_data_type"]]), ]

  fact_coding_list <- list()
  if(nrow(fact_cols_only) > 0) {
    for (i in seq(1, nrow(fact_cols_only))) {

      if(is.na(fact_cols_only[[i, "coding"]])) {  # skip columns without specified coding
        next
      }

      col_name <- ifelse(!is.na(fact_cols_only[[i, "new_column_name"]]),
                         fact_cols_only[[i, "new_column_name"]],
                         fact_cols_only[[i, "old_column_name"]])

      fact_coding_list[[col_name]] <- .parse_string_to_named_vector(fact_cols_only[[i, "coding"]])
    }
  }


  data <- assign_factorial_levels(data = data, factor_keys_values = fact_coding_list, na_action_default = na_action_default)

  return(data)
}


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
#'   Will result in a warning if the new factor variable will have more than 10 levels.
#'   * date (can only confirm correct datatype assignment or coerce characters with format '%Y-%m-%d').
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
      if(length(levels(filtered_data[[data_to_use$new_column_name[i]]])) > 10) {
        warning(sprintf('In column %s, created a factor with %s levels',
                        data_to_use$old_column_name[i],
                        length(levels(filtered_data[[data_to_use$new_column_name[i]]]))))
      }
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
#'    * if a 'default' key is passed, all existing values not conforming to the new scheme will be converted to the 'default' value
#'    * if a 'NA' key is passed, all NA values will be converted to the value specified here. Overwrites na_action_default for the specified column.
#' @param na_action_default character: Specify what to do with NA values. Defaults to 'keep_NA'. Options are:
#' * 'keep_NA' NA values remain NA values
#' * 'assign_default' NA values are assigned the value specified as 'default'. Requires a 'default' value to be specified
#' Can be overwritten for individal columns by specifying a value for key 'NA'
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
#' @importFrom assertthat assert_that
#'
#' @export
#'
#' @author J. Peter Marquardt
assign_factorial_levels <- function(data, factor_keys_values, na_action_default="keep_NA") {

  assertive.types::assert_is_data.frame(data)
  assertive.types::assert_is_list(factor_keys_values)
  assertthat::assert_that(na_action_default %in% c("keep_NA", "assign_default"))

  for (col in names(factor_keys_values)) {

    # Establish action when encountering NA values
    if (! "NA" %in% names(factor_keys_values[[col]])) {
      if(na_action_default == "keep_na") {
        factor_keys_values[[col]]['NA'] <- NA
      }
      else if(na_action_default == "assign_default") {

        if (! "default" %in% names(factor_keys_values[[col]])) {
          stop(sprintf("In column %s: Selected \"assign_default\" as na_action_default without specifying default level.", col))
        }
        factor_keys_values[[col]]['NA'] <- factor_keys_values[[col]]['default']
      }
    }

    data[col] <- as.character.factor(data[[col]])
    for (level in names(factor_keys_values[[col]])) {
      data[col] <- ifelse(data[[col]] == level,
                          factor_keys_values[[col]][[level]],
                          data[[col]])
      if (level == "NA") {
        data[col] <- ifelse(is.na(data[[col]]),
                            factor_keys_values[[col]][[level]],
                            data[[col]])
      }
      if (level == "default") { # assign default value to all fields that don't have a value yet
        data[col] <- ifelse(!data[[col]] %in% unname(factor_keys_values[[col]])
                            & !data[[col]] %in% names(factor_keys_values[[col]])
                            & !is.na(data[[col]]),
                            factor_keys_values[[col]][[level]],
                            data[[col]])
      }
    }

    data[col] <- as.factor(data[[col]])  # coercing into type factor
  }

  return(data)
}


#' Parse values in date columns as Dates
#'
#' Parse date columns in a data.frame as Date.
#' Use a named list to specify each date column (key) and the format (value) it is coded in.
#'
#' @param data data.frame to modify
#' @param date_formats named list with:
#' * Keys: Names of date columns
#' * values: character specifying the format
#'
#' @return data.frame with date columns in Date type
#'
#' @examples
#' data <- data.frame(date = rep('01/23/4567', 5))
#' data <- parse_date_columns(data, list(date = '%m/%d/%Y'))
#'
#' @importFrom assertive.types assert_is_data.frame
#' @importFrom assertive.types assert_is_list
#'
#' @export
#'
#' @author J. Peter Marquardt
parse_date_columns <- function(data, date_formats) {

  assertive.types::assert_is_data.frame(data)
  assertive.types::assert_is_list(date_formats)

  for (col in names(date_formats)) {

    date_format <- date_formats[[col]]
    if (is.na(date_formats[[col]])) {
      warning(sprintf("No date format specified for column %s. Using %s.", col, "%Y-%m-%d"))
      date_format <- "%Y-%m-%d"
    }
    else if (!is.character(date_formats[[col]])) {
      stop(sprintf("Date format for column %s is not of type character", col))
    }

    data[col] <- as.Date(data[[col]], format = date_format)
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
#'
#' @importFrom assertive.types assert_is_character
#'
#' @keywords internal
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
