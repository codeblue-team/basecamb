# Dealing with duplicate entries in data sets

#' Identify duplicate values in a vector representing a set
#'
#' @param vect a vector of any type
#'
#' @return a vector of duplicate elements
#'
#' @examples setduplicates(c(1,2,2,3))
#'
#' @seealso \link[generics]{setops}
#'
#' @export
#'
#' @author J. Peter Marquardt
setduplicates <- function(vect) {
  stopifnot(is.vector(vect))
  unique_list <- c()
  duplicate_list <- c()
  for (i in seq(1, length(vect))) {
    if (vect[i] %in% unique_list) {
      duplicate_list <- append(duplicate_list, vect[i])
    }
    else {
      unique_list <- append(unique_list, vect[i])
    }
  }
  return(unique(duplicate_list))
}


#' Remove duplicate rows from data.frame
#'
#' Removes rows that are duplicates of another row in all columns except exclude_columns
#'
#' Wraps unique()
#'
#' @param data data.frame to check
#' @param exclude_columns character vector, these columns are not considered in determining whether two rows are equal
#' @param ID_column character; column with identifiers to scan if possible duplicates remain
#' @param quiet logical: Should messages be printed?
#'
#' @return vector of row indices with non-unique data
#'
#' @importFrom dplyr setdiff
#'
#' @examples
#' data <- data.frame(Study_ID = c("A", "B", "C"), ID = c(123, 456, 123), num_cars = c(10, 2, 10))
#' remove_duplicates(data, exclude_columns = "Study_ID")
#' remove_duplicates(data, exclude_columns = "Study_ID", ID_column = "ID")
#'
#' @export
#'
#' @author J. Peter Marquardt
remove_duplicates <- function(data, exclude_columns=NULL, ID_column=NULL, quiet=FALSE) {
  stopifnot(is.data.frame(data))
  if (!is.null(exclude_columns)) {stopifnot(all(exclude_columns %in% colnames(data)))}
  else {exclude_columns = ""}
  if (!is.null(ID_column)) {stopifnot(ID_column %in% colnames(data))}
  stopifnot(is.logical(quiet))

  compare_cols <- dplyr::setdiff(colnames(data), exclude_columns)
  unique_df <- data[rownames(unique(data [, compare_cols])), ]
  removed_row_indices <- dplyr::setdiff(rownames(data), rownames(unique_df))

  if(!quiet) {
    msg <- sprintf("Removed %s duplicate row(s)", length(removed_row_indices))
    if (!is.null(ID_column)) {
      msg <- append(msg, sprintf(" for ID(s): %s", paste(unique(data[removed_row_indices, ID_column]), collapse = ", ")))
      remaining_duplicate_IDs <- unique(setduplicates(unique_df[[ID_column]]))
      if (length(remaining_duplicate_IDs) > 0) msg <- append(msg, sprintf("\nPossible duplicates remaining for IDs: %s", paste(remaining_duplicate_IDs, collapse = ", ")))
    }
    message(msg)
  }

  return(unique_df)

}
