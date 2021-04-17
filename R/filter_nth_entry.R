#' Filter dataframe for nth entry
#'
#' Filter a dataframe for the nth entry of each subject in it.
#' A typical use cases would be to filter a dataset for the first or last measurement of a subject.#'
#'
#' @param data the data.frame to filter
#' @param ID_column character column identifying subjects
#' @param entry_column character column identifying order of entries.
#' That column can by of types Date, numeric, or any other type suitable for order()
#' @param n integer number of entry to keep after ordering
#' @param reverse_order logical when TRUE sorts entries last to first before filtering
#'
#' @return data.frame with <= 1 entry per subject
#'
#' @examples
#' data <- data.frame(list(ID = rep(1:5, 3), encounter = rep(1:3, each=5), value = rep(4:6, each=5)))
#'
#'
#' @importFrom assertive.types assert_is_data.frame
#' @importFrom assertive.types assert_is_character
#' @importFrom assertive.types assert_is_a_number
#' @importFrom assertive.types assert_is_a_bool
#'
#' @export
#'
#' @author J. Peter Marquardt
filter_nth_entry <- function(data, ID_column, entry_column, n=1, reverse_order=FALSE){
  assertive.types::assert_is_data.frame(data)
  assertive.types::assert_is_character(ID_column)
  assertive.types::assert_is_character(entry_column)
  assertive.types::assert_is_a_number(n)
  assertive.types::assert_is_a_bool(reverse_order)
  stopifnot(ID_column %in% names(data))
  stopifnot(entry_column %in% names(data))

  filtered_df <- data.frame()
  ID_list <- c()
  for (i in seq(1, nrow(data))){
    ID <- data[[i, ID_column]]
    if (!(ID %in% ID_list)) {
      ID_list <- append(ID_list, ID)
      subject_df <- data[data[[ID_column]] == ID, ]
      subject_df <- subject_df[order(subject_df[[entry_column]], decreasing = reverse_order),]
      if(!(is.na(subject_df[[n, ID_column]]))){
        filtered_df <- rbind(filtered_df, subject_df[n, ])
      }
    }
  }
  return(filtered_df)
}
