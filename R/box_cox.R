# Functions to simplify Box-Cox transformation


#' Box-Cox transformation for stratified data
#'
#' Create Box-Cox transformation using different optimal lambda values for each stratum
#'
#' @param data data.frame containing the data
#' @param value_col character, name of column with values to be transformed
#' @param strat_cols character (vector), name(s) of columns to stratify by
#' @param plot logical, should the lambda distribution be plotted?
#' @param return character, either "values" or "lambdas"
#' @param buffer numeric, buffer value to be added before transformation, used to ensure all positive values
#' @param inverse logical, if TRUE, the function reverses the transformation given a list of lambdas
#' @param lambdas if inverse == TRUE: Nested list of lambdas used in original transformation. Can be obtained by using return = "lambdas" on untransformed data
#'
#' @return if "values", vector of transformed values, if "lambdas" nested named list of used lambdas. The buffer will be equal for all strata
#'
#' @examples
#' data <- data.frame("value" = c(1:50, rnorm(50, 100, 10)),
#'                    "strat_var" = rep(c(1,2), each = 50),
#'                    "strat_var2" = rep(c(1, 2), 50))
#' lambdas <- stratified_boxcox(data = data, value_col = "value",
#'                              strat_cols = c("strat_var", "strat_var2"),
#'                              return = "lambdas")
#' data$value_boxed <- stratified_boxcox(data = data, value_col = "value",
#'                                       strat_cols = c("strat_var", "strat_var2"),
#'                                       return = "values")
#' data$value_unboxed <- stratified_boxcox(data = data, value_col = "value_boxed",
#'                                         strat_cols = c("strat_var", "strat_var2"),
#'                                         inverse = TRUE, lambdas = lambdas)
#'
#' @importFrom assertthat assert_that
#' @importFrom MASS boxcox
#' @importFrom sae bxcx
#'
#' @export
#'
#' @author J. Peter Marquardt
stratified_boxcox <- function(data, value_col, strat_cols, plot=FALSE, return="values", buffer=0, inverse=FALSE, lambdas=NULL) {

  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(is.character(value_col))
  assertthat::assert_that(is.character(strat_cols))
  assertthat::assert_that(is.numeric(data[[value_col]]))
  sapply(strat_cols, function(col) assertthat::assert_that(!is.null(data[[col]])))
  assertthat::assert_that(is.numeric(buffer))
  assertthat::assert_that(is.logical(inverse))
  if(inverse) {
    assertthat::assert_that(is.list(lambdas))
  }
  else {
    assertthat::assert_that(is.logical(plot))
    assertthat::assert_that(return %in% c("values", "lambdas"))
  }


  if (length(strat_cols) == 1) {

    if (!inverse) { # Forward transformation
      data[[value_col]] <- data[[value_col]] + buffer # add buffer to avoid fitting on negative values

      # we have reached the final characteristic to stratify by
      if (return == "lambdas") lambdas <- list()
      for (level in unique(data[[strat_cols]])) {

        values <- eval(data[data[[strat_cols]] == level, ][[value_col]]) #extract values as vector
        bcx <- MASS::boxcox(values ~ 1, plotit = plot)
        lmbda <- bcx$x[which.max(bcx$y)]

        if (return == "values") {
          data[[value_col]] <- sapply(seq(nrow(data)),
                                      function(rownm) ifelse(data[[rownm, strat_cols]] == level & !is.na(data[[rownm, value_col]]),
                                                             sae::bxcx(data[[rownm, value_col]], lmbda),
                                                             data[[rownm, value_col]]
                                      ))
        }
        else {
          lambdas[paste0(strat_cols, "_", level)] <- lmbda
        }

      }

      if(return == "values") return(data[[value_col]])
      if (return == "lambdas") return(lambdas)
      stop("Default elephant in Cairo")
    }
    else { # reverse transformation

      for (level in unique(data[[strat_cols]])) {

          data[[value_col]] <- sapply(seq(nrow(data)),
                                      function(rownm) ifelse(data[[rownm, strat_cols]] == level & !is.na(data[[rownm, value_col]]),
                                                             sae::bxcx(data[[rownm, value_col]],
                                                                       lambdas[[paste0(strat_cols, "_", level)]],
                                                                       InverseQ = TRUE),
                                                             data[[rownm, value_col]]
                                      )
          )

      }

      return(data[[value_col]])

    }

  }
  else {

    # Multiple characteristics to stratify by, invoke recursive design
    if(!inverse) {
      if (return == "values") {

        data[[paste0("temp_index_", strat_cols[1])]] <- seq(1, nrow(data)) # keep track of row indices
        return_df <- data.frame()

        for (level in unique(data[[strat_cols[1]]])) {
          df <- data[data[[strat_cols[1]]] == level, ]
          df[[value_col]] <- stratified_boxcox(df,
                                               value_col = value_col,
                                               strat_cols = strat_cols[seq(2, length(strat_cols))],
                                               plot = plot,
                                               return = return,
                                               buffer = buffer)
          return_df <- rbind(return_df, df)
        }

        data <- return_df[order(return_df[[paste0("temp_index_", strat_cols[1])]]), ] #reinstate original order

        return(data[[value_col]])

      }
      if (return == "lambdas") {

        lambdas <- list()
        for (level in unique(data[[strat_cols[1]]])) {
          df <- data[data[[strat_cols[1]]] == level, ]
          # elaborate scheme to silence warning of assigning to empty named list
          lambdas[[paste0(strat_cols[1], "_", level)]] <- stratified_boxcox(df,
                                                                            value_col = value_col,
                                                                            strat_cols = strat_cols[seq(2, length(strat_cols))],
                                                                            plot = plot,
                                                                            return = return,
                                                                            buffer = buffer)
        }

        return(lambdas)

      }
    }
    else if (inverse == TRUE) { #reverse transformation

      data[[paste0("temp_index_", strat_cols[1])]] <- seq(1, nrow(data)) # keep track of row indices
      return_df <- data.frame()

      for (level in unique(data[[strat_cols[1]]])) {
        df <- data[data[[strat_cols[1]]] == level, ]
        df[[value_col]] <- stratified_boxcox(df,
                                             value_col = value_col,
                                             strat_cols = strat_cols[seq(2, length(strat_cols))],
                                             plot = plot,
                                             return = return,
                                             buffer = buffer,
                                             inverse = inverse,
                                             lambdas = lambdas[[paste0(strat_cols[1], "_", level)]])
        return_df <- rbind(return_df, df)
      }

      data <- return_df[order(return_df[[paste0("temp_index_", strat_cols[1])]]), ] #reinstate original order

      return(data[[value_col]])

    }

    else stop("Default elephant in Cairo")

  }

}
