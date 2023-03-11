#' Stratify a numeric vector into quantile groups
#'
#' Transforms a numeric vector into quantile groups. For each input value, the output value corresponds to the quantile that value is in.
#' When grouping into n quantiles, the lowest 1/n of values are assigned 1, the highest 1/n are assigned n.
#'
#' @param data a vector of type numeric with values to be grouped into quantiles
#' @param n integer indicating number of quantiles, minimum of 2. Must be smaller than length(data)
#' @param na.rm logical; if TRUE all NA values will be removed before calculating groups, if FALSE no NA values are permitted.
#'
#' @return vector of length length(data) with the quantile groups
#'
#' @examples quantile_group(10:1, 3)
#' @examples quantile_group(c(rep(1,3), 10:1, NA), 5)
#'
#' @details Tied values will be assigned to the lower quantile group rather than etsimating a distribution. In extreme cases this can mean one or more quantile groups are not represented.
#' @details If uneven group sizes cannot be avoided, values will be assigned the higher quantile group.
#'
#' @author J. Peter Marquardt
#'
#' @importFrom assertive.types assert_is_numeric
#' @importFrom assertthat assert_that
#'
#' @export
quantile_group <- function(data, n, na.rm=TRUE) {

  # Assertions
  assertive.types::assert_is_numeric(data)
  assertthat::assert_that(!any(is.na(data)) | na.rm)
  assertive.types::is_numeric(n)
  assertthat::assert_that(n %% 1 == 0)
  assertthat::assert_that(n >= 2)
  assertthat::assert_that(n < length(data))

  # Build auxiliary data.frame
  quantiles <- data.frame("rnum" = 1:length(data))
  quantiles$values <- data
  quantiles$quantile_group <- NA

  # Assign quantile groups
  quants <- quantiles[!is.na(quantiles$values), ]
  quants <- quants[order(quants$values), ]  # Order according to values
  current_quantile <- 1

  for (i in seq(1, nrow(quants))){
    if (i/nrow(quants) > current_quantile/n) {  # move up to next quantile group
      current_quantile <- current_quantile + 1
    }
    # correct quantile from here on out
    if (i == 1) {
      quants$quantile_group[i] <- 1
    }
    else {
      # check for ties
      if(quants$values[i] == quants$values[i-1]) {
        quants$quantile_group[i] <- quants$quantile_group[i-1]
      }
      else {
        quants$quantile_group[i] <- current_quantile
      }
    }
  }

  # Export quantile groups
  quantiles['output_col'] <- as.double(NA)
  for (i in seq(1, nrow(quants))) {
    ind = quants$rnum[i]
    quantiles[ind,'output_col'] <- quants$quantile_group[i]
  }
  return(quantiles$output_col)
}
