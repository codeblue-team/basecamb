#' Summarise a logistic regression model on the odds ratio scale
#'
#' This function summarises regression models that return data on the log-odds
#'   scale and returns a dataframe with estimates, and confidence intervals as
#'   odds ratios. P value are also provided.
#'   Additionally, intercepts can be removed from the summary. This comes in
#'   handy when ordinal logistic regression models are fit. Ordinal regression
#'   models (such as proportional odds models) usually result in many intercepts
#'   that are not really of interest.
#'   This function is also compatible with models obtained from multiply imputed
#'   datasets, for example models fitted with `Hmisc::fit.mult.impute()`.
#'
#' CAVE! The function does not check whether your estimates are on the
#'   log-odds scale. It will do the transformation no matter what!
#'
#' @param model a model object with estimates on the log-odds scale.
#' @param conf_int a numeric used to calculate the confidence intervals. The
#'   default of 1.96 gives the 95% confidence interval.
#' @param print_intercept a logical flag indicating whether intercepts shall
#'   be removed. All variables that start with "y>=" will be removed. If there
#'   is a variable matching this pattern, it will also be removed!
#' @param round_est the number of decimals returned for estimates (odds ratios)
#'   and confidence intervals.
#' @param round_p the number of decimals provided for p-values.
#'
#' @return a dataframe with the adjusted odds ratio, confidence intervals and
#'   p-values.
#'
#' @examples
#' # fit a logistic model
#' mod <- glm(formula = am ~ mpg + cyl, data = mtcars, family = binomial())
#'
#' or_model_summary(model = mod)
#'
#' @importFrom assertive.types assert_is_any_of
#' @importFrom assertive.types assert_is_a_number
#' @importFrom assertive.types assert_is_a_bool
#' @importFrom assertive.types assert_is_numeric
#' @importFrom stats pnorm
#' @importFrom stats vcov
#'
#' @export
#'
#' @author Till D. Best
or_model_summary <- function(model,
                             conf_int = 1.96, # 1.96 times the standard deviation gets you the 95% confidence interval
                             print_intercept = FALSE,
                             round_est = 3,
                             round_p = 4) {
  # assert input
  assertive.types::assert_is_any_of(x = model, classes = c("orm", "lrm", "rms", "glm"))
  assertive.types::assert_is_a_number(conf_int)
  assertive.types::assert_is_a_bool(print_intercept)
  assertive.types::assert_is_numeric(round_est)
  assertive.types::assert_is_numeric(round_p)

  # There's different ways of getting the variance of a model object depending
  #   on its class.
  #   Here we've decided to support glm and rms objects (including orm and
  #   lrm objects).

  # calculate adjusted odds ratio
  adjusted_odds_ratio <- exp(model$coefficients)

  if (inherits(x = model, what = "rms")) {
    # calculate lower confidence interval
    lower_ci <- exp(model$coefficients - conf_int * sqrt(diag(model$var)))
    # calculate upper confidence interval
    upper_ci <- exp(model$coefficients + conf_int * sqrt(diag(model$var)))
    # calculate two sided p_values
    p_value <- 2 * pnorm(-abs(model$coefficients / sqrt(diag(model$var))))

  } else {

    # calculate adjusted odds ratio
    adjusted_odds_ratio <- exp(model$coefficients)
    # calculate lower confidence interval
    lower_ci <- exp(model$coefficients - conf_int * sqrt(diag(vcov(model))))
    # calculate upper confidence interval
    upper_ci <- exp(model$coefficients + conf_int * sqrt(diag(vcov(model))))
    # calculate two sided p_values
    p_value <- 2 * pnorm(-abs(model$coefficients / sqrt(diag(vcov(model)))))
  }

  # put everything into a dataframe
  df <- data.frame(
    adj_OR = adjusted_odds_ratio,
    low_ci = lower_ci,
    up_ci = upper_ci,
    p_val = p_value
  )

  # round estimates according to the argument "round_est"
  df[1:3] <- round(df[1:3], round_est)

  # round p value and coerce to character
  df[4] <- ifelse(test = p_value < 10^-round_p, # if p_value smaller than specified level
                  yes = paste("<", as.character(10^-round_p), sep = ""), # replace with '<0.0...01'
                  no = as.character(round(p_value, round_p)) # else, round to specified level
                  )

  # remove intercepts from an ordinal logistic regression model if intercept flag is "FALSE"
  if (!print_intercept) {
    rownames <- rownames(df) # get the rownames from the return dataframe
    is_intercept <- startsWith(x = rownames, prefix = "y>=") # identify intercepts following the pattern "y>="
    df <- df[!is_intercept, ] # remove intercepts from return dataframe
  }

  return(df)
}
