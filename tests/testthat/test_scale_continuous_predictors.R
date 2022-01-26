library(basecamb)

test_that("scale_continuous_predictors scales model predictors correctly", {

  # ensure that we have a test for all S3 generics
  expect_equal(sort(utils::methods(scale_continuous_predictors)),
               sort(c('scale_continuous_predictors.default',
                      'scale_continuous_predictors.data.frame',
                      'scale_continuous_predictors.mids'))
               )

  # set up scaling factors
  scaling_dictionary <- data.frame('variable' = c('mpg', 'cyl', 'hp'),
                        'scaling_denominator' = c(2, 2, 0.5)
                        )

  # test function on a data.frame
  data <- datasets::mtcars
  expect_equal(scale_continuous_predictors(data, scaling_dictionary)$mpg, data$mpg/2)
  expect_equal(scale_continuous_predictors(data, scaling_dictionary)$cyl, data$cyl/2)
  expect_equal(scale_continuous_predictors(data, scaling_dictionary)$hp, data$hp*2)

  # test function on a mids object
  data$mpg[1] <- NA
  mids <- mice::mice(data, seed=1, printFlag = FALSE)
  long <- mice::complete(mids, action = "long", include = TRUE)
  mids_scaled <- scale_continuous_predictors(mids, scaling_dictionary)
  scaled_long <- mice::complete(mids_scaled, action = "long", include = TRUE)
  expect_equal(scaled_long$mpg, long$mpg/2)
  expect_equal(scaled_long$cyl, long$cyl/2)
  expect_equal(scaled_long$hp, long$hp*2)

  # test that we get warnings/errors on unexpected behaviour:
  # expect warning when submitting a non-supported class
  expect_warning(scale_continuous_predictors(list(), scaling_dictionary),
    'Objects of class list are not supported by scale_continuous_predictors()')
  scaling_err <- scaling_dictionary
  scaling_err$variable[2] <- NA
  # expect error when there is a scaling factor but no variable specified
  expect_error(scale_continuous_predictors(data, scaling_err),
               "In your scaling dictionary there is at least 1 cell in the column 'scaling_denominator' that has no entry in the column 'variable'.\nPlease check your scaling dictionary and fix this.")
  # expect warning when a factor is selected for scaling,but return rest scaled normally
  data_err <- data
  data_err$cyl <- as.factor(data_err$cyl)
  expect_warning(scale_continuous_predictors(data_err, scaling_dictionary), "Column 'cyl' is of type factor and cannot be scaled.")
  scaling_err$scaling_denominator[2] <- NA # removing cyl from variables to be scaled
  data_scaled_fact <- scale_continuous_predictors(data, scaling_err)
  data_scaled_fact$cyl <- as.factor(data_scaled_fact$cyl)
  expect_equal(suppressWarnings(scale_continuous_predictors(data_err, scaling_dictionary)),
               data_scaled_fact)

})
