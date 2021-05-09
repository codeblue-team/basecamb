library(basecamb)

test_that("scale_continuous_predictors scales model predictors correctly", {

  # ensure that we have a test for all S3 generics
  expect_equal(sort(utils::methods(scale_continuous_predictors)),
               sort(c('scale_continuous_predictors.default',
                      'scale_continuous_predictors.data.frame',
                      'scale_continuous_predictors.default'))
               )

  # set up scaling factors
  scaling <- data.frame('variable' = c('mpg', 'cyl', 'hp'),
                        'scaling' = c(2, 2, 0.5)
                        )

  # test function on a data.frame
  data <- datasets::mtcars
  expect_equal(scale_continuous_predictors(data, scaling)$mpg, data$mpg/2)
  expect_equal(scale_continuous_predictors(data, scaling)$cyl, data$cyl/2)
  expect_equal(scale_continuous_predictors(data, scaling)$hp, data$hp*2)

  # test function on a mids object
  data$mpg[1] <- NA
  mids <- mice::mice(data, seed=1)
  long <- mice::complete(mids, action = "long", include = TRUE)
  mids_scaled <- scale_continuous_predictors(mids, scaling)
  scaled_long <- mice::complete(mids_scaled, action = "long", include = TRUE)
  expect_equal(scaled_long$mpg, long$mpg/2)
  expect_equal(scaled_long$cyl, long$cyl/2)
  expect_equal(scaled_long$hp, long$hp*2)

  # test that we get warnings/errors on unexpected behaviour:
  # expect warning when submitting a non-supported class
  expect_warning(scale_continuous_predictors(list(), scaling),
    'Objects of class list are not supported by scale_continuous_predictors()')
  scaling_err <- scaling$variable[2] <- NA
  # expect error when there is a scaling factor but no variable specified
  expect_error(scale_continuous_predictors(data, scaling_err),
               "In your scaling dictionary there is at least 1 cell in the column 'scaling' that has no entry in the column 'variable'.\nPlease check you scaling dictionary and fix this.")
  data_err <- data
  # expect warning when a factor is selected for scaling,but return rest scaled normally
  data_err$cyl <- as.factor(data_err$cyl)
  expect_warning(scale_continuous_predictors(data_err, scaling), "Column 'cyl' is of type factor and cannot be scaled.")
  scaling_err$scaling[2] <- NA # removing cyl from variables to be scaled
  expect_equal(suppressWarnings(scale_continuous_predictors(data_err, scaling)),
               scale_continuous_predictors(data, scaling_err))

})
