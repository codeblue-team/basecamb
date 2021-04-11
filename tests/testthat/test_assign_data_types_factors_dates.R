library(basecamb)

test_that("parse_date_columns assigns dates correctly", {
  data <- data.frame(date = rep('23/01/4567', 5),
                     date1 = rep('01/23/4567', 5),
                     date2 = rep('4567-01-23', 5),
                     date3 = rep('01/23/4567', 5)
                     )
  data <- parse_date_columns(data, list(date = '%d/%m/%Y',
                                        date1 = '%m/%d/%Y',
                                        date2 = '%Y-%m-%d',
                                        date3 = '%d/%m/%Y'
                                        )
                             )
  reference_date <- as.Date("4567-01-23", format = "%Y-%m-%d")
  expect_equal(data$date, rep(reference_date, 5))
  expect_equal(data$date1, rep(reference_date, 5))
  expect_equal(data$date2, rep(reference_date, 5))
  expect_equal(is.na(data$date3), rep(TRUE, 5))
  expect_equal(class(data$date3), "Date")
})


test_that("assign_factorial_levels assigns factor levels correctly", {
  data <- data.frame(col = as.factor(rep(c('1', '2', '4'), 5)),
                     col1 = as.factor(rep(c('1', '2', '4'), 5)),
                     col2 = as.factor(rep(c('1', '2', '4'), 5)),
                     col3 = as.factor(rep(c('1', '2', '4'), 5)),
                     col4 = rep(c('1', '2', '4'), 5)
                     )
  data <- assign_factorial_levels(data, list(col = c('1' = 'One', '2' = 'Two', '4' = 'Four'),
                                             col1 = c('1' = 'One', '2' = 'Two'),
                                             col2 = c('1' = 'One', 'default' = 'Not_One'),
                                             col3 = c('1' = 'One', 'default' = NA)
                                             )
                                  )
  expect_equal(data$col, as.factor(rep(c('One', 'Two', 'Four'), 5)))
  expect_equal(data$col1, as.factor(rep(c('One', 'Two', '4'), 5)))
  expect_equal(data$col2, as.factor(rep(c('One', 'Not_One', 'Not_One'), 5)))
  expect_equal(data$col3, as.factor(rep(c('One', NA, NA), 5)))
  expect_error(assign_factorial_levels(data, list(col4 = c('1' = 'One'))),
               "attempting to coerce non-factor")
})


test_that("assign_types_names assigns correct data types", {
  # Generating a dataset with missing values
  data <- datasets::mtcars
  data$date <- "4567-01-23"
  data_dict <- data.frame(list("old_column_name" = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "date"),
                               "new_data_type" = c("float", "integer", "float", "float", "float", "float", "float", "factor", "factor", "integer", "integer", "date"),
                               "new_column_name" = c("miles_per_gallon", "cylinders", "displacement", "horsepower",
                                                     "rear_axle_ratio", "weight_lbs", "quarter_mile_time_secs",
                                                     "engine_type", "transmission_type", "num_gears", "num_carburetors", "date")
                               )
                          )
  expect_equal(names(assign_types_names(data=data, meta_data = data_dict)),
               data_dict$new_column_name)
  expect_true(is.integer(assign_types_names(data=data, meta_data = data_dict)[['cylinders']]))
  expect_true(is.double(assign_types_names(data=data, meta_data = data_dict)[['horsepower']]))
  expect_true(is.factor(assign_types_names(data=data, meta_data = data_dict)[['transmission_type']]))
  expect_equal(class(assign_types_names(data=data, meta_data = data_dict)[['date']]), 'Date')
  expect_equal(assign_types_names(data=data, meta_data = data_dict)[['date']], rep(as.Date('4567-01-23'), 32))
  data_dict$new_data_type[1] <- "factor"
  expect_warning(assign_types_names(data=data, meta_data = data_dict), "In column mpg, created a factor with 25 levels")
})


test_that("apply_data_dictionary assigns correct values", {
  # Generating a dataset with missing values
  data <- datasets::mtcars
  data$date <- "4567-01-23"
  data$vs[c(2,4,8,16)] <- NA
  data$am[c(1,3,9,27)] <- NA
  data_dict <- data.frame(list("old_column_name" = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "date"),
                               "new_data_type" = c("float", "integer", "float", "float", "float", "float", "float", "factor", "factor", "integer", "integer", "date"),
                               "new_column_name" = c("miles_per_gallon", "cylinders", "displacement", "horsepower",
                                                     "rear_axle_ratio", "weight_lbs", "quarter_mile_time_secs",
                                                     "engine_type", "transmission_type", "num_gears", "num_carburetors", "date"),
                               "coding" = c(NA, NA, NA, NA, NA, NA, NA,
                                            "'0' = 'V-shaped', '1' = 'straight', 'default' = NA",
                                            "'0' = 'automatic', '1' = 'manual', 'default' = Unknown",
                                            NA, NA, "%m/%d/%Y")
                               )
                          )
  data <- apply_data_dictionary(data = data, data_dictionary = data_dict)
  expect_equal(names(data), data_dict$new_column_name)
  expect_true(is.integer(data[['cylinders']]))
  expect_true(is.double(data[['horsepower']]))
  expect_true(is.factor(data[['transmission_type']]))
  expect_equal(class(data[['date']]), 'Date')
  expect_equal(data[['date']], rep(as.Date('4567-01-23'), 32))
  expect_equal(levels(data$engine_type), c("V-shaped", "straight"))
  expect_equal(levels(data$transmission_type), c("Unknown", "automatic", "manual"))
  expect_equal(sum(is.na(data$engine_type)), 4)
})
