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
                     col4 = rep(c('1', '2', '4'), 5),
                     col5 = as.factor(rep(c('1', '2', NA), 5)),
                     col6 = as.factor(rep(c('1', '2', NA), 5)),
                     col7 = as.factor(rep(c('1', '2', NA), 5))
                     )
  data1 <- assign_factorial_levels(data, list(col = c('1' = 'One', '2' = 'Two', '4' = 'Four'),
                                             col1 = c('1' = 'One', '2' = 'Two'),
                                             col2 = c('1' = 'One', 'default' = 'Not_One'),
                                             col3 = c('1' = 'One', 'default' = NA),
                                             col5 = c('1' = 'One', 'default' = 'Not_One'),
                                             col6 = c('1' = 'One', 'default' = 'Not_One', 'NA' = 'N/A')
                                             )
                                  )
  data1 <- assign_factorial_levels(data1, list(col7 = c('1' = 'One', 'default' = 'Not_One')),
                                  na_action_default = 'assign_default')

  expect_equal(data1$col, as.factor(rep(c('One', 'Two', 'Four'), 5)))
  expect_equal(data1$col1, as.factor(rep(c('One', 'Two', '4'), 5)))
  expect_equal(data1$col2, as.factor(rep(c('One', 'Not_One', 'Not_One'), 5)))
  expect_equal(data1$col3, as.factor(rep(c('One', NA, NA), 5)))
  expect_equal(data1$col5, as.factor(rep(c('One', 'Not_One', NA), 5)))
  expect_equal(data1$col6, as.factor(rep(c('One', 'Not_One', 'N/A'), 5)))
  expect_equal(data1$col7, as.factor(rep(c('One', 'Not_One', 'Not_One'), 5)))
  expect_error(assign_factorial_levels(data, list(col4 = c('1' = 'One'))),
               "attempting to coerce non-factor")
  expect_error(assign_factorial_levels(data, list(col5 = c('1' = 'One')), na_action_default = 'assign_default'),
               "In column col5: Selected \"assign_default\" as na_action_default without specifying default level.")
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
  data$date1 <- "01/23/4567"
  data$date2 <- "4567-01-23"
  data$vs[c(2,4,8,16)] <- NA
  data$am[c(1,3,9,27)] <- NA
  data_copy <- data
  data_dict <- data.frame(list("old_column_name" = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs",
                                                     "am", "gear", "carb", "date", "date1", "date2"),
                               "new_data_type" = c("float", "integer", "float", "float", "float", "float", "float",
                                                   "factor", "factor", "factor", "integer", "date", "date", "date"),
                               "new_column_name" = c("miles_per_gallon", "cylinders", "displacement", "horsepower",
                                                     "rear_axle_ratio", "weight_lbs", "quarter_mile_time_secs",
                                                     "engine_type", "transmission_type", "num_gears",
                                                     "num_carburetors", "date", "date1", "date2"),
                               "coding" = c(NA, NA, NA, NA, NA, NA, NA,
                                            "'0' = 'V-shaped', '1' = 'straight', 'default' = NA",
                                            "'0' = 'automatic', '1' = 'manual', 'NA' = Unknown",
                                            "'3' = 'Three'",
                                            NA, "%Y-%m-%d", "%m/%d/%Y", "%m-%d-%Y")
                               )
                          )
  data <- apply_data_dictionary(data = data, data_dictionary = data_dict, print_coerced_NA = FALSE)
  expect_equal(names(data), data_dict$new_column_name)
  expect_true(is.integer(data[['cylinders']]))
  expect_true(is.double(data[['horsepower']]))
  expect_true(is.factor(data[['transmission_type']]))
  expect_equal(class(data[['date']]), 'Date')
  expect_equal(data[['date']], rep(as.Date('4567-01-23'), 32))
  expect_equal(data[['date1']], rep(as.Date('4567-01-23'), 32))
  expect_equal(data[['date2']], as.Date(rep(NA, 32)))
  expect_equal(levels(data$engine_type), c("V-shaped", "straight"))
  expect_equal(levels(data$transmission_type), c("Unknown", "automatic", "manual"))
  expect_equal(sum(is.na(data$engine_type)), 4)
  expect_error(apply_data_dictionary(data = data_copy, data_dictionary = data_dict, na_action_default = 'assign_default'),
               "In column num_gears: Selected \"assign_default\" as na_action_default without specifying default level.")
  data_dict$coding[14] <- NA
  expect_warning(apply_data_dictionary(data = data_copy, data_dictionary = data_dict),
                 "No date format specified for column date2. Using %Y-%m-%d.")

  # Test whether the call to `.find_NA_coercions()` works properly and that NA's are found
  data_dict$coding[14] <- "%Y-%m-%d"
  data_dict$new_data_type[12] <- "integer"
  # test whether custom message is returned
  expect_message(object = suppressWarnings(apply_data_dictionary(data = data_copy, data_dictionary = data_dict, print_coerced_NA = TRUE)),
                 regexp = "In the following rows and columns, values have been coerced to NA's")
  # test whether generic warning is returned
  expect_warning(object = apply_data_dictionary(data = data_copy, data_dictionary = data_dict, print_coerced_NA = FALSE),
                 regexp = "NAs introduced by coercion")

  # Test whether the call to `.find_NA_coercions()` works properly when no NA's have been introduced
  ## set the "date2" column coding to something that will not introduce NA's
  data_dict$coding[14] <- "%Y-%m-%d"
  data_dict$new_data_type[12] <- "date"
  ## check for apply_data_dictionary and specifically the call to .find_NA_coercoins() to NOT return a message
  expect_message(object = apply_data_dictionary(data = data_copy, data_dictionary = data_dict, print_coerced_NA = TRUE),
                 regexp = NA)


})


testthat::test_that(".find_NA_coercions finds introduced NA's.", {

  # create input
  data_raw <- data.frame(a_char = c("1", "2", "a", "b"),
                         b_char = c("1970-01-01", "1970-11-05", "1970-5-22", "1970-99-01"))

  # create output (based on data_dictionary below)
  data <- data.frame(a_numeric = c(1, 2, NA, NA),
                     b_date = as.Date(c("1970-01-01", "1970-11-05", "1970-5-22", NA), format = "%Y-%m-%d"))

  # create data dictionary, reflecting difference between data_raw and data
  data_dictionary <- data.frame(old_column_name = colnames(data_raw),
                                new_data_type = c("float", "date"),
                                new_column_name = colnames(data),
                                coding = NA)

  # find NA coercion when casting character to integer
  testthat::expect_equal(object = .find_NA_coercions(data_raw = data_raw,
                                                     data = data,
                                                     data_dictionary = data_dictionary)[1:2, ],
                         expected = data.frame(column = c("a_char", "a_char"),
                                               row = c(3, 4),
                                               value = c("a", "b"),
                                               row.names = c(1L, 2L)))
  # find NA coercion when casting character to date
  testthat::expect_equal(object = .find_NA_coercions(data_raw = data_raw,
                                                     data = data,
                                                     data_dictionary = data_dictionary)[3, ],
                         expected = data.frame(column = "b_char",
                                               row = 4,
                                               value = "1970-99-01",
                                               row.names = 3L))
  # work with not finding any NA coercion
  ## create a dataframe "data" that is the same as "data_raw"
  data <- data.frame(a_numeric = c("1", "2", "a", "b"),
                     b_date = c("1970-01-01", "1970-11-05", "1970-5-22", "1970-99-01"))
  # run test, for which .find_NA_coercions should return an empty dataframe:
  testthat::expect_equal(object = .find_NA_coercions(data_raw = data_raw,
                                                     data = data,
                                                     data_dictionary = data_dictionary),
                         expected = data.frame())
})
