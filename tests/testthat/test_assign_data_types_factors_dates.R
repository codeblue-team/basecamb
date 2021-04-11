library(basecamb)

test_that("parse_date_columns assigns dates correctly", {
  data <- data.frame(date = rep('23/01/4567', 5),
                     date1 = rep('01/23/4567', 5),
                     date2 = rep('4567-01-23', 5),
                     date3 = rep('01/23/4567', 5))
  data <- parse_date_columns(data, list(date = '%d/%m/%Y',
                                        date1 = '%m/%d/%Y',
                                        date2 = '%Y-%m-%d',
                                        date3 = '%d/%m/%Y'))
  reference_date <- as.Date("4567-01-23", format = "%Y-%m-%d")
  expect_equal(data$date, rep(reference_date, 5))
  expect_equal(data$date1, rep(reference_date, 5))
  expect_equal(data$date2, rep(reference_date, 5))
  expect_equal(is.na(data$date3), rep(TRUE, 5))
  expect_equal(class(data$date3), "Date")
})
