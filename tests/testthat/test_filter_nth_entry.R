library(basecamb)

test_that("filter_nth_entry filters appointments correctly", {
  data <- data.frame(list(ID = rep(1:5, 3),
                          encounter = rep(1:3, each=5),
                          date = rep(as.Date(c('4567-01-23', '4567-01-25', '4567-01-22')), each=5),
                          value = rep(4:6, each=5)
                          )
                     )

  expect_equal(filter_nth_entry(data, 'ID', 'encounter')[['value']], rep(4, 5))
  expect_equal(filter_nth_entry(data, 'ID', 'encounter', n = 2)[['value']], rep(5, 5))
  expect_equal(filter_nth_entry(data, 'ID', 'encounter', reverse_order = TRUE)[['value']], rep(6, 5))
  expect_equal(filter_nth_entry(data, 'ID', 'date')[['value']], rep(6, 5))
})

