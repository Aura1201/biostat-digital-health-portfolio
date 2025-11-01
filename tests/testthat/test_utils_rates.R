library(testthat)
source(file.path("..", "..", "R", "utils_rates.R"), local = TRUE)

test_that("rate works on typical inputs", {
  res <- inc_rate_ci(events = 10, pt_years = 50, mult = 100)
  expect_equal(ncol(res), 5)
  expect_equal(res$events, 10)
  expect_equal(res$pt_years, 50)
  expect_gt(res$rate, 0)
  expect_lt(res$lcl, res$ucl)
})

test_that("handles zero events safely", {
  res <- inc_rate_ci(events = 0, pt_years = 10, mult = 100)
  expect_equal(res$rate, 0)
  expect_gte(res$lcl, 0)
  expect_gt(res$ucl, 0)
})

test_that("rejects non-positive person-time", {
  expect_error(inc_rate_ci(1, 0))
  expect_error(inc_rate_ci(1, -5))
})
