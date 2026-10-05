test_that("Date must exist", {

  tse_test <- tse_example

  SummarizedExperiment::colData(tse_test)$Date <- NULL

  expect_error(
    .get_dive_dates(tse_test),
    "Date"
  )
})


test_that("uneven sampling is detected", {

  dates <- as.Date(
    c(
      "2025-01-01",
      "2025-01-20",
      "2025-02-14"
    )
  )

  info <- .inspect_sampling(
    dates,
    sampling_even = 0
  )

  expect_false(
    info$observed_even
  )
})


test_that("lag-specific pair count decreases correctly", {

  alpha <- data.frame(
    shannon = 1:10
  )

  functionality <- 11:20

  result <- .calculate_ccf_table(
    alpha,
    functionality,
    max_lag = 2,
    sampling_interval_days = 21
  )

  expect_equal(
    result$n_pairs[result$lag == 0],
    10
  )

  expect_equal(
    result$n_pairs[result$lag == 1],
    9
  )

  expect_equal(
    result$n_pairs[result$lag == 2],
    8
  )
})
