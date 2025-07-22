test_that("numeric sampling interval is returned as is", {
  df <- tibble::tibble(x = 1:3, rfid = "A") |>
    dplyr::group_by(rfid)

  out <- resolve_sampling_interval(df, 5)
  testthat::expect_equal(out, 5)
})

test_that("custom function is evaluated and result is returned", {
  df <- tibble::tibble(x = 1:3, rfid = "X") |>
    dplyr::group_by(rfid)

  custom_fun <- function(data) {
    testthat::expect_equal(nrow(data), 3)
    return(7.5) # Arbitrary valid output
  }

  out <- resolve_sampling_interval(df, custom_fun)
  testthat::expect_equal(out, 7.5)
})

test_that("default estimate_sampling_interval passes through cleanly", {
  df <- tibble::tibble(
    common_dt = as.POSIXct(c("2023-01-01 00:00:00", "2023-01-01 00:05:00")),
    rfid = "A",
    temperature = c(30, 31)
  ) |>
    dplyr::group_by(rfid)

  out <- resolve_sampling_interval(df, estimate_sampling_interval)
  testthat::expect_true(is.numeric(out) && length(out) == 1)
})

test_that("error is thrown for invalid sampling_interval input", {
  df <- tibble::tibble(x = 1:3, rfid = "B") |>
    dplyr::group_by(rfid)

  testthat::expect_error(
    resolve_sampling_interval(df, "not_valid"),
    "must be a numeric value or function"
  )
})

test_that("error is thrown for ungrouped input data", {
  df <- tibble::tibble(x = 1:3, rfid = "C") # ungrouped

  testthat::expect_error(
    resolve_sampling_interval(df, estimate_sampling_interval),
    "must be grouped"
  )
})
