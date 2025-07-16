library(dplyr)

test_that("estimate_sampling_interval returns shared interval", {
  df <- tibble(
    id = rep(c("A", "B"), each = 4),
    common_dt = c(
      seq.POSIXt(as.POSIXct("2024-01-01 00:00:00"), by = "min", length.out = 4),
      seq.POSIXt(as.POSIXct("2024-01-01 00:00:00"), by = "min", length.out = 4)
    )
  ) %>%
    group_by(id)

  expect_equal(estimate_sampling_interval(df), 1)
})

test_that("estimate_sampling_interval errors with irregular intervals", {
  df <- tibble(
    id = "A",
    common_dt = as.POSIXct(c(
      "2024-01-01 00:00:00",
      "2024-01-01 00:01:00",
      "2024-01-01 00:03:00"
    ))
  ) %>%
    group_by(id)

  expect_error(estimate_sampling_interval(df))
})

test_that("estimate_sampling_interval errors when groups differ", {
  df <- tibble(
    id = rep(c("A", "B"), each = 3),
    common_dt = c(
      as.POSIXct(c(
        "2024-01-01 00:00:00",
        "2024-01-01 00:01:00",
        "2024-01-01 00:02:00"
      )),
      as.POSIXct(c(
        "2024-01-01 00:00:00",
        "2024-01-01 00:02:00",
        "2024-01-01 00:04:00"
      ))
    )
  ) %>%
    group_by(id)

  expect_error(estimate_sampling_interval(df))
})
