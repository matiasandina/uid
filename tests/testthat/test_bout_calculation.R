rfids <- c("RFID_A1", "RFID_B2", "RFID_C3", "RFID_C4")
n <- 10

df <- tidyr::expand_grid(
  rfid = rfids,
  common_dt = seq.POSIXt(
    as.POSIXct("2025-01-01 00:00:00"),
    by = "1 min",
    length.out = n
  )
) |>
  dplyr::mutate(
    session_name = "sample_session",
    variable = "temperature",
    value = c(
      37,
      34,
      34,
      37,
      37,
      34,
      37,
      37,
      34,
      34,
      37,
      NA,
      34,
      34,
      37,
      37,
      NA,
      NA,
      37,
      34,
      rep(34, 10),
      rep(37, 10)
    )
  )


test_that("duration_mode = 'strict' produces 0 for single-point bouts", {
  strict_result <- df |>
    dplyr::group_by(rfid) |>
    quantify_temp_bouts(
      threshold = 35,
      sampling_interval = 1,
      duration_mode = "strict"
    )

  # Check that RFID_A1 has 0-min bouts (from isolated points)
  expect_true(any(strict_result$rfid == "RFID_A1" & strict_result$duration_minutes == 0))
})

test_that("duration_mode = 'inclusive' adds sampling_interval", {
  inclusive_result <- df |>
    dplyr::group_by(rfid) |>
    quantify_temp_bouts(
      threshold = 35,
      sampling_interval = 1,
      duration_mode = "inclusive"
    )

  strict_result <- df |>
    dplyr::group_by(rfid) |>
    quantify_temp_bouts(
      threshold = 35,
      sampling_interval = 1,
      duration_mode = "strict"
    )

  merged <- dplyr::left_join(
    strict_result,
    inclusive_result,
    by = c("rfid", "start", "end"),
    suffix = c("_strict", "_inclusive")
  )

  expect_true(all(merged$duration_minutes_inclusive >= merged$duration_minutes_strict))
})

test_that("drop_single_point_bouts removes zero-duration bouts only", {
  no_singletons <- df |>
    dplyr::group_by(rfid) |>
    quantify_temp_bouts(
      threshold = 35,
      sampling_interval = 1,
      duration_mode = "strict",
      drop_single_point_bouts = TRUE
    )

  # remove the true zeros
  duration_single_bouts <- no_singletons |> dplyr::filter(!is.na(start)) |> dplyr::pull(duration_minutes)
  duration_true_zeros <- no_singletons |> dplyr::filter(is.na(start)) |> dplyr::pull(duration_minutes)

  expect_false(any(duration_single_bouts == 0))
  expect_true(any(duration_true_zeros == 0))
})

test_that("fill_undetected_groups = FALSE omits non-bouting animals", {
  result <- df |>
    dplyr::group_by(rfid) |>
    quantify_temp_bouts(
      threshold = 30,  # All below threshold → all qualify
      fill_undetected_groups = FALSE
    )

  expect_equal(nrow(result), 0)

})

test_that("fill_undetected_groups = TRUE fills with duration = 0", {
  result <- df |>
    dplyr::group_by(rfid) |>
    quantify_temp_bouts(
      threshold = 20,  # No value below this
      fill_undetected_groups = TRUE
    )

  expect_equal(nrow(result), length(unique(df$rfid)))
  expect_true(all(result$duration_minutes == 0))
})
