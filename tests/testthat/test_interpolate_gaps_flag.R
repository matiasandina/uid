test_that("interpolate_gaps() flags interpolated values", {
  set.seed(42)

  # Create datetime index
  common_dt <- seq.POSIXt(
    from = as.POSIXct("2023-01-01 08:00:00"),
    by = "min",
    length.out = 120
  )

  make_temp <- function(seed) {
    set.seed(seed)
    base <- 37
    trend <- 0.1 * sin(seq(0, 3 * pi, length.out = 120))
    noise <- rnorm(120, mean = 0, sd = 0.1)
    base + trend + noise
  }

  df_A <- tibble::tibble(
    rfid = "A001",
    common_dt = common_dt,
    temperature = make_temp(1),
    session_name = "session1",
    matrix_name = "matrix1"
  )
  df_B <- tibble::tibble(
    rfid = "B002",
    common_dt = common_dt,
    temperature = make_temp(2),
    session_name = "session1",
    matrix_name = "matrix1"
  )

  # Inject gaps
  df_A$temperature[21:26] <- NA
  df_A$temperature[61:81] <- NA
  df_B$temperature[11:13] <- NA
  df_B$temperature[91:109] <- NA

  df <- dplyr::bind_rows(df_A, df_B) |>
    dplyr::group_by(rfid, session_name, matrix_name)

  df_interp <- interpolate_gaps(df, max_gap = 10, target_cols = "temperature", add_flag = TRUE)

  expected_flag <- is.na(df$temperature) & !is.na(df_interp$temperature)
  expect_identical(df_interp$.interpolated, expected_flag)

  short_gap_A <- df_interp |> dplyr::filter(rfid == "A001") |> dplyr::slice(21:26)
  long_gap_A  <- df_interp |> dplyr::filter(rfid == "A001") |> dplyr::slice(61:81)

  short_gap_B <- df_interp |> dplyr::filter(rfid == "B002") |> dplyr::slice(11:13)
  long_gap_B  <- df_interp |> dplyr::filter(rfid == "B002") |> dplyr::slice(91:109)

  expect_true(all(short_gap_A$.interpolated))
  expect_true(all(!long_gap_A$.interpolated))
  expect_true(all(short_gap_B$.interpolated))
  expect_true(all(!long_gap_B$.interpolated))
})
