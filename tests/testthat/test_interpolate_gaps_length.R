test_that("interpolate_gaps() interpolates only short gaps", {
  set.seed(42)

  # Create datetime index
  common_dt <- seq.POSIXt(
    from = as.POSIXct("2023-01-01 08:00:00"),
    by = "min",
    length.out = 120  # 2 hours
  )

  # Generate smooth temperature curve with noise
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
  df_A$temperature[21:26] <- NA     # gap of 6
  df_A$temperature[61:81] <- NA     # gap of 21
  df_B$temperature[11:13] <- NA     # gap of 3
  df_B$temperature[91:109] <- NA    # gap of 19

  df <- dplyr::bind_rows(df_A, df_B) |>
    dplyr::group_by(rfid, session_name, matrix_name)

  # Interpolate gaps with a cutoff of 10
  df_interp <- interpolate_gaps(df, max_gap = 10, target_cols = "temperature")

  # Check interpolation on short gaps
  df_interp_A <- dplyr::filter(df_interp, rfid == "A001")
  df_interp_B <- dplyr::filter(df_interp, rfid == "B002")

  short_gap_A <- df_interp_A |> dplyr::slice(21:26)
  long_gap_A  <- df_interp_A |> dplyr::slice(61:81)

  short_gap_B <- df_interp_B |> dplyr::slice(11:13)
  long_gap_B  <- df_interp_B |> dplyr::slice(91:109)

  expect_true(all(!is.na(short_gap_A$temperature)))
  expect_true(all(is.na(long_gap_A$temperature)))

  expect_true(all(!is.na(short_gap_B$temperature)))
  expect_true(all(is.na(long_gap_B$temperature)))
})
