toy_data <- tibble::tibble(
  rfid = "X1",
  common_dt = seq(
    lubridate::ymd_hms("2025-01-01 00:00:00"),
    by = "min",
    length.out = 20
  ),
  variable = "temperature",
  value = c(
    34,
    34,
    33,
    32,
    31,
    NA,
    30,
    29,
    NA,
    35,
    36,
    34,
    32,
    31,
    30,
    NA,
    NA,
    29,
    28,
    27
  )
)

#quantify_temp_bouts(toy_data, threshold = 33, greedy = FALSE)
#quantify_temp_bouts(toy_data, threshold = 33, greedy = TRUE, max_gap = 0)
