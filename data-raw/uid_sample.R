# Generates uid_sample.csv for the uid package
# Run manually to update the sample dataset

library(dplyr)
library(lubridate)

set.seed(123)
start <- ymd_hms("2025-01-01 00:00:00")
N <- 24*60*3  # 1 sample every 20 seconds for 24 hours
step <- 20
rfids <- c("A1B2C3D4", "E5F6G7H8", "I9J0K1L2")

rows <- purrr::map_dfr(rfids, function(id) {
  times <- start + seq(0, by = step, length.out = N)
  temp <- 37 + rnorm(N, 0, 0.2)
  temp[100] <- 40
  temp[2000] <- 0
  temp[3000] <- 40
  temp[500:505] <- NA
  temp[1500:1519] <- NA
  temp[3500:3507] <- NA
  zone <- sample(1:8, N, replace = TRUE)
  tibble(
    Date = format(times, "%Y/%m/%d %H:%M:%S"),
    RFID = id,
    Zone = zone,
    `Matrix Name` = "matrix1",
    `Session Name` = "2025_01_01_00_00_00_thru_2025_01_02_00_00_00",
    Temperature = round(temp, 2)
  )
})

rows$Temperature[is.na(rows$Temperature)] <- ""

usethis::use_data_raw()
fs::dir_create("inst/extdata")
readr::write_csv(rows, "inst/extdata/uid_sample.csv")
