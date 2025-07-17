# Generates uid_sample_data for the uid package
# Run manually to update the sample dataset

library(dplyr)
library(lubridate)

set.seed(123)
start <- ymd_hms("2025-01-01 00:00:00")
N <- 24 * 60 * 3 # 1 sample every 20 seconds for 24 hours
step <- 20
rfids <- c("A1B2C3D4", "E5F6G7H8", "I9J0K1L2")
nums <- seq(1, length(rfids))
names(rfids) <- rfids
mm <- paste0("MM", nums)
mm <- rep(mm, each = N)

rows <- purrr::map_dfr(rfids, function(id) {
  times <- start + seq(0, by = step, length.out = N)
  temp <- sample(c(35, 36, 37), size = 1) + rnorm(N, 0, 0.2)

  ## Add spike artifacts independently per RFID
  spiking_points <- sample(N, size = 7)
  spiking_vals <- sample(c(0, 40), size = 7, replace = TRUE)
  temp[spiking_points] <- spiking_vals

  ## Add NAs in random blocks per RFID
  na_block1 <- sample(1:(N - 10), 1)
  na_block2 <- sample(1:(N - 20), 1)
  na_block3 <- sample(1:(N - 10), 1)
  temp[na_block1:(na_block1 + 8)] <- NA
  temp[na_block2:(na_block2 + 20)] <- NA
  temp[na_block3:(na_block3 + 7)] <- NA

  zone <- sample(1:8, N, replace = TRUE)

  tibble(
    datetime = as_datetime(format(times, "%Y/%m/%d %H:%M:%S")),
    rfid = id,
    zone = zone,
    session_name = "sample_session",
    temperature = round(temp, 2)
  )
})

uid_sample_data <- mutate(rows, matrix_name = mm)

usethis::use_data(uid_sample_data, overwrite = T)
