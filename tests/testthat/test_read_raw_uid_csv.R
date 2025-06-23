library(testthat)

test_that("read_raw_uid_csv parses datetime and returns columns", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "Date,RFID,Temperature,Session Name,Matrix Name,Zone",
    "2025/05/24 11:15:25,ABC123,37.5,session1,matrix1,1"
  ), tmp)

  df <- read_raw_uid_csv(tmp)

  expect_true(all(c("datetime", "rfid", "temperature", "session_name", "matrix_name", "zone") %in% names(df)))
  expect_s3_class(df$datetime, "POSIXct")
  expect_equal(df$rfid[1], "ABC123")
})


test_that("read_raw_uid_csv errors on missing columns", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "Date,RFID,Temperature,Session Name,Matrix Name",
    "2025/05/24 11:15:25,ABC123,37.5,session1,matrix1"
  ), tmp)

  expect_error(read_raw_uid_csv(tmp), "Missing required")
})


test_that("read_raw_uid_csv errors on bad datetime", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "Date,RFID,Temperature,Session Name,Matrix Name,Zone",
    "bad-date,ABC123,37.5,session1,matrix1,1"
  ), tmp)

  expect_error(read_raw_uid_csv(tmp), "Failed to parse datetime")
})
