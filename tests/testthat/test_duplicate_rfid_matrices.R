test_that("resolve_duplicate_rfid_matrices removes low percentage duplicates after confirmation", {
  df <- tibble::tibble(
    session_name = c(rep("session1", 100), rep("session1", 5), rep("session2", 8)),
    rfid = c(rep("A001", 105), rep("B002", 8)),
    matrix_name = c(rep("matrix1", 100), rep("matrix2", 5), rep("matrix9", 8)),
    datetime = as.POSIXct("2025-05-24 11:15:25", tz = "UTC") + seq_len(113),
    temperature = 37,
    zone = 1
  )

  cleaned <- uid:::resolve_duplicate_rfid_matrices(df, user_response = 1)

  testthat::expect_equal(
    unique(cleaned$matrix_name[cleaned$rfid == "A001"]),
    "matrix1"
  )
  testthat::expect_equal(sum(cleaned$rfid == "A001"), 100)
  testthat::expect_equal(sum(cleaned$rfid == "B002"), 8)
})


test_that("resolve_duplicate_rfid_matrices aborts on substantial duplicate detections", {
  df <- tibble::tibble(
    session_name = "session1",
    rfid = "A001",
    matrix_name = c(rep("matrix1", 85), rep("matrix2", 15)),
    datetime = as.POSIXct("2025-05-24 11:15:25", tz = "UTC") + seq_len(100),
    temperature = 37,
    zone = 1
  )

  testthat::expect_error(
    uid:::resolve_duplicate_rfid_matrices(df, user_response = 1),
    "substantial number of measurements in more than one matrix"
  )
})


test_that("resolve_duplicate_rfid_matrices aborts on ties across matrices", {
  df <- tibble::tibble(
    session_name = "session1",
    rfid = "A001",
    matrix_name = c(rep("matrix1", 50), rep("matrix2", 50)),
    datetime = as.POSIXct("2025-05-24 11:15:25", tz = "UTC") + seq_len(100),
    temperature = 37,
    zone = 1
  )

  testthat::expect_error(
    uid:::resolve_duplicate_rfid_matrices(df, user_response = 1),
    "ambiguous dominant matrix"
  )
})
