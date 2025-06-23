test_that("extract_base_name strips UID suffix", {
  expect_equal(extract_base_name("exp_1_of_3.CSV"), "exp")
  expect_equal(
    extract_base_name(file.path("/tmp", "exp_2_of_3.CSV")),
    "exp"
  )

  expect_equal(
    extract_base_name("session_2024_01_01_99_10_of_12.CSV"),
    "session_2024_01_01_99"
  )
})

test_that("extract_base_name does not match lowercase extension", {
  expect_equal(extract_base_name("exp_1_of_3.csv"), "exp_1_of_3.csv")
})

test_that("find_raw_export_files only returns CSV files", {
  tmp <- withr::local_tempdir()
  files <- file.path(tmp, c("a.CSV", "b.csv", "c.CSV", "d.txt"))
  file.create(files)

  csv_files <- find_raw_export_files(tmp)

  expect_equal(sort(basename(csv_files)), sort(c("a.CSV", "c.CSV")))
  expect_false(any(grepl("b.csv", csv_files, fixed = TRUE)))
})
