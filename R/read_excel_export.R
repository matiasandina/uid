#Read All Sheets in Excel
# This makes use of readxl::cell_limits() to grab the data we need
# from the nasty excel output organization from UID
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) {
    readxl::read_excel(
      filename,
      sheet = X,
      range = readxl::cell_limits(c(15, 1), c(NA, 8))
    )
  })
  if (!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


read_excel_datasheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  # remove summary from sheets
  sheets <- stringr::str_subset(sheets, pattern = "Summary", negate = TRUE)
  x <- lapply(sheets, function(X) {
    readxl::read_excel(
      filename,
      sheet = X,
      range = readxl::cell_limits(c(15, 1), c(NA, 8))
    )
  })
  if (!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# To be used with the previous functions that return a list()
# TODO: user should be given some examples so that it makes sense for user
# this function in principle is not mandatory if user wants to keep a list
# Function to add session name as a column and flatten
flatten_with_session_names <- function(nested_list) {
  # Adding session names to each dataframe
  # TODO: defensive here, check for named list input
  named_list <- purrr::imap(nested_list, ~ map(.x, mutate, session_name = .y))
  # Flattening the list of lists to a single list of data frames
  flat_list <- purrr::list_flatten(named_list)
  # Binding all data frames into one
  combined_df <- dplyr::bind_rows(flat_list)
  return(combined_df)
}


# These are some helpers to read the specific excel export and parse it
# need some testing and troubleshooting likely
extract_uid_dates <- function(filepaths) {
  # Regular expression to match the pattern
  pattern <- "\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_thru_\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}"
  # Extract matches using stringr's str_extract
  dates <- stringr::str_extract(filepaths, pattern)
  return(dates)
}

parse_uid_session_dt <- function(df) {
  df |>
    tidyr::separate(
      session_name,
      into = c("session_start", "session_end"),
      sep = "_thru_"
    ) |>
    dplyr::mutate(
      session_start = lubridate::ymd_hms(session_start),
      session_end = lubridate::ymd_hms(session_end)
    )
}
