#' Interpolate Short Gaps in UID Time Series Data
#'
#' Fills small gaps (NAs) in downsampled UID data using linear interpolation,
#' only for gaps that are no longer than a specified number of bins.
#' Assumes a regular time step (e.g., 1-minute bins) and operates within
#' grouping variables like `rfid` and `session_name`.
#'
#' @param df A downsampled data frame, ideally grouped by one or more identifiers.
#' @param max_gap Maximum number of consecutive missing bins to interpolate (default = 10).
#' @param target_cols Character vector of column names to interpolate.
#' @param add_flag Logical. If TRUE, adds a `.interpolated` column (default = FALSE).
#'
#' @return A data frame with interpolated values for short gaps.
#' @export
interpolate_gaps <- function(df, max_gap = 10, target_cols = c("temperature", "activity_index"), add_flag = FALSE) {
  # Check for grouping
  group_vars <- dplyr::group_vars(df)
  if (length(group_vars) == 0) {
    stop("Data is not grouped. Consider grouping by 'rfid' or another unit before interpolation.")
  }

  # Apply interpolation per group
  df_interp <- df |>
    dplyr::group_modify(~ interpolate_group(.x, max_gap, target_cols, add_flag)) |>
    dplyr::ungroup()

  return(df_interp)
}

# Internal: Linear interpolation of short gaps in a single vector
linear_interpolate_short_gaps <- function(x, max_gap) {
  is_na <- is.na(x)
  if (!any(is_na)) return(x)

  rle_na <- rle(is_na)
  lengths <- rle_na$lengths
  values <- rle_na$values

  ends <- cumsum(lengths)
  starts <- ends - lengths + 1

  for (i in which(values & lengths <= max_gap)) {
    s <- starts[i]
    e <- ends[i]

    # only interpolate if we have bounds
    if (s > 1 && e < length(x)) {
      left <- x[s - 1]
      right <- x[e + 1]
      x[s:e] <- seq(left, right, length.out = lengths[i] + 2)[-c(1, lengths[i] + 2)]
    }
  }

  return(x)
}

# Internal: Interpolates within a group (linear only for now)
interpolate_group <- function(data, max_gap, target_cols, add_flag) {
  data <- dplyr::arrange(data, common_dt)

  for (col in target_cols) {
    original <- data[[col]]
    interpolated <- linear_interpolate_short_gaps(original, max_gap)

    if (add_flag) {
      if (!".interpolated" %in% names(data)) data$.interpolated <- FALSE
      changed <- is.na(original) & !is.na(interpolated)
      data$.interpolated[changed] <- TRUE
    }

    data[[col]] <- interpolated
  }

  return(data)
}
