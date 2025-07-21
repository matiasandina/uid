fill_bouts_greedy <- function(cond, max_gap = 2) {
  # cond: logical vector with NAs
  n <- length(cond)
  bout <- rep(FALSE, n)
  i <- 1
  while (i <= n) {
    if (!isTRUE(cond[i])) {
      i <- i + 1
      next
    }
    # Start a bout
    start <- i
    i <- i + 1
    na_count <- 0
    while (
      i <= n && (isTRUE(cond[i]) || (is.na(cond[i]) && na_count < max_gap))
    ) {
      if (is.na(cond[i])) na_count <- na_count + 1 else na_count <- 0
      i <- i + 1
    }
    bout[start:(i - 1)] <- TRUE
  }
  bout
}

validate_bout_input <- function(df) {
  required_cols <- c("rfid", "common_dt", "variable", "value")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing required column(s): {missing_cols}")
  }
  if (!all(df$variable == "temperature")) {
    cli::cli_abort("The 'variable' column must be entirely 'temperature'.")
  }
}

#' Estimate Sampling Interval from Grouped Data
#'
#' Determines the time difference in minutes between consecutive `common_dt`
#' observations for each group in `df`. All groups must have a single unique
#' interval and share the same value.
#'
#' @param df A grouped data frame containing a `common_dt` column.
#'
#' @return Numeric sampling interval in minutes.
estimate_sampling_interval <- function(df) {
  group_vars <- dplyr::group_vars(df)
  if (length(group_vars) == 0) {
    cli::cli_abort(
      "`df` must be grouped before estimating the sampling interval."
    )
  }

  intervals <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::arrange(common_dt, .by_group = TRUE) |>
    dplyr::summarise(
      interval = list(unique(as.numeric(diff(common_dt), units = "mins"))),
      .groups = "drop"
    )

  bad_groups <- vapply(intervals$interval, length, integer(1)) != 1
  if (any(bad_groups)) {
    cli::cli_abort("Multiple sampling intervals detected in some groups.")
  }

  interval_vals <- vapply(intervals$interval, `[`, numeric(1), 1)
  if (length(unique(interval_vals)) != 1) {
    cli::cli_abort("Groups have different sampling intervals.")
  }

  unique(interval_vals)
}


#' Quantify bouts of temperature above or below a threshold
#'
#' @param df A data frame with at least columns: `rfid`, `common_dt`, `variable`, and `value`
#' @param direction Either "above" or "below"
#' @param sampling_interval A numeric value representing the sampling interval in minutes for all groups. Defaults to automatic estimation using [estimate_sampling_interval()].
#' @param threshold Numeric threshold to compare temperature values against
#' @param greedy Logical. If TRUE, allow bouts to continue through NAs. Default is FALSE.
#' @param max_gap Numeric. Maximum number of consecutive NA values allowed within a bout.
#'   Note: bouts are still broken by non-NA values that fail the threshold condition.
#' @return A data frame with one row per bout per animal, with start/end/duration
#' @export
quantify_temp_bouts <- function(
  df,
  threshold,
  sampling_interval = estimate_sampling_interval,
  direction = "below",
  greedy = FALSE,
  max_gap = 10
) {
  validate_bout_input(df)
  # helper
  cond_fun <- if (direction == "below") {
    function(x) x <= threshold
  } else {
    function(x) x >= threshold
  }

  # Save original grouping
  original_groups <- dplyr::group_vars(df)
  # Get original group combinations before filtering
  grouping_keys <- df |>
    dplyr::select(dplyr::all_of(original_groups), rfid) |>
    dplyr::distinct()

  # Estimate sampling frequency if needed
  sampling_interval <- switch(
    TRUE,
    identical(sampling_interval, estimate_sampling_interval) ~
      estimate_sampling_interval(df),
    is.numeric(sampling_interval) && length(sampling_interval) == 1 ~
      as.numeric(sampling_interval),
    cli::cli_abort(
      "`sampling_interval` must be a numeric value in minutes or `estimate_sampling_interval()`"
    )
  )

  df <- df |>
    dplyr::group_by(rfid, .add = TRUE) |>
    dplyr::arrange(common_dt, .by_group = TRUE) |>
    dplyr::mutate(cond_bool = cond_fun(value))

  if (!greedy) {
    df <- df |>
      dplyr::mutate(run_id = vctrs::vec_identify_runs(cond_bool)) |>
      dplyr::filter(cond_bool)
  } else {
    cli::cli_warn(
      "Using greedy = TRUE: allowing NA gaps within bouts (up to `max_gap` = {max_gap})."
    )

    df <- df |>
      dplyr::group_by(rfid, .add = TRUE) |>
      dplyr::mutate(
        cond_filled = fill_bouts_greedy(cond_bool, max_gap = max_gap),
        run_id = dplyr::if_else(
          cond_filled,
          vctrs::vec_identify_runs(cond_filled),
          NA_integer_
        )
      ) |>
      dplyr::filter(!is.na(run_id))
  }

  # Restore original grouping
  if (length(original_groups) > 0) {
    df <- df |> dplyr::group_by(dplyr::across(dplyr::all_of(original_groups)))
  }

  # Compute bout summary (still grouped by rfid + run_id)
  bouts_summary <- df |>
    dplyr::group_by(rfid, run_id, .add = TRUE) |>
    dplyr::summarise(
      start = min(common_dt),
      end = max(common_dt),
      duration_minutes = as.numeric(difftime(end, start, units = "mins")) +
        sampling_interval,
    )

  bouts_summary <- grouping_keys |>
    dplyr::full_join(bouts_summary, by = c(original_groups, "rfid")) |>
    tidyr::complete(
      fill = list(duration_minutes = 0)
    ) |>
    dplyr::select(-run_id)

  return(bouts_summary)
}
