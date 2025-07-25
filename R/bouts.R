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


#' Resolve sampling interval specification
#'
#' Internal utility to resolve the sampling interval for temperature data. Accepts either:
#' - a single numeric value (assumed to be in minutes)
#' - a function that returns a numeric value when applied to the input data
#'
#' Used internally by [quantify_temp_bouts()] to support flexible user input.
#'
#' @param df A data frame used as input to the function if `sampling_interval` is a function.
#' @param sampling_interval A numeric value (in minutes) or a function taking `df` and returning a numeric value.
#'
#' @return A numeric sampling interval in minutes.
#' @keywords internal
resolve_sampling_interval <- function(df, sampling_interval) {
  if (is.function(sampling_interval)) {
    if (!identical(sampling_interval, estimate_sampling_interval)) {
      cli::cli_alert_warning("Using custom sampling interval function.")
    }
    return(sampling_interval(df))
  } else if (is.numeric(sampling_interval) && length(sampling_interval) == 1) {
    return(as.numeric(sampling_interval))
  } else {
    cli::cli_abort("`sampling_interval` must be a numeric value or function.")
  }
}

#' Compute bout durations
#'
#' @param df A grouped df with `start` and `end` timestamps per bout.
#' @param sampling_interval Numeric value representing sampling interval in minutes.
#' @param duration_mode Either "strict" (end - start) or "inclusive" (end - start + sampling_interval).
#'
#' @return A data frame with a `duration_minutes` column.
#' @keywords internal
compute_bout_durations <- function(df, sampling_interval, duration_mode = c("strict", "inclusive")) {
  duration_mode <- rlang::arg_match(duration_mode)
  df <- dplyr::mutate(
    df,
    duration_minutes = as.numeric(difftime(end, start, units = "mins"))
  )
  if (duration_mode == "inclusive") {
    df <- dplyr::mutate(df, duration_minutes = duration_minutes + sampling_interval)
  }
  df
}

#' Quantify bouts of temperature above or below a threshold
#'
#' Identifies contiguous periods ("bouts") where temperature is consistently above or below
#' a user-defined threshold, allowing optional gap-tolerant detection and customizable
#' duration calculation.
#'
#' @param df A grouped data frame with at least columns: `rfid`, `common_dt`, `variable`, and `value`.
#'   Must be grouped (e.g., by `rfid`) prior to calling this function.
#' @param threshold Numeric threshold to compare against temperature values.
#' @param sampling_interval A numeric value representing the sampling interval in minutes,
#'   or a function that takes `df` and returns a single numeric value (e.g., [estimate_sampling_interval()]).
#' @param direction Either `"below"` or `"above"` to determine whether to identify values
#'   less than or equal to (`"below"`) or greater than or equal to (`"above"`) the threshold.
#' @param greedy Logical. If `TRUE`, allows `max_gap` NAs within a bout. If `FALSE`, bouts are broken by any NA or threshold-violating value.
#' @param max_gap Maximum number of consecutive `NA` values allowed within a bout when `greedy = TRUE`.
#' @param duration_mode How to compute bout duration:
#'   - `"strict"`: calculates `end - start`
#'   - `"inclusive"`: calculates `end - start + sampling_interval`
#' @param drop_single_point_bouts If `TRUE`, removes bouts consisting of only one time point
#'   (i.e., duration = 0 in `"strict"` mode). Default is `FALSE`.
#' @param fill_undetected_groups If `TRUE` (default), ensures that all groups from the input are
#'   represented in the output, even if no bouts were found, with `duration_minutes = 0`.
#'
#' @return A data frame with one row per detected bout per animal. Columns include:
#'   - `start`, `end`: timestamps of each bout
#'   - `duration_minutes`: duration of each bout in minutes
#'
#' @export
quantify_temp_bouts <- function(
  df,
  threshold,
  sampling_interval = estimate_sampling_interval,
  direction = c("below", "above"),
  greedy = FALSE,
  max_gap = 10,
  duration_mode = c("strict", "inclusive"),
  drop_single_point_bouts = FALSE,
  fill_undetected_groups = TRUE
) {
  validate_bout_input(df)
  direction <- rlang::arg_match(direction)
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

  # Resolve sampling interval for bout calculations
  sampling_interval <- resolve_sampling_interval(df, sampling_interval)

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
      .groups = "drop"
    ) |>
    compute_bout_durations(sampling_interval, duration_mode)

  if (drop_single_point_bouts) {
    bouts_summary <- bouts_summary |> dplyr::filter(duration_minutes > 0)
  }

  # Return true zeros
  if (fill_undetected_groups) {
    bouts_summary <- grouping_keys |>
    dplyr::full_join(bouts_summary, by = union(original_groups, "rfid")) |>
    tidyr::complete(
      fill = list(duration_minutes = 0)
    ) |>
    dplyr::select(-run_id)
  }

  return(bouts_summary)
}
