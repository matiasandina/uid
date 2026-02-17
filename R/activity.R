# TODO: this fails if we are not using an 8 coil matrix
.zone_coords <- tibble::tibble(
  zone = 1:8,
  x = c(0, 1, 2, 3, 3, 2, 1, 0) * 3.625,
  y = c(0, 0, 0, 0, 1, 1, 1, 1) * 3.16
)

# we will use this to map the movements of mice with pre-computed distances
.transition_distances <- expand.grid(
  from = 1:8,
  to = 1:8
) |>
  dplyr::left_join(.zone_coords, by = c("from" = "zone")) |>
  dplyr::rename(x1 = x, y1 = y) |>
  dplyr::left_join(.zone_coords, by = c("to" = "zone")) |>
  dplyr::rename(x2 = x, y2 = y) |>
  dplyr::mutate(activity_index = sqrt((x2 - x1)^2 + (y2 - y1)^2)) |>
  dplyr::select(from, to, activity_index)

.min_nonzero_transition_distance <- min(
  .transition_distances$activity_index[.transition_distances$activity_index > 0]
)

# Apply minute-level flicker correction to zone stream before activity quantification.
# This is intentionally internal and controlled via clean_raw_uid/process_all_uid_files.
.flicker_correct_zone_stream <- function(
    df,
    n = 1,
    precision = "minute",
    dominant_two_thr = 0.65,
    alt_rate_thr = 0.40,
    contiguous_thr = 0.65
) {
  if (nrow(df) == 0) {
    return(df |>
      dplyr::mutate(
        zone_corrected = zone,
        .flicker_corrected = FALSE
      ))
  }

  events <- df |>
    dplyr::arrange(session_name, rfid, matrix_name, datetime) |>
    dplyr::group_by(session_name, rfid, matrix_name) |>
    dplyr::mutate(
      prev_zone = dplyr::lag(zone),
      prev2_zone = dplyr::lag(zone, 2),
      alt_abab = zone == prev2_zone & zone != prev_zone,
      common_dt = clock::date_floor(datetime, n = n, precision = precision)
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      .transition_distances |>
        dplyr::rename(prev_zone = from, zone = to, transition_distance = activity_index),
      by = c("prev_zone", "zone")
    ) |>
    dplyr::mutate(
      changed = zone != prev_zone,
      contiguous_change = changed &
        !is.na(transition_distance) &
        abs(transition_distance - .min_nonzero_transition_distance) < 1e-8
    )

  minute_stats <- events |>
    dplyr::group_by(session_name, rfid, matrix_name, common_dt) |>
    dplyr::summarise(
      dominant_two_frac = {
        zone_counts <- table(zone)
        if (length(zone_counts) == 0) {
          NA_real_
        } else {
          sum(sort(zone_counts, decreasing = TRUE)[1:min(2, length(zone_counts))]) / sum(zone_counts)
        }
      },
      alt_rate = mean(alt_abab, na.rm = TRUE),
      contiguous_change_frac = mean(contiguous_change, na.rm = TRUE),
      mode_zone = {
        zc <- table(zone)
        if (length(zc) == 0) NA_integer_ else as.integer(names(zc)[which.max(zc)])
      },
      .groups = "drop"
    ) |>
    dplyr::mutate(
      .flicker_corrected = dominant_two_frac >= dominant_two_thr &
        alt_rate >= alt_rate_thr &
        contiguous_change_frac >= contiguous_thr
    )

  events |>
    dplyr::left_join(
      minute_stats |>
        dplyr::select(session_name, rfid, matrix_name, common_dt, mode_zone, .flicker_corrected),
      by = c("session_name", "rfid", "matrix_name", "common_dt")
    ) |>
    dplyr::mutate(
      .flicker_corrected = dplyr::coalesce(.flicker_corrected, FALSE),
      zone_corrected = dplyr::if_else(.flicker_corrected, mode_zone, zone)
    ) |>
    dplyr::select(-mode_zone, -transition_distance, -changed, -contiguous_change, -alt_abab, -prev2_zone, -prev_zone)
}


#' Calculate Activity Index Based on Zone Transitions
#'
#' Computes theoretical distance traveled between sequential zone visits
#' using UID Mouse Matrix coordinates. Adds a new column `activity_index`
#' to the data frame.
#'
#' @param df A data frame with columns `rfid`, `datetime`, and `zone`
#'
#' @return The input data frame with an added `activity_index` column (in inches).
#' @export
calculate_activity <- function(df) {
  df |>
    dplyr::arrange(rfid, datetime) |>
    dplyr::group_by(rfid) |>
    dplyr::mutate(prev_zone = dplyr::lag(zone)) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      .transition_distances,
      by = c("prev_zone" = "from", "zone" = "to")
    )
}


#' Downsample activity index data to fixed intervals
#'
#' @param df A cleaned dataframe with `datetime`, `rfid`, `session_name`, `matrix_name`, and `activity_index`.
#' @param n Number of time units per bin (default = 1).
#' @param precision Time unit for binning (e.g., "minute").
#' @return Downsampled data frame with activity index summarized by total distance per bin and a `.flicker_corrected` logical flag indicating whether flicker correction was applied in each bin. Missing data for time bins will be filled with `NA`.
#' @seealso [calculate_activity()]
#' @export
downsample_activity <- function(df, n = 1, precision = "minute") {
  if (!".flicker_corrected" %in% names(df)) {
    df <- dplyr::mutate(df, .flicker_corrected = FALSE)
  }

  df |>
    dplyr::mutate(
      common_dt = clock::date_floor(datetime, n = n, precision = precision)
    ) |>
    dplyr::group_by(session_name, rfid, common_dt, matrix_name) |>
    dplyr::summarise(
      activity_index = dplyr::if_else(
        all(is.na(activity_index)),
        NA_real_,
        sum(activity_index, na.rm = TRUE)
      ),
      .flicker_corrected = any(.flicker_corrected, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::complete(
      tidyr::nesting(session_name, rfid, matrix_name),
      common_dt,
      fill = list(activity_index = NA, .flicker_corrected = FALSE)
    )
}
