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
#' @return Downsampled data frame with activity index summarized by median. Missing data for time bins will be filled with `NA`.
#' @seealso [calculate_activity()]
#' @export
downsample_activity <- function(df, n = 1, precision = "minute") {
  df |>
    dplyr::mutate(
      common_dt = clock::date_floor(datetime, n = n, precision = precision)
    ) |>
    dplyr::group_by(session_name, rfid, common_dt, matrix_name) |>
    dplyr::summarise(
      activity_index = stats::median(activity_index, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::complete(
      tidyr::nesting(session_name, rfid, matrix_name),
      common_dt,
      fill = list(activity_index = NA)
    )
}
