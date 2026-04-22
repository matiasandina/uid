#' Process All UID CSV Files in Batch
#'
#' Finds, groups, and processes all raw UID export CSVs in a directory,
#' then saves cleaned and downsampled outputs to a target directory.
#'
#' Each cleaned session is expected to contain a single `rfid` to `matrix_name`
#' mapping. If the same `rfid` appears on more than one matrix within a session,
#' the cleaning step prints a summary table and requires an interactive choice.
#' Low-percentage stray detections can be removed after confirmation; more
#' substantial duplicate detections abort processing and should be reviewed
#' manually.
#'
#' @param raw_export_dir Directory containing raw UID .CSV files.
#' @param output_dir Directory to write cleaned files to.
#' @param n Bin size for downsampling (default = 1).
#' @param precision Time unit for binning (default = \"minute\").
#' @param outlier_threshold Temperature delta threshold for outlier detection.
#' @param flicker_correction Whether to apply activity flicker correction before quantification.
#' @param flicker_dominant_two_thr Threshold for dominant two-zone occupancy fraction.
#' @param flicker_alt_rate_thr Threshold for ABAB alternation rate within minute bins.
#' @param flicker_contiguous_thr Threshold for contiguous-transition fraction within minute bins.
#' @return Invisibly returns a list of output file paths.
#' @export
process_all_uid_files <- function(
  raw_export_dir = "temperature/raw_data",
  output_dir = "temperature/data",
  n = 1,
  precision = "minute",
  outlier_threshold = 1,
  flicker_correction = TRUE,
  flicker_dominant_two_thr = 0.65,
  flicker_alt_rate_thr = 0.40,
  flicker_contiguous_thr = 0.65
) {
  temperature_files <- find_raw_export_files(raw_export_dir = raw_export_dir)
  base_names <- vapply(temperature_files, extract_base_name, FUN.VALUE = character(1))
  file_groups <- split(temperature_files, base_names)

  cli::cli_alert_info(
    "Files will be processed using these {length(file_groups)} group(s): {names(file_groups)}",
    wrap = TRUE
  )

  approved_group_names <- check_overwrite_permissions(
    output_dir,
    names(file_groups)
  )
  approved_file_groups <- file_groups[approved_group_names]

  output_paths <- purrr::map2(
    .x = approved_file_groups,
    .y = names(approved_file_groups),
    .f = function(files, group_name) {
      cli::cli_alert_info("Reading file(s) in group: {group_name}")
      clean_list <- purrr::map(
        files,
        clean_raw_uid,
        n = n,
        precision = precision,
        outlier_threshold_celsius = outlier_threshold,
        output_dir = output_dir,
        flicker_correction = flicker_correction,
        flicker_dominant_two_thr = flicker_dominant_two_thr,
        flicker_alt_rate_thr = flicker_alt_rate_thr,
        flicker_contiguous_thr = flicker_contiguous_thr
      )
      # TODO: clean_raw_uid returns a list of temperature and activity
      # fix this to pluck first temperatures and activity separately?
      #out <- dplyr::bind_rows(clean_list)

      out <- clean_list |>
        # change from list(list(activity=..., temperature=...), ...)
        # to list(activity = list(), temperature = list())
        purrr::transpose() |>
        purrr::map(dplyr::bind_rows) |>
        purrr::list_rbind(names_to = "variable") |>
        # this list_rbind will give us two columns
        # we want to output one column `value`
        dplyr::mutate(value = dplyr::coalesce(temperature, activity_index)) |>
        dplyr::select(-temperature, -activity_index)

      output_filename <- fs::path(output_dir, paste0(group_name, ".csv"))
      readr::write_csv(out, output_filename)

      cli::cli_alert_success("Wrote group: {group_name} to {output_filename}")
      return(output_filename)
    }
  )

  invisible(output_paths)
}

#' Helper to pluck the temperature and activity datasets separately
#' we can consider including this instead of the bind_rows(.id = "variable")
#' the con of this is handling multiple files
#' @keywords internal
pluck_bind_temp_activity <- function(clean_list, nm) {
  purrr::pluck(clean_list, nm) |>
    dplyr::bind_rows()
}
