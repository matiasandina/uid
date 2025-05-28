#' Process All UID CSV Files in Batch
#'
#' Finds, groups, and processes all raw UID export CSVs in a directory,
#' then saves cleaned and downsampled outputs to a target directory.
#'
#' @param raw_export_dir Directory containing raw UID .CSV files.
#' @param output_dir Directory to write cleaned files to.
#' @param n Bin size for downsampling (default = 1).
#' @param precision Time unit for binning (default = \"minute\").
#' @param outlier_threshold Temperature delta threshold for outlier detection.
#' @return Invisibly returns a list of output file paths.
#' @export
process_all_uid_files <- function(
  raw_export_dir = "temperature/raw_data",
  output_dir = "temperature/data",
  n = 1,
  precision = "minute",
  outlier_threshold = 1
) {
  temperature_files <- find_raw_export_files(raw_export_dir = raw_export_dir)
  base_names <- sapply(temperature_files, extract_base_name)
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
        output_dir = output_dir
      )

      out <- dplyr::bind_rows(clean_list)

      output_filename <- fs::path(output_dir, paste0(group_name, ".csv"))
      readr::write_csv(out, output_filename)

      cli::cli_alert_success("Wrote group: {group_name} to {output_filename}")
      return(output_filename)
    }
  )

  invisible(output_paths)
}
