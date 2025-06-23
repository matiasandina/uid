#' Read and clean raw UID CSV file
#'
#' Reads a UID-formatted CSV, cleans column names, and parses datetime.
#'
#' @param filepath Path to the raw CSV file.
#' @param dt_format Sting defining the format for the time `string` to `datetime` conversion (e.g,. default is `"%Y/%m/%d %H:%M:%S"`)
#' @return A cleaned data frame with a `datetime` column.
#' @export
read_raw_uid_csv <- function(filepath, dt_format = "%Y/%m/%d %H:%M:%S") {
  df <- readr::read_csv(filepath) |>
    janitor::clean_names()

  required_cols <- c(
    "date", "rfid", "temperature",
    "session_name", "matrix_name", "zone"
  )

  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(
      "Missing required column(s): ",
      paste(missing, collapse = ", ")
    )
  }

  df <- dplyr::mutate(
    df,
    datetime = lubridate::as_datetime(date, format = dt_format)
  )

  if (any(is.na(df$datetime))) {
    stop("Failed to parse datetime with provided format")
  }

  return(df)
}


#' Flag temperature outliers based on rolling difference threshold
#'
#' @param df A dataframe with `datetime`, `rfid`, and `temperature`.
#' @param threshold Maximum allowed temperature jump (default = 1).
#' @return Data frame with added `temp_diff` and `outlier_global` columns.
#' @export
flag_temperature_outliers <- function(df, threshold = 1) {
  df |>
    dplyr::group_by(rfid) |>
    dplyr::arrange(datetime) |>
    dplyr::mutate(
      temp_diff = abs(temperature - dplyr::lag(temperature)),
      outlier_global = temp_diff > threshold
    ) |>
    dplyr::ungroup()
}


#' Downsample temperature data to fixed intervals
#'
#' @param df A cleaned dataframe with `datetime`, `rfid`, `session_name`, `matrix_name`, and `temperature`.
#' @param n Number of time units per bin (default = 1).
#' @param precision Time unit for binning (e.g., "minute").
#' @return Downsampled data frame with temperature summarized by median. Missing data for time bins will be filled with `NA`.
#' @export
downsample_temperature <- function(df, n = 1, precision = "minute") {
  df |>
    dplyr::mutate(
      common_dt = clock::date_floor(datetime, n = n, precision = precision)
    ) |>
    dplyr::group_by(session_name, rfid, common_dt, matrix_name) |>
    dplyr::summarise(
      temperature = stats::median(temperature, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::complete(
      tidyr::nesting(session_name, rfid, matrix_name),
      common_dt,
      fill = list(temperature = NA)
    )
}

#' Plot and save outlier diagnostic
#'
#' Creates a line plot of temperature over time, highlighting outliers in red,
#' and writes it to `output_dir/clean_plots/<basename>_outliers.png`.
#'
#' @param df_flagged Data frame returned by `flag_outliers()`, must include
#'   columns `datetime`, `temperature`, `rfid`, and `outlier`.
#' @param output_dir Path to the root output directory for plots.
#' @param filepath Original data file path used to derive the base filename.
#' @return Invisibly returns the ggplot object.
#' @export
plot_outliers <- function(df_flagged, output_dir, filepath) {
  # ensure directory exists
  fs::dir_create(file.path(output_dir, "clean_plots"))
  filename_out <- file.path(
    output_dir,
    "clean_plots",
    paste0(tools::file_path_sans_ext(basename(filepath)), "_outliers.png")
  )

  p <- ggplot2::ggplot(df_flagged, ggplot2::aes(datetime, temperature)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(
      data = df_flagged |> dplyr::filter(outlier_global),
      ggplot2::aes(datetime, temperature),
      color = "red"
    ) +
    ggplot2::facet_wrap(~rfid)

  ggplot2::ggsave(filename_out, plot = p, width = 12, height = 8)
}

#' Plot and save downsampled temperature trace
#'
#' Generates a line plot of downsampled `temperature` over `common_dt`,
#' faceted by `rfid`, and writes it to
#' `output_dir/clean_plots/<basename>_downsampled.png`.
#'
#' @param df_down Data frame returned by `downsample()`. Must contain
#'   `common_dt`, `temperature`, and `rfid`.
#' @param output_dir Directory where `clean_plots/` will be created (if needed).
#' @param filepath Original CSV path—used to derive the base filename for saving.
#' @return Invisibly returns the ggplot object.
#' @export
plot_downsampled_temperature <- function(df_down, output_dir, filepath) {
  # ensure directory exists
  fs::dir_create(file.path(output_dir, "clean_plots"))
  filename_out <- file.path(
    output_dir,
    "clean_plots",
    paste0(tools::file_path_sans_ext(basename(filepath)), "_downsampled.png")
  )

  p <- ggplot2::ggplot(df_down, ggplot2::aes(common_dt, temperature)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~rfid)

  ggplot2::ggsave(filename_out, plot = p, width = 12, height = 8)
}

#' Wrapper to clean UID temperature CSV: read, flag outliers, downsample, and plot
#'
#' @param filepath Path to a raw UID CSV.
#' @param n Downsampling interval size (default = 1).
#' @param precision Time unit for downsampling (default = "minute").
#' @param outlier_threshold_celsius Temperature difference threshold to flag outliers.
#' @param output_dir Directory to save diagnostic plots.
#' @param plot boolean to indicate whether to plot the process of outlier removal and downsampling
#' @return Cleaned, downsampled data frame.
#' @export
clean_raw_uid <- function(
  filepath,
  n = 1,
  precision = "minute",
  outlier_threshold_celsius = 1,
  output_dir = "temperature/data",
  plot = TRUE
) {
  df_flagged <- read_raw_uid_csv(filepath) |>
    flag_temperature_outliers(
      threshold = outlier_threshold_celsius
    )
  df_filtered <- df_flagged |>
    dplyr::filter(!outlier_global) |>
    dplyr::select(-temp_diff, -outlier_global)

  df_downsampled_temperature <- downsample_temperature(
    df_filtered,
    n = n,
    precision = precision
  )

  df_downsampled_activity <- calculate_activity(df_filtered) |>
    downsample_activity()

  if (isTRUE(plot)) {
    plot_outliers(
      df_flagged,
      output_dir,
      filepath
    )
    plot_downsampled_temperature(
      df_downsampled_temperature,
      output_dir,
      filepath
    )
    #TODO: plot downsampled activity
    # no point in keeping the jittery original one
  }

  return(list(
    temperature = df_downsampled_temperature,
    activity = df_downsampled_activity
  ))
}

#' Extract Base Name from UID CSV Filename
#'
#' Internal helper that removes `_x_of_y.CSV` suffix from a UID file name,
#' returning the shared base name across split parts of a session.
#'
#' @param filename A character vector of file paths or names.
#' @return A character vector with the base names.
#' @keywords internal
extract_base_name <- function(filename) {
  sub("_\\d+_of_\\d+\\.CSV$", "", basename(filename))
}


#' Check for Existing Output Files and Prompt Overwrite Options
#'
#' Given a set of group names and an output directory, this function checks
#' if the expected output files already exist. If so, prompts the user to
#' decide whether to overwrite, skip, or abort.
#'
#' @param output_dir Path to the directory where output files are written.
#' @param group_names A character vector of file group base names.
#'
#' @return A character vector of group names that should be processed.
#'         This excludes any that the user chose to skip.
#' @export
check_overwrite_permissions <- function(output_dir, group_names) {
  existing <- purrr::map_chr(
    group_names,
    ~ fs::path(output_dir, paste0(.x, ".csv"))
  ) |>
    purrr::keep(fs::file_exists)

  if (length(existing) == 0) return(group_names) # nothing exists, proceed as is

  cli::cli_alert_warning("The following output file(s) already exist:")
  cli::cli_alert_info("{existing}", wrap = TRUE)

  user_response <- utils::menu(
    choices = c("Overwrite all", "Skip existing", "Abort"),
    title = "How should we handle existing output files?"
  )

  if (user_response == 1) {
    return(group_names) # overwrite all
  } else if (user_response == 2) {
    return(setdiff(group_names, fs::path_ext_remove(fs::path_file(existing))))
  } else {
    cli::cli_alert_danger("Aborting by user request.")
  }
}


#' Finds raw exported files ending with CSV
#'
#'
#' @param raw_export_dir Path to the directory where raw data is located.
#' @keywords internal
#'

find_raw_export_files <- function(raw_export_dir) {
  temperature_files <- list.files(
    raw_export_dir,
    pattern = "CSV$",
    full.names = T
  )

  cli::cli_alert_success("Found {length(temperature_files)} raw files:")
  cli::cli_alert_info("{fs::path(temperature_files)}", wrap = TRUE)
  return(temperature_files)
}
