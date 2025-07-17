#' Example UID Sample Data
#'
#' This dataset contains 24 hours of simulated temperature and zone data from
#' three RFID-tagged animals. Temperatures include noise and realistic artifacts
#' such as dropped and spurious values. Zone entries are randomly generated.
#'
#' @format A data frame with ~10,000 rows and 5 columns:
#' \describe{
#'   \item{Date}{Timestamp in character format (YYYY/MM/DD HH:MM:SS)}
#'   \item{RFID}{Unique identifier for each animal (e.g., A1B2C3D4)}
#'   \item{Zone}{Integer between 1 and 8 indicating spatial zone}
#'   \item{Session Name}{Character label for the session (constant)}
#'   \item{Temperature}{Body temperature in °C, with missing and spurious values}
#'   \item{Matrix Name}{String identifier (e.g., MM1, MM2, ...) for each animal}
#' }
#' @source Simulated data for testing \pkg{uid} functionality.
"uid_sample_data"
