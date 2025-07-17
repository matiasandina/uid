#' Example UID Sample Data
#'
#' This dataset contains 24 hours of simulated temperature and zone data from
#' three RFID-tagged animals. Temperatures include noise and realistic artifacts
#' such as dropped and spurious values. Zone entries are randomly generated.
#'
#' @format A data frame with N rows and 6 columns:
#' \describe{
#'   \item{datetime}{Timestamp in POSIXct format}
#'   \item{rfid}{Unique identifier for each subject}
#'   \item{zone}{Integer between 1 and 8}
#'   \item{session_name}{Session identifier (character)}
#'   \item{temperature}{Body temperature with artifacts}
#'   \item{matrix_name}{Group label}
#' }
#' @source Simulated data for testing \pkg{uid} functionality.
"uid_sample_data"
