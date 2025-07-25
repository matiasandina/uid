% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uid_sample_data.R
\docType{data}
\name{uid_sample_data}
\alias{uid_sample_data}
\title{Example UID Sample Data}
\format{
A data frame with N rows and 6 columns:
\describe{
\item{datetime}{Timestamp in POSIXct format}
\item{rfid}{Unique identifier for each subject}
\item{zone}{Integer between 1 and 8}
\item{session_name}{Session identifier (character)}
\item{temperature}{Body temperature with artifacts}
\item{matrix_name}{Group label}
}
}
\source{
Simulated data for testing \pkg{uid} functionality.
}
\usage{
uid_sample_data
}
\description{
This dataset contains 24 hours of simulated temperature and zone data from
three RFID-tagged animals. Temperatures include noise and realistic artifacts
such as dropped and spurious values. Zone entries are randomly generated.
}
\keyword{datasets}
