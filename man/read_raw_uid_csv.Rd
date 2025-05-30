% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_temperature.R
\name{read_raw_uid_csv}
\alias{read_raw_uid_csv}
\title{Read and clean raw UID CSV file}
\usage{
read_raw_uid_csv(filepath, dt_format = "\%Y/\%m/\%d \%H:\%M:\%S")
}
\arguments{
\item{filepath}{Path to the raw CSV file.}

\item{dt_format}{Sting defining the format for the time \code{string} to \code{datetime} conversion (e.g,. default is \code{"\%Y/\%m/\%d \%H:\%M:\%S"})}
}
\value{
A cleaned data frame with a \code{datetime} column.
}
\description{
Reads a UID-formatted CSV, cleans column names, and parses datetime.
}
