% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_temperature.R
\name{clean_raw_uid}
\alias{clean_raw_uid}
\title{Wrapper to clean UID temperature CSV: read, flag outliers, downsample, and plot}
\usage{
clean_raw_uid(
  filepath,
  n = 1,
  precision = "minute",
  outlier_threshold_celsius = 1,
  output_dir = "temperature/data",
  plot = TRUE
)
}
\arguments{
\item{filepath}{Path to a raw UID CSV.}

\item{n}{Downsampling interval size (default = 1) applied to both temperature
and activity data.}

\item{precision}{Time unit for downsampling (default = "minute").}

\item{outlier_threshold_celsius}{Temperature difference threshold to flag outliers.}

\item{output_dir}{Directory to save diagnostic plots.}

\item{plot}{boolean to indicate whether to plot the process of outlier removal and downsampling}
}
\value{
Cleaned, downsampled data frame.
}
\description{
Wrapper to clean UID temperature CSV: read, flag outliers, downsample, and plot
}
