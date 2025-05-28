
<!-- README.md is generated from README.Rmd. Please edit that file -->

# uid

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of uid is to streamline the processing of temperature data
exported from UID devices. It provides functions for cleaning, outlier
detection, downsampling, and diagnostic plotting, assuming a structured
directory layout for raw and processed data.

## Installation

You can install the development version of uid like so:

``` r
devtools::install_github("matiasandina/uid")
#> Using github PAT from envvar GITHUB_PAT
#> Downloading GitHub repo matiasandina/uid@HEAD
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/tmp/RtmpnFBEMs/remotes12029e4884825e/matiasandina-uid-86e129e/DESCRIPTION’ ... OK
#> * preparing ‘uid’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * creating default NAMESPACE file
#> * building ‘uid_0.0.0.9000.tar.gz’
#> Installing package into '/tmp/RtmppFG0Cw/temp_libpath11f5c6246810bc'
#> (as 'lib' is unspecified)
```

## Directory Structure

The package assumes the following directory layout:

    temperature/
    ├── raw_data/   # Place raw UID .CSV files here
    └── data/       # Cleaned, downsampled outputs will be written here

## Example

``` r
library(uid)

# Process all raw UID CSVs from a directory
process_all_uid_files(
  raw_export_dir = "temperature/raw_data",
  output_dir = "temperature/data"
)
```

This will:

- Group files by shared base name (e.g. handling \_1_of_n format)
- Remove temperature outliers
- Downsample by 1-minute intervals (by default)
- Save cleaned data and diagnostic plots

------------------------------------------------------------------------

You can also use lower-level functions for more custom workflows:

``` r
df <- read_raw_uid_csv("path/to/file.csv")
df_flagged <- flag_temperature_outliers(df, threshold = 1)
df_clean <- df_flagged |> dplyr::filter(!outlier_global)
df_downsampled <- downsample_temperature(df_clean)
```

## Roadmap

Planned features include:

- Activity calculation from raw signal
- Interpolation of gaps
- Duration analysis for threshold crossing events
