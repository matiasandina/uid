# Interpolate Short Gaps in UID Time Series Data

Fills small gaps (NAs) in downsampled UID data using linear
interpolation, only for gaps that are no longer than a specified number
of bins. Assumes a regular time step (e.g., 1-minute bins) and operates
within grouping variables like `rfid` and `session_name`.

## Usage

``` r
interpolate_gaps(
  df,
  max_gap = 10,
  target_cols = c("temperature", "activity_index"),
  add_flag = FALSE
)
```

## Arguments

- df:

  A downsampled data frame, ideally grouped by one or more identifiers.

- max_gap:

  Maximum number of consecutive missing bins to interpolate (default =
  10).

- target_cols:

  Character vector of column names to interpolate.

- add_flag:

  Logical. If TRUE, adds a `.interpolated` column (default = FALSE).

## Value

A data frame with interpolated values for short gaps.
