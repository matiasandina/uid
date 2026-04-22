# Downsample activity index data to fixed intervals

Downsample activity index data to fixed intervals

## Usage

``` r
downsample_activity(df, n = 1, precision = "minute")
```

## Arguments

- df:

  A cleaned dataframe with `datetime`, `rfid`, `session_name`,
  `matrix_name`, and `activity_index`.

- n:

  Number of time units per bin (default = 1).

- precision:

  Time unit for binning (e.g., "minute").

## Value

Downsampled data frame with activity index summarized by total distance
per bin and a `.flicker_corrected` logical flag indicating whether
flicker correction was applied in each bin. Missing data for time bins
will be filled with `NA`.

## See also

[`calculate_activity()`](https://matiasandina.github.io/uid/reference/calculate_activity.md)
