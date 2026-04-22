# Downsample temperature data to fixed intervals

Downsample temperature data to fixed intervals

## Usage

``` r
downsample_temperature(df, n = 1, precision = "minute")
```

## Arguments

- df:

  A cleaned dataframe with `datetime`, `rfid`, `session_name`,
  `matrix_name`, and `temperature`.

- n:

  Number of time units per bin (default = 1).

- precision:

  Time unit for binning (e.g., "minute").

## Value

Downsampled data frame with temperature summarized by median. Missing
data for time bins will be filled with `NA`.
