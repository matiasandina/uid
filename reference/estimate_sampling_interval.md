# Estimate Sampling Interval from Grouped Data

Determines the time difference in minutes between consecutive
`common_dt` observations for each group in `df`. All groups must have a
single unique interval and share the same value.

## Usage

``` r
estimate_sampling_interval(df)
```

## Arguments

- df:

  A grouped data frame containing a `common_dt` column.

## Value

Numeric sampling interval in minutes.
