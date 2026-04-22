# Compute bout durations

Compute bout durations

## Usage

``` r
compute_bout_durations(
  df,
  sampling_interval,
  duration_mode = c("strict", "inclusive")
)
```

## Arguments

- df:

  A grouped df with `start` and `end` timestamps per bout.

- sampling_interval:

  Numeric value representing sampling interval in minutes.

- duration_mode:

  Either "strict" (end - start) or "inclusive" (end - start +
  sampling_interval).

## Value

A data frame with a `duration_minutes` column.
