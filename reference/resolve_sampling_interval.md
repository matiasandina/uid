# Resolve sampling interval specification

Internal utility to resolve the sampling interval for temperature data.
Accepts either:

- a single numeric value (assumed to be in minutes)

- a function that returns a numeric value when applied to the input data

## Usage

``` r
resolve_sampling_interval(df, sampling_interval)
```

## Arguments

- df:

  A data frame used as input to the function if `sampling_interval` is a
  function.

- sampling_interval:

  A numeric value (in minutes) or a function taking `df` and returning a
  numeric value.

## Value

A numeric sampling interval in minutes.

## Details

Used internally by
[`quantify_temp_bouts()`](https://matiasandina.github.io/uid/reference/quantify_temp_bouts.md)
to support flexible user input.
